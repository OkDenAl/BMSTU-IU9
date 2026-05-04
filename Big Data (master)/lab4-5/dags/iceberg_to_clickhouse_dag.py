from datetime import datetime, timedelta
from airflow import DAG
from airflow.operators.python import PythonOperator
from airflow.exceptions import AirflowSkipException
import os

ICEBERG_CATALOG_CONFIG = {
    "type": "sql",
    "uri": "postgresql+psycopg2://nessie:nessie@postgres:5432/nessie",
    "warehouse": "s3://warehouse/",
    "s3.endpoint": "http://minio:9000",
    "s3.access-key-id": "admin",
    "s3.secret-access-key": "password123",
    "s3.region": "us-east-1",
    "py-io-impl": "pyiceberg.io.fsspec.FsspecFileIO",
}

CLICKHOUSE_CONFIG = {
    "host": "clickhouse",
    "port": 8123,
    "username": "default",
    "password": "clickhouse",
}

ICEBERG_TABLE = "yellow_taxi.trips"


def get_catalog():
    os.environ["AWS_ACCESS_KEY_ID"] = "admin"
    os.environ["AWS_SECRET_ACCESS_KEY"] = "password123"
    os.environ["AWS_REGION"] = "us-east-1"
    from pyiceberg.catalog import load_catalog
    return load_catalog("default", **ICEBERG_CATALOG_CONFIG)


def get_clickhouse():
    import clickhouse_connect
    return clickhouse_connect.get_client(**CLICKHOUSE_CONFIG)


def detect_new_snapshot(**context):
    catalog = get_catalog()
    tbl = catalog.load_table(ICEBERG_TABLE)
    current_snapshot_id = tbl.metadata.current_snapshot_id

    ch = get_clickhouse()
    result = ch.query(
        "SELECT snapshot_id FROM _processed_snapshots "
        "ORDER BY processed_at DESC LIMIT 1"
    )

    if result.result_rows and result.result_rows[0][0]:
        last_processed_id = result.result_rows[0][0]
    else:
        last_processed_id = 0

    print(f"Current Iceberg snapshot: {current_snapshot_id}")
    print(f"Last processed snapshot:  {last_processed_id}")

    all_snapshots = {s.snapshot_id: s for s in tbl.metadata.snapshots}
    print(f"Available snapshots: {list(all_snapshots.keys())}")

    if current_snapshot_id == last_processed_id:
        raise AirflowSkipException("No new snapshot")

    last_exists = last_processed_id in all_snapshots

    if last_exists:
        print("Incremental mode: will diff snapshots")
    else:
        print("Last processed snapshot not found in Iceberg — will do full sync")

    return {
        "current_snapshot_id": str(current_snapshot_id),
        "last_processed_id": str(last_processed_id),
        "last_exists_in_iceberg": last_exists,
    }


def incremental_load(**context):
    import pyarrow.compute as pc

    ti = context["ti"]
    detect_result = ti.xcom_pull(task_ids="detect_new_snapshot")
    print(f"DEBUG: detect_result = {detect_result}, type = {type(detect_result)}")


    current_snapshot_id = int(detect_result["current_snapshot_id"])
    last_processed_id = int(detect_result["last_processed_id"])
    last_exists = detect_result["last_exists_in_iceberg"]
    catalog = get_catalog()
    tbl = catalog.load_table(ICEBERG_TABLE)
    ch = get_clickhouse()

    ch_count = ch.query("SELECT count() FROM yellow_taxi_raw").result_rows[0][0]
    print(f"Rows currently in ClickHouse: {ch_count:,}")

    df_current = tbl.scan(snapshot_id=current_snapshot_id).to_arrow()
    iceberg_count = len(df_current)
    print(f"Rows in Iceberg (current snapshot): {iceberg_count:,}")

    if last_exists and last_processed_id != 0:
        df_previous = tbl.scan(snapshot_id=last_processed_id).to_arrow()
        rows_before = len(df_previous)
        rows_new = iceberg_count - rows_before
        print(f"Rows in previous snapshot: {rows_before:,}")
        print(f"New rows (by snapshot diff): {rows_new:,}")

        if rows_new <= 0:
            print("No new rows by snapshot diff")
            if ch_count < iceberg_count:
                rows_new = iceberg_count - ch_count
                rows_before = ch_count
                print(f"But ClickHouse has fewer rows! Loading {rows_new:,} rows")
            else:
                return {"rows_added": 0, "current_snapshot_id": str(current_snapshot_id)}

        df_new = df_current.slice(rows_before)
    else:
        rows_new = iceberg_count - ch_count
        print(f"Full sync mode. Missing rows: {rows_new:,}")

        if rows_new <= 0:
            print("ClickHouse already has all data")
            return {"rows_added": 0, "current_snapshot_id": str(current_snapshot_id)}

        df_new = df_current.slice(ch_count)

    print(f"Loading {len(df_new):,} rows...")

    if "Airport_fee" in df_new.schema.names:
        if "airport_fee" in df_new.schema.names:
            col1 = df_new.column("airport_fee")
            col2 = df_new.column("Airport_fee")
            merged = pc.if_else(pc.is_null(col1), col2, col1)
            df_new = df_new.drop(["airport_fee", "Airport_fee"])
            df_new = df_new.append_column("airport_fee", merged)
        else:
            names = ["airport_fee" if n == "Airport_fee" else n for n in df_new.schema.names]
            df_new = df_new.rename_columns(names)

    ch_columns = [
        "VendorID", "tpep_pickup_datetime", "tpep_dropoff_datetime",
        "passenger_count", "trip_distance", "RatecodeID", "store_and_fwd_flag",
        "PULocationID", "DOLocationID", "payment_type", "fare_amount",
        "extra", "mta_tax", "tip_amount", "tolls_amount",
        "improvement_surcharge", "total_amount", "congestion_surcharge", "airport_fee"
    ]
    available = [col for col in ch_columns if col in df_new.schema.names]
    df_new = df_new.select(available)

    ch.insert_df("yellow_taxi_raw", df_new.to_pandas())
    print(f"Inserted {len(df_new):,} rows")

    return {"rows_added": len(df_new), "current_snapshot_id": str(current_snapshot_id)}


def rebuild_marts(**context):
    ch = get_clickhouse()

    print("Rebuilding mart_avg_price_per_km...")
    ch.command("TRUNCATE TABLE mart_avg_price_per_km")
    ch.command("""
    INSERT INTO mart_avg_price_per_km
    SELECT toDate(tpep_pickup_datetime) AS trip_date,
           avg(total_amount / nullIf(trip_distance, 0)) AS avg_price_per_km
    FROM yellow_taxi_raw
    WHERE trip_distance > 0 AND total_amount > 0
      AND tpep_pickup_datetime >= '2023-01-01'
      AND tpep_pickup_datetime < '2024-01-01'
    GROUP BY trip_date ORDER BY trip_date
    """)

    print("Rebuilding mart_top_zones_by_weekday...")
    ch.command("TRUNCATE TABLE mart_top_zones_by_weekday")
    ch.command("""
    INSERT INTO mart_top_zones_by_weekday
    SELECT day_of_week, day_name, PULocationID, total_revenue, zone_rank
    FROM (
        SELECT day_of_week, day_name, PULocationID, total_revenue,
            row_number() OVER (PARTITION BY day_of_week ORDER BY total_revenue DESC) AS zone_rank
        FROM (
            SELECT toDayOfWeek(tpep_pickup_datetime) AS day_of_week,
                CASE toDayOfWeek(tpep_pickup_datetime)
                    WHEN 1 THEN 'Monday' WHEN 2 THEN 'Tuesday'
                    WHEN 3 THEN 'Wednesday' WHEN 4 THEN 'Thursday'
                    WHEN 5 THEN 'Friday' WHEN 6 THEN 'Saturday'
                    WHEN 7 THEN 'Sunday' END AS day_name,
                PULocationID, sum(total_amount) AS total_revenue
            FROM yellow_taxi_raw
            WHERE tpep_pickup_datetime >= '2023-01-01'
              AND tpep_pickup_datetime < '2024-01-01'
            GROUP BY day_of_week, day_name, PULocationID
        )
    ) WHERE zone_rank <= 10
    """)

    print("Rebuilding mart_trip_duration_distribution...")
    ch.command("TRUNCATE TABLE mart_trip_duration_distribution")
    ch.command("""
    INSERT INTO mart_trip_duration_distribution
    SELECT duration_bucket, count() AS trip_count,
        medianExact(total_amount) AS median_fare,
        count() * 1.0 / (SELECT count() FROM yellow_taxi_raw
            WHERE tpep_dropoff_datetime > tpep_pickup_datetime
              AND tpep_pickup_datetime >= '2023-01-01'
              AND tpep_pickup_datetime < '2024-01-01') AS share_of_total
    FROM (
        SELECT total_amount,
            CASE
                WHEN dm BETWEEN 0 AND 5 THEN '1. 0-5 min'
                WHEN dm BETWEEN 6 AND 15 THEN '2. 5-15 min'
                WHEN dm BETWEEN 16 AND 30 THEN '3. 15-30 min'
                WHEN dm BETWEEN 31 AND 60 THEN '4. 30-60 min'
                ELSE '5. 60+ min'
            END AS duration_bucket
        FROM (
            SELECT total_amount,
                dateDiff('minute', tpep_pickup_datetime, tpep_dropoff_datetime) AS dm
            FROM yellow_taxi_raw
            WHERE tpep_dropoff_datetime > tpep_pickup_datetime
              AND tpep_pickup_datetime >= '2023-01-01'
              AND tpep_pickup_datetime < '2024-01-01'
        )
    ) GROUP BY duration_bucket ORDER BY duration_bucket
    """)
    print("All marts rebuilt")


def mark_snapshot_processed(**context):
    ti = context["ti"]
    load_result = ti.xcom_pull(task_ids="incremental_load")
    print(f"DEBUG: load_result = {load_result}")

    if load_result is None:
        catalog = get_catalog()
        tbl = catalog.load_table(ICEBERG_TABLE)
        current_snapshot_id = tbl.metadata.current_snapshot_id
        rows_added = 0
    else:
        current_snapshot_id = load_result["current_snapshot_id"]
        rows_added = load_result["rows_added"]

    ch = get_clickhouse()
    ch.command(
        f"INSERT INTO _processed_snapshots (snapshot_id, rows_added) "
        f"VALUES ({current_snapshot_id}, {rows_added})"
    )
    print(f"Snapshot {current_snapshot_id} marked as processed ({rows_added} rows)")


default_args = {"owner": "airflow", "retries": 1, "retry_delay": timedelta(minutes=2)}

with DAG(
    dag_id="iceberg_to_clickhouse",
    default_args=default_args,
    description="Incremental load from Iceberg to ClickHouse",
    schedule=timedelta(minutes=30),
    start_date=datetime(2024, 1, 1),
    catchup=False,
    tags=["iceberg", "clickhouse"],
) as dag:
    t1 = PythonOperator(task_id="detect_new_snapshot", python_callable=detect_new_snapshot)
    t2 = PythonOperator(task_id="incremental_load", python_callable=incremental_load)
    t3 = PythonOperator(task_id="rebuild_marts", python_callable=rebuild_marts)
    t4 = PythonOperator(task_id="mark_snapshot_processed", python_callable=mark_snapshot_processed)
    t1 >> t2 >> t3 >> t4
import math
from pyspark.sql import SparkSession
import re

spark = SparkSession.builder \
    .appName("LogsAnalysis") \
    .master("spark://spark-master:7077") \
    .getOrCreate()

sc = spark.sparkContext

rdd = sc.textFile("file:///data/data/access.log")


def parse(line):
    pattern = r'\[(\d{2}/\w{3}/\d{4}).*?"\w+ (.*?) HTTP.*?" (\d{3})'
    match = re.search(pattern, line)
    if match:
        return (match.group(2), match.group(3), match.group(1))
    return None


parsed = rdd.map(parse).filter(lambda x: x is not None)

top_urls = (parsed
            .map(lambda x: (x[0], 1))
            .reduceByKey(lambda a, b: a + b)
            .takeOrdered(10, key=lambda x: -x[1]))

top_url_set = set(url for url, _ in top_urls)

status_summary_rdd = (parsed
                      .filter(lambda x: x[0] in top_url_set)
                      .map(lambda x: ((x[0], x[1]), 1))
                      .reduceByKey(lambda a, b: a + b)
                      .map(lambda x: (x[0][0], (x[0][1], x[1])))
                      .groupByKey()
                      .mapValues(lambda vals: dict(sorted(vals, key=lambda x: x[0]))))

status_summary = dict(status_summary_rdd.collect())

url_date_counts = (parsed
                   .filter(lambda x: x[0] in top_url_set)
                   .map(lambda x: ((x[0], x[2]), 1))
                   .reduceByKey(lambda a, b: a + b)
                   .map(lambda x: (x[0][0], (x[0][1], x[1])))
                   .groupByKey())


def detect_anomalies(data):
    values = list(data)
    counts = [v[1] for v in values]
    if not counts:
        return {"low": [], "high": []}
    mean = sum(counts) / len(counts)
    std = math.sqrt(sum((x - mean) ** 2 for x in counts) / len(counts))
    anomalies = {"low": [], "high": []}
    for date, count in values:
        if count < mean - 2 * std:
            anomalies["low"].append((date, count))
        elif count > mean + 2 * std:
            anomalies["high"].append((date, count))
    return anomalies


anomalies = dict(url_date_counts.mapValues(detect_anomalies).collect())

print("\nТоп 10 URL")
for url, count in top_urls:
    print(f"{url}: {count} запросов")

print("\nРаспределение по кодам ответа:")
for url, _ in top_urls:
    total_requests = sum(status_summary.get(url, {}).values())
    print(f"URL: {url}")
    for status, cnt in status_summary.get(url, {}).items():
        pct = cnt / total_requests * 100
        print(f"\tСтатус {status}: {cnt} запросов ({pct:.2f}%)")

print("\nАномалии по датам:")
for url, _ in top_urls:
    if url in anomalies:
        print(f"URL: {url}")
        low = anomalies[url]["low"]
        high = anomalies[url]["high"]
        if low:
            print("\tАномально мало запросов:")
            for date, count in sorted(low):
                print(f"\t\t{date}: {count}")
        if high:
            print("\tАномально много запросов:")
            for date, count in sorted(high):
                print(f"\t\t{date}: {count}")
        if not low and not high:
            print("\tНет аномальных дней")

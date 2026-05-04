CREATE STREAM logs_stream (
    url STRING,
    status INT,
    timestamp_ms BIGINT
) WITH (
    KAFKA_TOPIC='logs',
    VALUE_FORMAT='JSON',
    TIMESTAMP='timestamp_ms'
);

CREATE STREAM top_urls AS
SELECT *
FROM logs_stream
WHERE url IN (
'/images/NASA-logosmall.gif',
'/images/KSC-logosmall.gif',
'/images/MOSAIC-logosmall.gif',
'/images/USA-logosmall.gif',
'/images/WORLD-logosmall.gif',
'/images/ksclogo-medium.gif',
'/ksc.html',
'/history/apollo/images/apollo-logo1.gif',
'/images/launch-logo.gif',
'/'
)
EMIT CHANGES;

CREATE TABLE requests_per_day AS
SELECT
    url,
    WINDOWSTART AS day,
    COUNT(*) AS requests
FROM top_urls
WINDOW TUMBLING (SIZE 1 DAY, RETENTION 200000 DAYS, GRACE PERIOD 100000 DAYS)
GROUP BY url
EMIT CHANGES;

CREATE TABLE status_stats AS
SELECT
    url,
    COUNT(*) AS total,
    SUM(CASE WHEN status >= 200 AND status < 300 THEN 1 ELSE 0 END) AS success,
    SUM(CASE WHEN status >= 300 THEN 1 ELSE 0 END) AS failed
FROM top_urls
GROUP BY url
EMIT CHANGES;
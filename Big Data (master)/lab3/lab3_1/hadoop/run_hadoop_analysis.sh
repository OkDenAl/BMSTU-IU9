#!/bin/bash

INPUT_FILE="/data/access.log"
OUTPUT_URL="/data/output_url_counts"
OUTPUT_STATUS_DATE="/data/output_status_date"

rm -rf $OUTPUT_URL $OUTPUT_STATUS_DATE

hadoop jar $HADOOP_HOME/share/hadoop/tools/lib/hadoop-streaming-*.jar \
    -D mapreduce.framework.name=local \
    -D mapreduce.job.reduces=2 \
    -files /hadoop/job1_mapper.py,/hadoop/job1_reducer.py \
    -mapper "python3 job1_mapper.py" \
    -reducer "python3 job1_reducer.py" \
    -input file://$INPUT_FILE \
    -output file://$OUTPUT_URL

cat $OUTPUT_URL/part-* | sort -k2 -nr | head -10 > /data/top_urls.txt

hadoop jar $HADOOP_HOME/share/hadoop/tools/lib/hadoop-streaming-*.jar \
    -D mapreduce.framework.name=local \
    -D mapreduce.job.reduces=2 \
    -files /hadoop/job2_mapper.py,/hadoop/job2_reducer.py,/data/top_urls.txt \
    -mapper "python3 job2_mapper.py" \
    -reducer "python3 job2_reducer.py" \
    -input file://$INPUT_FILE \
    -output file://$OUTPUT_STATUS_DATE

cat $OUTPUT_STATUS_DATE/part-* | grep "|status|" > /data/status_counts.txt
cat $OUTPUT_STATUS_DATE/part-* | grep "|date|" > /data/date_counts.txt

python3 /hadoop/analyze_results.py /data/top_urls.txt /data/status_counts.txt /data/date_counts.txt
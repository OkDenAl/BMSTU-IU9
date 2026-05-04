import re
import json
import time
from datetime import datetime, timedelta
from kafka import KafkaProducer

producer = KafkaProducer(
    bootstrap_servers='localhost:6502',
    value_serializer=lambda v: json.dumps(v).encode('utf-8')
)

log_pattern = re.compile(
    r'(\S+) - - \[(.*?)\] "(\S+) (\S+) (\S+)" (\d{3}) (\d+|-)'
)


def to_milliseconds(timestamp_str: str) -> int:
    date_part, offset_part = timestamp_str.rsplit(' ', 1)
    dt = datetime.strptime(date_part, '%d/%b/%Y:%H:%M:%S')
    sign = -1 if offset_part[0] == '-' else 1
    hours = int(offset_part[1:3])
    minutes = int(offset_part[4:6])
    offset_seconds = sign * (hours * 3600 + minutes * 60)
    dt_utc = dt - timedelta(seconds=offset_seconds)
    return int(dt_utc.timestamp() * 1000)


file_path = './data/access.log'
delay = 0.001

while True:
    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            m = log_pattern.match(line)
            if m:
                host, timestamp_str, method, url, protocol, status, size = m.groups()
                data = {
                    'url': url,
                    'status': int(status),
                    'timestamp_ms': to_milliseconds(timestamp_str)
                }
                producer.send('logs', value=data)
                print(f"Sent: {url} -> {status}  ts={data['timestamp_ms']}")
            else:
                print(f"Failed: {line[:80]}")
            time.sleep(delay)
    print("Restarting...")

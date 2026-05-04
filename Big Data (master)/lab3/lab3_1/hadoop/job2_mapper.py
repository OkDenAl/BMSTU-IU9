import sys
import re

TOP_URLS_FILE = "top_urls.txt"
top_urls = set()

try:
    with open(TOP_URLS_FILE) as f:
        for line in f:
            parts = line.strip().split()
            if len(parts) >= 1:
                top_urls.add(parts[0])
except FileNotFoundError:
    pass

pattern = re.compile(r'\[(\d{2}/\w{3}/\d{4}).*?"\w+ (.*?) HTTP.*?" (\d{3})')

for line in sys.stdin:
    match = pattern.search(line)
    if match:
        date = match.group(1).strip()
        url = match.group(2).strip()
        status = match.group(3).strip()

        if not top_urls or url in top_urls:
            sys.stdout.write("{}|{}|{}\t{}\n".format(url, "status", status, 1))
            sys.stdout.write("{}|{}|{}\t{}\n".format(url, "date", date, 1))
import sys
import re

pattern = re.compile(r'\[(\d{2}/\w{3}/\d{4}).*?"\w+ (.*?) HTTP.*?" (\d{3})')

for line in sys.stdin:
    match = pattern.search(line)
    if match:
        url = match.group(2)
        sys.stdout.write("{}\t1\n".format(url))
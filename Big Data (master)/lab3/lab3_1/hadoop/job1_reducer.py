import sys

current_url = None
count = 0

for line in sys.stdin:
    parts = line.strip().split('\t')
    if len(parts) != 2:
        continue
    url, val = parts
    val = int(val)
    if url == current_url:
        count += val
    else:
        if current_url:
            sys.stdout.write("{}\t{}\n".format(current_url, count))
        current_url = url
        count = val
if current_url:
    sys.stdout.write("{}\t{}\n".format(current_url, count))
import sys

current_key = None
count = 0

for line in sys.stdin:
    parts = line.strip().split('\t')

    if len(parts) != 2:
        continue

    key, cnt = parts

    try:
        cnt = int(cnt)
    except ValueError:
        continue

    if key == current_key:
        count += cnt
    else:
        if current_key is not None:
            print("{}\t{}".format(current_key, count))
        current_key = key
        count = cnt

if current_key is not None:
    print("{}\t{}".format(current_key, count))
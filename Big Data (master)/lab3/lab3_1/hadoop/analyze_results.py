import sys
import math
from collections import defaultdict


def read_top_urls(file):
    top_urls = []
    with open(file) as f:
        for line in f:
            parts = line.strip().split('\t')
            if len(parts) == 2:
                top_urls.append((parts[0], int(parts[1])))
    return top_urls


def read_status_counts(file):
    status_counts = defaultdict(dict)
    with open(file) as f:
        for line in f:
            parts = line.strip().split('\t')
            if len(parts) != 2:
                continue

            key, cnt = parts

            try:
                cnt = int(cnt)
            except ValueError:
                continue

            key_parts = key.split('|')
            if len(key_parts) != 3:
                continue

            url, typ, value = key_parts

            if typ == "status":
                status_counts[url][value] = cnt

    return status_counts


def read_date_counts(file):
    date_counts = defaultdict(dict)
    with open(file) as f:
        for line in f:
            parts = line.strip().split('\t')
            if len(parts) != 2:
                continue

            key, cnt = parts

            try:
                cnt = int(cnt)
            except ValueError:
                continue

            key_parts = key.split('|')
            if len(key_parts) != 3:
                continue

            url, typ, value = key_parts

            if typ == "date":
                date_counts[url][value] = cnt

    return date_counts


def detect_anomalies(date_counts):
    anomalies = {}
    for url, counts in date_counts.items():
        values = list(counts.items())
        if not values:
            anomalies[url] = {"low": [], "high": []}
            continue
        counts_only = [c for _, c in values]
        mean = sum(counts_only) / len(counts_only)
        variance = sum((c - mean) ** 2 for c in counts_only) / len(counts_only)
        std = math.sqrt(variance)
        low = []
        high = []
        for date, cnt in values:
            if cnt < mean - 2 * std:
                low.append((date, cnt))
            elif cnt > mean + 2 * std:
                high.append((date, cnt))
        anomalies[url] = {"low": low, "high": high}
    return anomalies


def main(top_file, status_file, date_file):
    top_urls = read_top_urls(top_file)
    status_counts = read_status_counts(status_file)
    date_counts = read_date_counts(date_file)
    anomalies = detect_anomalies(date_counts)

    print("\nTop 10 URLs")
    for url, cnt in top_urls:
        print("{}: {} requests".format(url, cnt))

    print("\nStatus Code Distribution:")
    for url, _ in top_urls:
        total = sum(status_counts.get(url, {}).values())
        print("URL: {}".format(url))
        if total == 0:
            print("  No requests")
        else:
            for status, cnt in sorted(status_counts.get(url, {}).items()):
                pct = cnt / total * 100
                print("  Status {}: {} requests ({:.2f}%)".format(status, cnt, pct))

    print("\nAnomalies by Date:")
    for url, _ in top_urls:
        if url in anomalies:
            print("URL: {}".format(url))
            low = anomalies[url]["low"]
            high = anomalies[url]["high"]
            if low:
                print("  Unusually low days:")
                for date, count in sorted(low):
                    print("    {}: {}".format(date, count))
            if high:
                print("  Unusually high days:")
                for date, count in sorted(high):
                    print("    {}: {}".format(date, count))
            if not low and not high:
                print("  No anomalies detected.")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3])

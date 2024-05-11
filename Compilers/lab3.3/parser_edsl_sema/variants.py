#!/usr/bin/env python3

with open('variants.md', encoding='utf8') as f:
    vars = f.read().strip().split('\n\n-----\n\n')

for n, v in enumerate(vars, 1):
    with open(f'variants/{n:02}', 'w', encoding='utf8') as f:
        print(v, file=f)

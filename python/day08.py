from math import gcd
from functools import reduce

with open("input08.txt", "r") as fp:
    lines = [line.strip() for line in fp]

directions = lines[0]
nodes = {}
for line in lines[2:]:
    node, paths = line.split(" = ")
    nodes[node] = [p for p in paths[1:-1].split(", ")]

# part 1
loc = 'AAA'
total = 0
while loc != 'ZZZ':
    loc = nodes[loc][0 if directions[total % len(directions)] == 'L' else 1]
    total += 1
print(total)

# part 2
locs = [n for n in nodes if n.endswith('A')]
cycles = []
for orig in locs:
    loc = orig
    timer = 0
    seen = {}
    while not (timer % len(directions), loc) in seen:
        seen[((timer % len(directions), loc))] = timer
        loc = nodes[loc][0 if directions[timer % len(directions)] == 'L' else 1]
        timer += 1
    cycle_len = timer - seen[(timer % len(directions), loc)]
    cycles.append(cycle_len)
    # cycles always end with 'Z'

print(reduce((lambda x, y: x * y // gcd(x, y)), cycles))

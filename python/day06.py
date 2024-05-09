# reproduced from my hasty phone python and pencil-and-paper version
from math import floor, ceil
from functools import reduce
from operator import mul

with open("input06.txt", "r") as fp:
    lines = [line.strip() for line in fp]

def solver(t, d):
    Δ = t**2 - 4*d
    return ((t - Δ**0.5)/2, (t + Δ**0.5)/2)

ts = [int(t) for t in lines[0].split()[1:]]
ds = [int(d) for d in lines[1].split()[1:]]

print(reduce(mul, [
    ceil(upper) - floor(lower) - 1
    for lower, upper in [solver(t, d) for t, d in zip(ts, ds)]
]))

bigt = int(''.join(str(t) for t in ts))
bigd = int(''.join(str(d) for d in ds))

lower, upper = solver(bigt, bigd)
print(ceil(upper) - floor(lower) - 1)


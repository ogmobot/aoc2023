from functools import reduce
from itertools import pairwise

with open("input09.txt", "r") as fp:
    lines = [line.strip() for line in fp]

rsub = lambda a, b: b - a

results_a = [] # could just use integers here
results_b = []
for line in lines:
    nums = [int(word) for word in line.split()]
    sequences = [nums]
    while not(all(x == 0 for x in sequences[0])):
        sequences.insert(
            0,
            [reduce(rsub, pair) for pair in pairwise(sequences[0])]
        )
    results_a.append(sum(s[-1] for s in sequences))
    results_b.append(reduce(rsub, (s[0] for s in sequences)))

print(sum(results_a))
print(sum(results_b))

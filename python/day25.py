from collections import defaultdict
import random

with open("input25.txt", "r") as fp:
    lines = [line.strip() for line in fp]

lines_ = """a: b c d
b: c d
c: d
d: e
e: f g h i
f: g h i
g: h i
h: i""".split("\n")

def random_cut(orig_edges):
    # orig_edges is [('abc', 'def'), ...]
    # pairs are in sorted order!
    edges = {
        frozenset(
            [frozenset([edge[0]]), frozenset([edge[1]])]
        ): [edge]
        for edge in orig_edges
    }
    #print(edges)
    while True:
        if len(edges) == 1:
            # (('subgraph', 'subgraph'), weight)
            return list(edges.items()).pop()
        keys = list(edges.keys())
        ab_pair = random.choices(
            keys,
            weights=[len(edges[k]) for k in keys]
        ).pop()
        weight = edges[ab_pair]
        a, b = list(ab_pair) # a, b are merged node
        new_node = a | b
        new_edges = defaultdict(list)
        for p, w in edges.items():
            x, y = list(p) # x, y are existing nodes
            if x in ab_pair and y in ab_pair:
                continue # delete this edge
            elif x in ab_pair or y in ab_pair:
                if x in ab_pair:
                    new_edges[frozenset({new_node, y})].extend(w)
                if y in ab_pair:
                    new_edges[frozenset({new_node, x})].extend(w)
            else:
                new_edges[p] += w
        edges = new_edges

wires = list()
#connections = defaultdict(list)
for line in lines:
    key, vals = line.split(": ")
    for val in vals.split():
        wires.append((key, val))
        #connections[val].append(key)
        #connections[key].append(val)
wires = [
    tuple(sorted(pair)) for pair in wires
]
# Takes ~15s per random_cut
cut = random_cut(wires)
attempts = 0
while len(cut[1]) > 3:
    #print(f'{attempts=}')
    #print(f'{len(cut[1])=}')
    cut = random_cut(wires)
    attempts += 1

subgraphs = list(cut[0])
soln = len(subgraphs[0]) * len(subgraphs[1])
print(soln)

#with open("soln.txt", "w") as fp:
    #fp.write(f'{soln}')

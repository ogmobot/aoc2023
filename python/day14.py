from functools import reduce

with open("input14.txt", "r") as fp:
    lines = [line.strip() for line in fp]

_lines = """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....""".split("\n") # 136

deltas = [(-1, 0), (0, -1), (1, 0), (0, 1)]

_cache = {}
def roll_em(grid, rocks, delta, limits):
    if (delta, frozenset(rocks)) in _cache:
        return _cache[(delta, frozenset(rocks))]
    max_r, min_r, max_c, min_c = limits
    dr, dc = delta
    rocks.sort(key=(lambda x: (x[0]*dr + x[1]*dc)), reverse=True)
    new_rocks = []
    for row, col in rocks:
        while (
            (row + dr, col + dc) not in grid
            and (row + dr, col + dc) not in new_rocks
            and (row + dr <= max_r)
            and (row + dr >= min_r)
            and (col + dc <= max_c)
            and (col + dc >= min_c)
        ):
            row, col = row + dr, col + dc
        new_rocks.append((row, col))
    _cache[(delta, frozenset(rocks))] = new_rocks
    return new_rocks
        

def figure_stress(rocks, limits):
    max_r, min_r, max_c, min_c = limits
    return sum(max_r - row + 1 for (row, col) in rocks)
        
            

grid = set()
rocks = []
for r, line in enumerate(lines):
    for c, symb in enumerate(line):
        if symb == 'O':
            rocks.append((r, c))
        elif symb == '#':
            grid.add((r, c))

max_row = max([r for (r, c) in grid | set(rocks)])
min_row = min([r for (r, c) in grid | set(rocks)])
max_col = max([c for (r, c) in grid | set(rocks)])
min_col = min([c for (r, c) in grid | set(rocks)])

new_rocks = roll_em(grid, rocks, (-1, 0), (max_row, min_row, max_col, min_col))
print(figure_stress(new_rocks, (max_row, min_row, max_col, min_col)))

# Takes about a minute

seen = dict()
nees = dict()
cycle_start = None
reqd_steps = 4*1000000000

i = 0
while True:
    if i % 4 == 0:
        if frozenset(rocks) in seen:
            if cycle_start == None:
                cycle_start = seen[frozenset(rocks)]
                cycle_len = i - cycle_start
                rocks = nees[cycle_start + ((reqd_steps - cycle_start) % cycle_len)]
                print(f"{cycle_start=} {cycle_len=}")
                print(f"{(cycle_start + ((reqd_steps - cycle_start) % cycle_len))=}")
                break
        else:
            seen[frozenset(rocks)] = i
            nees[i] = rocks
            print(figure_stress(rocks, (max_row, min_row, max_col, min_col)))
    delta = deltas[i % len(deltas)]
    rocks = roll_em(grid, rocks, delta, (max_row, min_row, max_col, min_col))
    i += 1
print(figure_stress(rocks, (max_row, min_row, max_col, min_col)))

# 79723

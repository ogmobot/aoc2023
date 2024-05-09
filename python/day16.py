
with open("input16.txt", "r") as fp:
    lines = [line.strip() for line in fp]

lines_ = r""".|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....""".split()

grid = {}
for r, line in enumerate(lines):
    for c, symb in enumerate(line):
        grid[(r, c)] = symb

def print_grid(grid, lit):
    min_r = min(a for a, b in grid)
    max_r = max(a for a, b in grid)
    min_c = min(b for a, b in grid)
    max_c = max(b for a, b in grid)
    for r in range(min_r, max_r + 1):
        for c in range(min_c, max_c + 1):
            if (r, c) in lit:
                print("#", end="")
            else:
                print(grid.get((r, c), "?"), end="")
        print()

def vec_add(a, b):
    return (a[0] + b[0], a[1] + b[1])
BIGMAX = 0
def find_lit(loc, facing):
    beams = [(loc, facing)]
    lit = set()
    seen = set()
    maxlen = 0
    while beams:
        if len(beams) > maxlen:
            maxlen = len(beams)
        loc, facing = beams.pop()
        if loc not in grid:
            continue
        if (loc, facing) in seen:
            continue
        seen.add((loc, facing))
        lit.add(loc)
        if grid[loc] == '/':
            if facing == (0, 1):
                facing = (-1, 0)
            elif facing == (1, 0):
                facing = (0, -1)
            elif facing == (-1, 0):
                facing = (0, 1)
            elif facing == (0, -1):
                facing = (1, 0)
            else:
                print("uh oh")
            beams.append((vec_add(loc, facing), facing))
        elif grid[loc] == '\\':
            if facing == (0, 1):
                facing = (1, 0)
            elif facing == (1, 0):
                facing = (0, 1)
            elif facing == (-1, 0):
                facing = (0, -1)
            elif facing == (0, -1):
                facing = (-1, 0)
            else:
                print("uh oh")
            beams.append((vec_add(loc, facing), facing))
        elif grid[loc] == '-':
            if facing == (1, 0) or facing == (-1, 0):
                beams.append((vec_add(loc, (0, 1)), (0, 1)))
                beams.append((vec_add(loc, (0, -1)), (0, -1)))
            else:
                beams.append((vec_add(loc, facing), facing))
        elif grid[loc] == '|':
            if facing == (0, 1) or facing == (0, -1):
                beams.append((vec_add(loc, (1, 0)), (1, 0)))
                beams.append((vec_add(loc, (-1, 0)), (-1, 0)))
            else:
                beams.append((vec_add(loc, facing), facing))
        else:
            beams.append((vec_add(loc, facing), facing))
    global BIGMAX
    BIGMAX = max(maxlen, BIGMAX)
    return len(lit)

print(find_lit((0, 0), (0, 1)))

min_r = min(a for a, b in grid)
max_r = max(a for a, b in grid)
min_c = min(b for a, b in grid)
max_c = max(b for a, b in grid)

vals = []
for r in range(min_r, max_r + 1):
    vals.append(find_lit((r, min_c), (0, 1)))
    vals.append(find_lit((r, max_c), (0, -1)))
for c in range(min_c, max_c + 1):
    vals.append(find_lit((min_r, c), (1, 0)))
    vals.append(find_lit((max_r, c), (-1, 0)))

print(max(vals))
print(BIGMAX)

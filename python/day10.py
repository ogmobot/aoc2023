
with open("input10.txt", "r") as fp:
    lines = fp.readlines()

lines_ = """...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........""".split()

grid = {}
for row, line in enumerate(lines):
    for col, symb in enumerate(line):
        grid[(row, col)] = symb
        if symb == "S":
            start = (row, col)
            grid[(row, col)] = "|" # from inspection



#def get_adjacent(grid, coord):
#    row, col = coord
#    res = []
#    for dr, dc, connects in [(1, 0, "-J7"), (-1, 0), (0, 1), (0, -1)]:
#        candidate = (row + dr, col + dc)
#        symb =

def get_path(grid, start):
    facing = "down" # from inspection
    path = []
    row, col = start
    row = row + 1
    path = [(row, col)]
    while (row, col) != start:
        #print((row, col))
        symb = grid[(row, col)]
        if facing == "down":
            if symb == "|":
                row += 1
            elif symb == "L":
                col += 1
                facing = "right"
            elif symb == "J":
                col -= 1
                facing = "left"
        elif facing == "up":
            if symb == "|":
                row -= 1
            elif symb == "F":
                col += 1
                facing = "right"
            elif symb == "7":
                col -= 1
                facing = "left"
        elif facing == "right":
            if symb == "-":
                col += 1
            elif symb == "J":
                row -= 1
                facing = "up"
            elif symb == "7":
                row += 1
                facing = "down"
        elif facing == "left":
            if symb == "-":
                col -= 1
            elif symb == "L":
                row -= 1
                facing = "up"
            elif symb == "F":
                row += 1
                facing = "down"
        path.append((row, col))
    return path

def is_enclosed(pipes, grid, coord):
    crosses = 0
    r, c = coord
    while r >= 0 and c >= 0:
        r -= 1
        c -= 1
        if (r, c) in pipes:
            crosses += (2 if (grid[(r, c)] in "7L") else 1)
    return (crosses % 2) == 1

path = get_path(grid, start)
# part 1
print(len(path)//2)

pathset = set(path)
max_r = max(r for r, c in grid)
max_c = max(c for r, c in grid)
total = 0
for r in range(max_r + 1):
    for c in range(max_c + 1):
        if ((r, c) not in pathset) and is_enclosed(pathset, grid, (r, c)):
            total += 1
print(total)

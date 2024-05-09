import heapq

with open("input17.txt", "r") as fp:
    lines = [line.strip() for line in fp]

lines_ = """2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533""".split("\n")

grid = {}
max_r = 0
max_c = 0
for r, line in enumerate(lines):
    for c, symb in enumerate(line):
        grid[(r, c)] = int(symb)
        if r > max_r:
            max_r = r
        if c > max_c:
            max_c = c

def search_grid(grid, min_move, max_move):
    max_path_len = 0
    paths = [
        (0, (0, 0, 1, 0)),
        (0, (0, 0, 0, 1)),
    ] # cost, loc, facing
    heapq.heapify(paths)
    seen = set()
    while paths:
        if len(paths) > max_path_len:
            max_path_len = len(paths)
        path = heapq.heappop(paths)
        cost, p = path
        r, c, dr, dc = p
        if (r, c) == (max_r, max_c):
            print(f"{max_path_len=}")
            return path
        elif (r, c, dr, dc) in seen:
            continue
        else:
            seen.add((r, c, dr, dc))
        opts = []
        for i in range(min_move, max_move + 1):
            opts.append({'spaces': [(r + (j*dc), c + (j*dr)) for j in range(1, i + 1)], 'facing': (dc, dr)})
            opts.append({'spaces': [(r - (j*dc), c - (j*dr)) for j in range(1, i + 1)], 'facing': (-dc, -dr)})
        for opt in opts:
            new_r, new_c = opt['spaces'][-1]
            if (new_r, new_c) not in grid:
                continue
            new_dr, new_dc = opt['facing']
            added_cost = sum(grid[loc] for loc in opt['spaces'])
            heapq.heappush(paths, (cost + added_cost, (new_r, new_c, new_dr, new_dc)))

print(search_grid(grid, 1,  3)[0])
print(search_grid(grid, 4, 10)[0])

import heapq
from collections import deque

with open("input23.txt", "r") as fp:
    lines = [line.strip() for line in fp]

lines_ = """#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#""".strip().split('\n')

def get_adj(grid, coord, ignore_slopes):
    r, c = coord
    if not ignore_slopes:
        if grid[coord] == '>':
            return [(r, c + 1)]
        elif grid[coord] == '<':
            return [(r, c - 1)]
        elif grid[coord] == '^':
            return [(r - 1, c)]
        elif grid[coord] == 'v':
            return [(r + 1, c)]
    res = []
    for dr, dc in [(1, 0), (-1, 0), (0, -1), (0, 1)]:
        new_coord = (r + dr, c + dc)
        if grid.get(new_coord, '#') != '#':
            res.append(new_coord)
    return res

def find_crossroads(grid):
    crossroads = []
    for ((r, c), symb) in grid.items():
        if symb == '#':
            continue
        adj_count = 0
        for dr, dc in [(1, 0), (-1, 0), (0, -1), (0, 1)]:
            if grid.get((r + dr, c + dc), '#') != '#':
                adj_count += 1
        if adj_count > 2:
            crossroads.append((r, c))
    return crossroads

def find_all_paths(grid, start_point, terminals, ignore_slopes):
    paths = []
    distances = []
    r, c = start_point
    paths = [
        [start_point, adj]
        for adj in get_adj(grid, start_point, ignore_slopes)
    ]
    while paths:
        path = paths.pop()
        current = path[-1]
        if current in terminals:
            distances.append((current, len(path) - 1))
            continue
        for adj in get_adj(grid, current, ignore_slopes):
            if adj not in path:
                paths.append(path + [adj])
    return distances

def crossroad_graph(grid, start, end, ignore_slopes):
    crossroads = find_crossroads(grid)
    connections = dict()
    pathfinder = (
        lambda x: find_all_paths(grid, x, [end] + crossroads, ignore_slopes)
    )
    for c in [start] + crossroads:
        connections[c] = pathfinder(c)
    return connections  # (coord: [(coord, dist), ...])

# Some fun facts:
# The crossroad graph contains only 35 nodes.
# Since this is a depth-first-search, the length of the path-stack being
# explored is small -- in fact, it never grows larger than 27 items long.
# Hence, the algorithm never has more than 27 * 35 = 945 coordinates in
# memory at once.
def longest_path_to(grid, start, end, ignore_slopes):
    longest_stack = 0
    graph = crossroad_graph(grid, start, end, ignore_slopes)
    paths = [(0, start, set())]
    longest = 0
    while paths:
        dist, head, tail = paths.pop()
        if head == end:
            if dist > longest:
                longest = dist
            continue
        new_tail = tail.copy()
        new_tail.add(head)
        for adj, added_dist in graph[head]:
            if adj not in tail:
                paths.append((
                    dist + added_dist,
                    adj,
                    new_tail
                ))
    return longest

grid = {}
max_r = 0
max_c = 0
for r, line in enumerate(lines):
    for c, symb in enumerate(line):
        grid[(r, c)] = symb
        if r == 0 and symb == '.':
            start = (r, c)
        if r > max_r:
            max_r = r
        if c > max_c:
            max_c = c
for c in range(max_c + 1):
    if grid.get((max_r, c)) == '.':
        end = (max_r, c)
        break

# takes ~20s
print(longest_path_to(grid, start, end, False))
print(longest_path_to(grid, start, end, True))

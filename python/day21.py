from collections import defaultdict

with open("input21.txt", "r") as fp:
    lines = [line.strip() for line in fp]

lines_ = """...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........""".split("\n")

max_r = 0
max_c = 0
grid = {}
for row, line in enumerate(lines):
    for col, symb in enumerate(line):
        grid[(row, col)] = symb
        if symb == 'S':
            start = (row, col)
            grid[(row, col)] = '.'
        if col >= max_c:
            max_c = col
    if row > max_r:
        max_r = row
height = max_r + 1
width = max_c + 1

def search_for_limit(grid, start_point, limit):
    # mutates grid
    distances = [{start_point: 1}] # values are the number of sub-grids counted
    i = 0
    #seen_configs = set()
    while i < limit:
        seen = set()
        #if i % 100 == 0: print(i, "...")
        distances.append(defaultdict(int))
        start_points = distances[i]
        for sp, count in start_points.items():
            r, c = sp
            for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
                if ((r + dr), (c + dc)) in seen:
                    continue
                seen.add(((r + dr), (c + dc)))
                # screws up at corners
                if grid[(((r + dr) % height), ((c + dc) % width))] == '.':
                    distances[i + 1][(((r + dr) % height), ((c + dc) % width))] += count
        i += 1
    return distances

def even_odd_floodfill(grid, start_point, limit):
    timer = 0
    evens = {start_point}
    evens_total = 0
    
    odds = set()
    odds_total = 0
    
    while timer < limit:
        #if timer % 100 == 0: print(timer, '...', f'{len(odds)=} {len(evens)=}')
        if timer % 2 == 0:
            start_points = evens
            other = odds
        else:
            start_points = odds
            other = evens
        new_front = set()
        for sp in start_points:
            #print(sp)
            r, c = sp
            for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
                if (
                    (((r + dr), (c + dc)) not in other)
                    and (grid[(((r + dr) % height), ((c + dc) % width))] == '.')
                ):
                    new_front.add((r + dr, c + dc))
        if timer % 2 == 0:
            odds_total += len(odds)
            odds = new_front
        else:
            evens_total += len(evens)
            evens = new_front
        
        timer += 1
    return (len(evens) + evens_total, len(odds) + odds_total)

def approximate(grid, limit):
    hashes = list(grid.values()).count('#')
    grid_area = height * width
    return limit*limit*(1 - (hashes / grid_area))

def time_to_reach_edges(grid, start_point):
    # target is edges
    timer = 0
    evens = {start_point}
    evens_total = 0
    
    odds = set()
    odds_total = 0

    edges = dict()
    while len(edges) < 10:
        #if timer % 100 == 0: print(timer, '...', f'{len(odds)=} {len(evens)=}')
        if timer % 2 == 0:
            start_points = evens
            other = odds
        else:
            start_points = odds
            other = evens
        new_front = set()
        for sp in start_points:
            #print(sp)
            r, c = sp
            if ((r, c) not in grid) and ((r, c) not in edges):
                edges[(r, c)] = timer
                continue
            for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
                if (
                    (((r + dr), (c + dc)) not in other)
                    and (grid[(((r + dr) % height), ((c + dc) % width))] == '.')
                ):
                    new_front.add((r + dr, c + dc))
        if timer % 2 == 0:
            odds_total += len(odds)
            odds = new_front
        else:
            evens_total += len(evens)
            evens = new_front
        
        timer += 1
    return edges

def time_to_reach_corners(grid, start_point):
    # target is edges
    timer = 0
    evens = {start_point}
    evens_total = 0
    
    odds = set()
    odds_total = 0

    corners = dict()
    while len(corners) < 10:
        #if timer % 100 == 0: print(timer, '...', f'{len(odds)=} {len(evens)=}')
        if timer % 2 == 0:
            start_points = evens
            other = odds
        else:
            start_points = odds
            other = evens
        new_front = set()
        for sp in start_points:
            #print(sp)
            r, c = sp
            if ((r < 0 or r >= height) and (c < 0 or c >= width)) and (r, c) not in corners:
                corners[(r, c)] = timer
                continue
            for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
                if (
                    (((r + dr), (c + dc)) not in other)
                    and (grid[(((r + dr) % height), ((c + dc) % width))] == '.')
                ):
                    new_front.add((r + dr, c + dc))
        if timer % 2 == 0:
            odds_total += len(odds)
            odds = new_front
        else:
            evens_total += len(evens)
            evens = new_front
        
        timer += 1
    return corners

# Every move goes from a black square onto a white square or vice versa.
# Hence, once a square is marked as "odd", it will show up in the final count.


'''
evens, odds = even_odd_floodfill(grid, start, 100) # 6536
print(evens)
evens, odds = even_odd_floodfill(grid, start, 500) # 167004
print(evens)
evens, odds = even_odd_floodfill(grid, start, 1000) # 668697
print(evens)
evens, odds = even_odd_floodfill(grid, start, 5000) # 16733044
print(evens)
'''

evens, odds = even_odd_floodfill(grid, start, 1375)
print(odds)

PART_1 = 64
PART_2 = 26501365 # = 202300 * 131 + 65 (where 131 is height and 65 is dist to first edge)

# 639 133 389 136 005
# is too high

# wolframalpha's quadratic approximation is
# 638 238 358 607 363
# but this is too low

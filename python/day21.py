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

lines_ = """...
.S.
..#""".split("\n")

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

def floodfill(orig_grid, start, limit):
    grid = orig_grid.copy()
    front = {start}
    timer = 0
    while timer <= limit:
        new_front = set()
        #print(front)
        for r, c in front:
            #print(r, c)
            grid[(r, c)] = {0: 'E', 1: 'O'}[timer % 2]
            #print_grid(grid)
            for dr, dc in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
                new_r = r + dr
                new_c = c + dc
                if (new_r, new_c) not in grid:
                    grid[(new_r, new_c)] = orig_grid[(new_r % height, new_c % width)]
                if grid[(new_r, new_c)] == '.':
                    new_front.add((new_r, new_c))
        front = new_front
        timer += 1
                    
    return grid

def count_of(piece, val):
    return list(piece.values()).count(val)

def print_grid(g):
    min_r = min(r for (r, _) in g)
    max_r = max(r for (r, _) in g)
    min_c = min(c for (_, c) in g)
    max_c = max(c for (_, c) in g)
    for r in range(min_r, max_r + 1):
        for c in range(min_c, max_c + 1):
            print(g.get((r, c), ' '), end=' ')
        print()
    return

PART_1 = 64 # soln 3853
PART_2 = 26501365 # = 202300 * 131 + 65 (where 131 is height and 65 is dist to first edge)

res = floodfill(grid, start, PART_1)
print(count_of(res, ('E' if PART_1 % 2 == 0 else 'O')))

#PART_2 = 458 # 131 * 3 + 65 => should return 191585
#PART_2 = 1375 # 131 * 10 + 65 => should return 1722471
#PART_2 = 65 # should return 3853

start_r, start_c = start

#PART_2 = height * 4 + start_r
multiplier = (PART_2 - start_r) // height
#res = floodfill(grid, start, PART_2)
#print(count_of(res, ('E' if PART_2 % 2 == 0 else 'O')))


'''
 1*1   * = corner piece
17X71  1 = one-eighth piece
*XXX*  7 = seven-eighths piece
17X71  X = full piece
 1*1
'''

big_grid = floodfill(grid, start, start_r + (height * 2))
# TODO make this not so lame
full_tile = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= 0 and r < height and c >= 0 and c < width)
}
upper_right_one_eighth = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= (-2 * height) and r < (-1 * height) and c >= width and c < (2 * width))
}
upper_left_one_eighth = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= (-2 * height) and r < (-1 * height) and c >= (-1 * width) and c < 0)
}
lower_left_one_eighth = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= (2 * height) and r < (3 * height) and c >= (-1 * width) and c < 0)
}
lower_right_one_eighth = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= (2 * height) and r < (3 * height) and c >= width and c < (2 * width))
}
upper_right_seven_eighths = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= (-1 * height) and r < 0 and c >= width and c < (2 * width))
}
upper_left_seven_eighths = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= (-1 * height) and r < 0 and c >= (-1 * width) and c < 0)
}
lower_left_seven_eighths = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= height and r < (2 * height) and c >= (-1 * width) and c < 0)
}
lower_right_seven_eighths = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= height and r < (2 * height) and c >= width and c < (2 * width))
}
top_corner = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= (-2 * height) and r < (-1 * height) and c >= 0 and c < width)
}
right_corner = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= 0 and r < height and c >= (2 * width) and c < (3 * width))
}
bottom_corner = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= (2* height) and r < (3 * height) and c >= 0 and c < width)
}
left_corner = {
    (r, c): v for (r, c), v in big_grid.items()
    if (r >= 0 and r < height and c >= (-2 * width) and c < (-1 * width))
}

total = 0
# odds
for piece, count in [
    # the seven-eighths might need to all be flipped
    (lower_left_seven_eighths,  multiplier - 1),
    (lower_right_seven_eighths, multiplier - 1),
    (upper_left_seven_eighths,  multiplier - 1),
    (upper_right_seven_eighths, multiplier - 1),
    # the one-eighths might need to all be flipped
    (lower_left_one_eighth,  multiplier),
    (lower_right_one_eighth, multiplier),
    (upper_left_one_eighth,  multiplier),
    (upper_right_one_eighth, multiplier),
    #
    (top_corner,    1),
    (bottom_corner, 1),
    (left_corner,   1),
    (right_corner,  1),
    # full tiles
    (full_tile, (multiplier - 1)**2)
]:
    total += (count * count_of(piece, 'O'))

# evens
total += ((multiplier ** 2) * count_of(full_tile, 'E'))

print(total)

# TODO offset everything so fewer unique pieces are required (8 instead of 12)


with open("input13.txt", "r") as fp:
    all_text = fp.read()

def row_symmetrical_about(row, index):
    return all(a == b for a, b in zip(reversed(row[:index]), row[index:]))

def find_horizontal_axis(grid, forbid=None):
    for i in range(1, len(grid)):
        if i == forbid: continue
        if all(a == b for a, b in zip(reversed(grid[:i]), grid[i:])):
            return i
    return None
def find_vertical_axis(grid, forbid=None):
    for i in range(1, len(grid[0])):
        if i == forbid: continue
        if all(row_symmetrical_about(row, i) for row in grid):
            return i
    return None

def smudge_grid(grid, r, c):
    smudge = ('.' if  grid[r][c] == '#' else '#')
    #if grid[r][c] == '#':
    smudge_line = grid[r][:c] + smudge + grid[r][c+1:]
    return grid[:r] + [smudge_line] + grid[r+1:]
    #return None

grids = []
for subgrid in all_text.strip().split("\n\n"):
    grids.append(subgrid.split("\n"))

example = """#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.""".split("\n")
examples = [example, """#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#""".split("\n")]
_example = ['#####..#.##', '.##....#...', '..#.#.##.##', '######..#..', '#..#####..#', '#..#####..#', '######..#..']
row = example[0]

hs = 0
vs = 0
for grid in grids:
    h = find_horizontal_axis(grid)
    v = find_vertical_axis(grid)
    if h != None:
        hs += h
    elif v != None:
        vs += v
    else:
        print("Oh no!")
        print(grid)
print( 100*hs + vs )

hs = 0
vs = 0
for grid in grids:
    done = False
    old_h = find_horizontal_axis(grid)
    old_v = find_vertical_axis(grid)
    for r in range(len(grid)):
        for c in range(len(grid[r])):
            g = smudge_grid(grid, r, c)
            h = find_horizontal_axis(g, old_h)
            v = find_vertical_axis(g, old_v)
            if h != None:
                hs += h
                done = True
                break
            elif v != None:
                vs += v
                done = True
                break
            else:
                continue
        if done: break
    else:
        print("oh no!")
        #print(grid)
        
print( 100*hs + vs )

# Better way: turn each grid into a dict of coords, then apply a function
# to flip it across each possible axis. Compare them using the intersection
# of their keys and count differences. Part 1: look for 0. Part 2: look for 1.

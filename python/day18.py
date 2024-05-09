
with open("input18.txt", "r") as fp:
    lines = [line.strip() for line in fp]

lines_ = """R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)""".split("\n")

def decode_colour(s):
    return (int(s[2:-2], 16), {'0':'R', '1': 'D', '2':'L', '3':'U'}[s[-2]])

# didn't need this in the end
def floodfill(grid, start_point):
    # mutates grid
    to_fill = [start_point]
    while to_fill:
        r, c = to_fill.pop(0)
        if r == -1 and c == -1:
            print("panic!")
            return
        for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            if (r + dr, c + dc) not in grid:
                grid[(r + dr, c + dc)] = '()'
                to_fill.append((r + dr, c + dc))
    return

def shoelace(vertices):
    shifted_vertices = vertices[1:] + [vertices[0]]
    positive_terms = [a[0] * b[1] for a, b in zip(vertices, shifted_vertices)]
    negative_terms = [a[1] * b[0] for a, b in zip(vertices, shifted_vertices)]
    return abs(sum(positive_terms) - sum(negative_terms)) // 2

def dig_trench(lines, do_decode=False):
    #grid = {}
    vertices = []
    perimeter = 0
    r = 1
    c = 0
    for line in lines:
        direction, amount, colour = line.split()
        if do_decode:
            amount, direction = decode_colour(colour)
        #print(amount, direction)
        dr, dc = {
            'U': (-1,  0),
            'D': ( 1,  0),
            'L': ( 0, -1),
            'R': ( 0,  1),
        }[direction]
        
        r += (dr * int(amount))
        c += (dc * int(amount))
        perimeter += int(amount)
        #for _ in range(int(amount)):
            #r += dr
            #c += dc
            #grid[(r, c)] = colour
        vertices.append((r, c))
    #return grid
    return vertices, perimeter

#grid = dig_trench(lines)
#floodfill(grid, (2, 2)) # by inspection
#print(len(grid))

vertices, perimeter = dig_trench(lines, False)
print(shoelace(vertices) + (perimeter // 2) +1)

vertices, perimeter = dig_trench(lines, True)
print(shoelace(vertices) + (perimeter // 2) +1)

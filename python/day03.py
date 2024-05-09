with open("input03.txt", "r") as fp:
    lines = fp.readlines()

grid = {}
max_r = 0
max_c = 0
for r, line in enumerate(lines):
    max_r = max(max_r, r)
    for c, symb in enumerate(line.strip()):
        grid[(r, c)] = symb
        max_c = max(max_c, c)

def adjacent_coords(coord):
     for delta in [(0, 1), (0, -1), (1, 0), (-1, 0), (1, -1), (1, 1), (-1, -1), (-1, 1)]:
         yield (coord[0] + delta[0], coord[1] + delta[1])

stars = {} # maps coord: [nums]

number = ''
is_adj = False
total = 0
starlocs = set()
for r in range(max_r + 1):
    for c in range(max_c + 1):
        if grid.get((r, c), ".").isdigit():
            number += grid[(r, c)]
            for coord in adjacent_coords((r, c)):
                if (grid.get(coord, ".") != '.') and (not grid.get(coord, ".").isdigit()):
                    is_adj = True
                    if grid.get(coord) == "*":
                        starlocs.add(coord)
        if (not grid.get((r, c), ".").isdigit()) or c == max_c:
            if number:
                #print(number)
                if is_adj:
                    #print("*")
                    total += int(number)
                if starlocs:
                    for star in starlocs:
                        if star not in stars:
                            stars[star] = []
                        stars[star].append(int(number))
                number = ''
                starlocs = set()
            is_adj = False
if number and is_adj:
    total += int(number)
print(total)

res = 0
for star, coords in stars.items():
    if len(coords) == 2:
        res += (coords[0] * coords[1])
print(res)

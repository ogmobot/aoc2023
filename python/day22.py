from collections import defaultdict

with open("input22.txt", "r") as fp:
    lines = [line.strip() for line in fp]

lines_ = """1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9""".strip().split("\n")

GROUND = -999999

def brick_contains_coord(brick, coord):
    xs, ys, zs = brick
    x, y, z = coord
    return (
        (xs[0] <= x <= xs[1])
        and (ys[0] <= y <= ys[1])
        and (zs[0] <= z <= zs[1])
    )   

# z-coordinate gets more negative over time, and can't go less than 1
# takes t ~ 146
def drop_all(bricks):
    timer = 0
    res = False
    something_moved = True
    while something_moved:
        if timer % 2 == 0: print(f'{timer=}')
        something_moved = False
        new_bricks = []
        for brick in bricks.copy():
            z_minus_1 = min(brick[2]) - 1
            if z_minus_1 > 0:
                can_move = True
                for x in range(brick[0][0], brick[0][1] + 1): #xs
                    for y in range(brick[1][0], brick[1][1] + 1): #ys
                        if any(brick_contains_coord(b, (x, y, z_minus_1)) for b in bricks):
                            can_move = False
            else:
                can_move = False
            if can_move:
                new_brick = (brick[0], brick[1], (brick[2][0] - 1, brick[2][1] - 1))
                new_bricks.append(new_brick)
                something_moved = True
                res = True
            else:
                new_bricks.append(brick)
        bricks = new_bricks
        timer += 1
    return (res, bricks)

def drop_initial(bricks):
    terrain = dict()
    settled = []
    bricks = sorted(bricks, key=(lambda b: min(b[2])))
    for b in bricks:
        movable = can_drop(b, terrain)
        while movable:
            b = (b[0], b[1], (b[2][0] - 1, b[2][1] - 1), b[3])
            movable = can_drop(b, terrain)
        settled.append(b)
        #print(f'{b[3]} settled at {b[:3]}')
        for x in range(b[0][0], b[0][1] + 1):
            for y in range(b[1][0], b[1][1] + 1):
                for z in range(b[2][0], b[2][1] + 1):
                    terrain[(x, y, z)] = b[3]
    return terrain, settled

def can_drop(b, terrain):
    if min(b[2]) - 1 == 0:
        return False
    for x in range(b[0][0], b[0][1] + 1):
        for y in range(b[1][0], b[1][1] + 1):
            if (x, y, min(b[2]) - 1) in terrain:
                return False
    return True

def find_supports(brick, terrain):
    #print(brick)
    supports = set()
    if min(b[2]) == 1:
        supports.add(GROUND) # supported by ground
    for x in range(b[0][0], b[0][1] + 1):
        for y in range(b[1][0], b[1][1] + 1):
            #print(f'testing {(x, y, min(b[2]) - 1)}')
            if (x, y, min(b[2]) - 1) in terrain:
                supports.add(terrain[(x, y, min(b[2]) - 1)])
    return supports

i = 0
bricks = []
for line in lines:
    a, b = line.split("~")
    x1, y1, z1 = [int(v) for v in a.split(",")]
    x2, y2, z2 = [int(v) for v in b.split(",")]
    bricks.append(((x1, x2), (y1, y2), (z1, z2), i))
    i += 1

terrain, bricks = drop_initial(bricks)

essentials = set()
supported_by = {}
for b in bricks:
    supported_by[b[3]] = find_supports(b, terrain)
    #print(f'{b[3]} supported by {supported_by[b[3]]}')
    if (len(supported_by[b[3]]) == 1) and (GROUND not in supported_by[b[3]]):
        essentials.add(list(supported_by[b[3]])[0])

# part 1
print(len(bricks) - len(essentials))

# part 2
total = 0
for b in bricks:
    #print(f'Testing {b[3]}')
    gone = {b[3]}
    moved = True
    while moved:
        moved = False
        for key, supporters in supported_by.items():
            if key in gone:
                continue
            remaining_supporters = set(supporters) - gone
            #print(f'{remaining_supporters=}')
            if len(remaining_supporters) == 0:
                moved = True
                gone.add(key)
    #print(f'{gone=}')
    total += len(gone) - 1
print(total)

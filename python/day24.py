import numpy

with open("input24.txt", "r") as fp:
    lines = [line.strip() for line in fp]

lines = """19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3""".split("\n")

MIN_DIM = 200000000000000
MAX_DIM = 400000000000000
#MIN_DIM = 7
#MAX_DIM = 27

def find_intersection_xy(a, b): # a and b are in the form (x, ux, y, vy)
    x0, u0, y0, v0 = a
    x1, u1, y1, v1 = b
    # Parallel lines don't intersect
    if u0 * v1 == u1 * v0: return None
    x = ((((u0 * v1 * x1) - (u1 * v0 * x0)) - (u1 * u0 * (y1 - y0)))
         /((u0 * v1) - (u1 * v0)))
    y = ((((v0 * u1 * y1) - (v1 * u0 * y0)) - (v1 * v0 * (x1 - x0)))
         /((v0 * u1) - (v1 * u0)))
    # Make sure this isn't in the past
    if (x - x0)/u0 < 0:
        return None
    if (x - x1)/u1 < 0:
        return None
    if (y - y0)/v0 < 0:
        return None
    if (y - y1)/v1 < 0:
        return None
    return (x, y)

hailstones = []
for line in lines:
    pos, vel = line.split(" @ ")
    pos = [int(x.strip()) for x in pos.split(", ")]
    vel = [int(x.strip()) for x in vel.split(", ")]
    #print(pos, vel)
    hailstones.append((pos, vel))

total = 0
for i in range(len(hailstones)):
    for j in range(i):
        (x0, y0, z0), (u0, v0, w0) = hailstones[i]
        (x1, y1, z1), (u1, v1, w1) = hailstones[j]
        intersection = find_intersection_xy((x0, u0, y0, v0), (x1, u1, y1, v1))
        if intersection != None and all((MIN_DIM <= x <= MAX_DIM) for x in intersection):
            total += 1
print(total)

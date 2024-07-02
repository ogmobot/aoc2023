import re
# Does this really count as a solution?
# Perhaps one day I'll write my own solver...
import z3

with open("input24.txt", "r") as fp:
    lines = [line.strip() for line in fp]

# Turns out only 5 data points are needed to solve this.
# Extra lines just make the solver take longer.
lines = lines[:5]

values = [
    [int(x) for x in re.findall("[\\d\\-]+", line)]
    for line in lines
]

X  = z3.Int( "X")
Y  = z3.Int( "Y")
Z  = z3.Int( "Z")
VX = z3.Int("VX")
VY = z3.Int("VY")
VZ = z3.Int("VZ")
ts = [z3.Int(f"t{i}") for i in range(len(values))]
res = z3.Int("res")

solver = z3.Solver()

for i, vals in enumerate(values):
    x, y, z, vx, vy, vz = vals
    solver.append(
        X + (VX * ts[i]) == x + (vx * ts[i]),
        Y + (VY * ts[i]) == y + (vy * ts[i]),
        Z + (VZ * ts[i]) == z + (vz * ts[i])
    )

solver.add(res == X + Y + Z)
solver.check()
solution = solver.model()

print(solution[res])

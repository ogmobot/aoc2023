with open("input11.txt", "r") as fp:
    lines = [line.strip() for line in fp]

raw_galaxies = []
r = 0
for line in lines:
    c = 0
    saw_galaxy = False
    for symb in line:
        if symb == "#":
            raw_galaxies.append((r, c))
            saw_galaxy = True
        c += 1
    r += 1
    if not saw_galaxy:
        r += 999999 # part 1: add 1 here instead

galaxies = []
new_c = 0
for orig_c in range(max(c for r, c in raw_galaxies) + 1):
    this_col = [(r, c) for r, c in raw_galaxies if c == orig_c]
    galaxies.extend([(r, new_c) for r, c in this_col])
    new_c += 1
    if len(this_col) == 0:
        new_c += 999999 # part 1: add 1 here instead

def manhattan(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

total = 0
for i in range(len(galaxies)):
    for j in range(i + 1, len(galaxies)):
        total += manhattan(galaxies[i], galaxies[j])
print(total)

# I'M ON THE LEADERBOARD LET'S GOOOOO

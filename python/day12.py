import re

with open("input12.txt", "r") as fp:
    lines = [line.strip() for line in fp]

_lines = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1""".split("\n")
#lines = ["?###???????? 3,2,1"]
# expect 1 + 4 + 1 + 4 + 10 = 21
# then expect 1 + 16384 + 1 + 16 + 2500 + 506250 = 525152

def maybe_consume_value(row, v):
    if v <= len(row) and all(c in "?#" for c in row[:v]):
        if len(row) == v or row[v] in ".?":
            return "." + row[v + 1:]
    return None

_cache = {}
def num_solns(row, vals):
    joined_vals = tuple(vals)
    if (row, joined_vals) in _cache:
        return _cache[(row, joined_vals)]
    if not vals:
        return all(c in ".?" for c in row)
    
    row = row.lstrip(".")
    if row.startswith("?"):
        res = num_solns("." + row[1:], vals) + num_solns("#" + row[1:], vals)
        _cache[(row, joined_vals)] = res
        return res
    new_row = maybe_consume_value(row, vals[0])
    if new_row:
        res = num_solns(new_row, vals[1:])
        _cache[(row, joined_vals)] = res
        return res
    _cache[(row, joined_vals)] = 0
    return 0

def solve_line(line):
    row, vals = line.split(" ")
    vals = [int(x) for x in vals.split(",")]
    res = num_solns(row, vals)
    #print(f"{row} {vals} => {res}")
    #print("=========")
    return res

print(sum(solve_line(line) for line in lines))

# part 2

def solve_line(line):
    row, vals = line.split(" ")
    vals = [int(x) for x in vals.split(",")]
    row = "?".join([row for _ in range(5)])
    vals = vals * 5
    res = num_solns(row, vals)
    #print(f"{row} {vals} => {res}")
    #print("=========")
    return res

#print(f"solving for {len(lines)} lines...")
print(sum(solve_line(line) for line in lines))


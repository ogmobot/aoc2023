import re

with open("input05.txt", "r") as fp:
    text = fp.read()

text_ = """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""

def get_type(s):
    # return tuple
    m = re.search(r"(\w+)-to-(\w+) map:", s)
    return (m.group(1), m.group(2))

def make_dict(lines):
    res = {}
    for line in lines:
        if line:
            nums = [int(x) for x in line.split()]
            res[nums[0]] = (nums[1], nums[2])
    return res

def get_table(key, almanac):
    for k, v in almanac.items():
        a, b = k
        if a == key:
            #print (b, v)
            return (b, v)

def map_to_location(ranges, data):
    fromtype = "seed"
    while fromtype != "location":
        new_ranges = []
        #print(f"from {fromtype} ", end="")
        fromtype, table = get_table(fromtype, data)
        #print(f"to {fromtype}")
        #print(ranges)
        while ranges:
            #print(ranges)
            lower, upper = ranges.pop()
            for dst, v in table.items():
                src, length = v
                #print(f"{lower=} {upper=}")
                #print(f"{src=} {dst=} {length=}")
                if lower >= src and upper <= src + length:
                    # map entire range
                    new_lower = dst + (lower - src)
                    new_upper = dst + (upper - src)
                    new_ranges.append((new_lower, new_upper))
                    break
                elif lower < src + length and upper > src + length:
                    # overlap; split range
                    ranges.append((lower, src + length - 1))
                    ranges.append((src + length, upper))
                    break
                elif lower < src and upper > src:
                    # overlap; split range
                    ranges.append((lower, src - 1))
                    ranges.append((src, upper))
                    break
                else:
                    # no overlap
                    continue
            else:
                # no overlap
                new_ranges.append((lower, upper))
        ranges = new_ranges
    return ranges

# range is startnum and length
data = {}
sections = text.split("\n\n")
#print(len(sections))
seednums = [int(x) for x in sections[0].split()[1:]]
seed_ranges_p1 = [(x, x + 1) for x in seednums]
seed_ranges_p2 = []
for i in range(0, len(seednums), 2):
    seed_ranges_p2.append((seednums[i], seednums[i] + seednums[i+1]))
    
for section in sections[1:]:
    lines = section.split("\n")
    typestring = lines[0]
    data[get_type(typestring)] = make_dict(lines[1:])

for seed_ranges in [seed_ranges_p1, seed_ranges_p2]:
    ranges = map_to_location(seed_ranges, data)
    minval = min(lower for lower, upper in ranges if lower != upper)
    print(minval)

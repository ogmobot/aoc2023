import re
from collections import defaultdict

def wfhelper(part, key, ineq, num, target):
    part_val = part[key]
    if ineq == "<" and part_val < int(num):
        return target
    elif ineq == ">" and part_val > int(num):
        return target
    else:
        return None

class Workflow:
    def __init__(self, line):
        #print(line)
        self.rules = []
        vals = line[1:-1].split(",")
        for v in vals:
            gs = re.match(r"(\w)([<>])(\d+):(\w+)", v)
            if gs:
                key = gs.group(1)
                ineq = gs.group(2)
                num = gs.group(3)
                target = gs.group(4)
                #print(f"Adding rule {key=} {ineq=} {num=} {target=}")
                self.rules.append((key, ineq, num, target))
            else:
                self.default = v
                
    def process(self, part):
        for rule in self.rules:
            res = wfhelper(part, *rule)
            if res != None:
                return res
        return self.default

    def multiprocess(self, multipart):
        #print(multipart)
        results = defaultdict(list)
        for key, ineq, num, target in self.rules:
            if ineq == ">" and multipart[key + "min"] > int(num):
                results[target].append(multipart)
                break
            elif ineq == ">" and multipart[key + "max"] < int(num):
                # go to the next rule
                continue
            elif ineq == ">":
                accepted = multipart.copy()
                accepted[key + "min"] = int(num) + 1
                results[target].append(accepted)
                # mutate the part that goes to the next rule
                multipart[key + "max"] = int(num)
            elif ineq == "<" and multipart[key + "max"] < int(num):
                results[target].append(multipart)
                break
            elif ineq == "<" and multipart[key + "min"] > int(num):
                continue
            elif ineq == "<":
                accepted = multipart.copy()
                accepted[key + "max"] = int(num) - 1
                results[target].append(accepted)
                # mutate the part that goes to the next rule
                multipart[key + "min"] = int(num)
            else:
                print("panic!")
                assert False
        else:
            results[self.default].append(multipart)
        #print(results)
        return results

def parse_part(s):
    s = s[1:-1]
    vals = s.split(",")
    res = {}
    for val in vals:
        bits = val.split("=")
        res[bits[0]] = int(bits[1])
    return res

def parse_workflows(lines):
    res = {}
    for line in lines:
        bracket = line.index("{")
        name = line[:bracket]
        res[name] = Workflow(line[bracket:])
    return res

def get_rating(part):
    return sum(part.values())

def process_all(parts, workflows):
    total = 0
    for part in parts:
        #print(part)
        target = "in"
        while target != "A" and target != "R":
            target = workflows[target].process(part)
            #print(f"-> {target}", end="")
        if target == "A":
            total += get_rating(part)
        #print()
    return total

def individuals(mp):
    product = 1
    for key in "xmas":
        product *= (mp[key + "max"] - mp[key + "min"] + 1)
    return product

with open("input19.txt", "r") as fp:
    lines = fp.read().strip()

lines_ = """px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"""

workflows_s, parts = lines.split("\n\n")
workflows = parse_workflows(workflows_s.split("\n"))

parts = [parse_part(p) for p in parts.split("\n")]

# part 1
print(process_all(parts, workflows))

# part 2
multiparts = {
    "in": [{
        "xmin": 1, "xmax": 4000,
        "mmin": 1, "mmax": 4000,
        "amin": 1, "amax": 4000,
        "smin": 1, "smax": 4000,
    }]
}

while any((k not in ["A", "R"]) for k in multiparts):
    new_multiparts = defaultdict(list)
    for target in list(multiparts):
        if target == "A" or target == "R":
            new_multiparts[target] = multiparts[target]
            continue
        for multipart in multiparts[target]:
            res = workflows[target].multiprocess(multipart)
            for k, v in res.items():
                new_multiparts[k].extend(v)
    multiparts = {k:v for k, v in new_multiparts.items() if v}

print(sum(individuals(mp) for mp in multiparts["A"]))

from collections import defaultdict

with open("input15.txt", "r") as fp:
    text = fp.read().strip()

_text = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

the_map = defaultdict(list)

def do_hash(s):
    current = 0
    for c in s:
        current = ((current + ord(c)) * 17) % 256
    return current

def do_op(s):
    if s.endswith("-"):
        label = s.rstrip("-")
        h = do_hash(label)
        for i in range(len(the_map[h])):
            if the_map[h][i][0] == label:
                the_map[h].pop(i)
                break
    elif "=" in s:
        label, focal = s.split("=")
        h = do_hash(label)
        for i in range(len(the_map[h])):
            if the_map[h][i][0] == label:
                the_map[h].pop(i)
                the_map[h].insert(i, (label, int(focal)))
                break
        else:
            the_map[h].append((label, int(focal)))

words = text.split(",")

print(sum(do_hash(s) for s in words))

for word in words:
    do_op(word)
    #print(word, the_map)

total = 0
for i in range(256):
    subtotal = 0
    for j, lens in enumerate(the_map[i]):
        subtotal += ((i + 1) * (j + 1) * lens[1])
    total += subtotal

print(total)

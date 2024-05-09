with open("input04.txt", "r") as fp:
    lines = [line.strip() for line in fp]

lines_ = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".split("\n")

# hasty version

# part 1
total = 0
for line in lines:
    winnerss, numss = (line.split(": ")[1]).split(" | ")
    winners = [int(w) for w in winnerss.split()]
    nums = [int(n) for n in numss.split()]
    overlap = len(set(winners).intersection(nums))
    if overlap >= 1:
        total += (2**(overlap - 1))
print(total)

# part 2
scratchcards = {}
for line in lines:
    gamenum = int(line.split()[1].rstrip(":"))
    winnerss, numss = (line.split(": ")[1]).split(" | ")
    winners = [int(w) for w in winnerss.split()]
    nums = [int(n) for n in numss.split()]
    scratchcards[gamenum] = (winners, nums)

to_process = {(n+1):1 for n in range(len(scratchcards))}
duds = 0

n = 1
while n in to_process:
    winners, nums = scratchcards[n]
    overlap = len(set(winners).intersection(nums))
    duds += to_process[n]
    for i in range(overlap):
        to_process[n+i+1] += to_process[n]
    n += 1
    
print(duds)


# refined version

import re
from collections import defaultdict

scratchcards = {}
total = 0
for line in lines:
    cardvals = re.match(r"Card\s+(\d+):\s+(.*)\s\|\s(.*)", line)
    gamenum = int(cardvals.group(1))
    winners = set(int(w) for w in cardvals.group(2).split())
    nums = set(int(n) for n in cardvals.group(3).split())
    scratchcards[gamenum] = (winners, nums)

points = 0
cardfreqs = defaultdict(int) # just in case
cardfreqs |= {n: 1 for n in scratchcards}
for n in sorted(scratchcards):
    winners, nums = scratchcards[n]
    overlaps = len(winners & nums)
    if overlaps > 0:
        points += (2**(overlaps - 1))
    for i in range(overlaps):
        cardfreqs[n + i + 1] += cardfreqs[n]
print(points)
print(sum(cardfreqs.values()))

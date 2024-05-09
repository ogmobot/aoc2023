with open("input02.txt", "r") as fp:
    lines = fp.readlines()

# hasty solution

target_bag_p1 = {
    "red": 12,
    "green": 13,
    "blue": 14
}

def powerof(bag):
    return bag["red"] * bag["green"] * bag["blue"]

total = 0
powersum = 0

for line in lines:
    parts = line.split(": ")
    game_id = int(parts[0].split()[1])
    draws = parts[1].split(";")
    bag = {}
    for draw in draws:
        balls = draw.split(",")
        for ball in balls:
            n = int(ball.split()[0])
            colour = ball.split()[1]
            bag[colour] = max(n, bag.get(colour, 0))
    if all(n <= target_bag_p1.get(c, 0) for c, n in bag.items()):
        total += game_id
    powersum += powerof(bag)

print(total, powersum)

# refined solution

# A friend of mine said I should try programming in a different language

from functools import reduce
from operator import mul

with open("input02.txt", "r") as 言及:
    文々 = 言及.readlines()

def 鞄に変える(文):
    _, 情報 = 文.split(": ")
    鞄 = {}
    for 引く in 情報.split("; "):
        for 対 in 引く.split(", "):
            数, 色 = 対.split()
            鞄[色] = max(int(数), 鞄.get(色, 0))
    return 鞄

def 鞄の力(鞄):
    return reduce(mul, 鞄.values())

目的 = {
    "red": 12,
    "green": 13,
    "blue": 14
}

鞄々 = list(map(鞄に変える, 文々))
print(sum(位置 + 1
          for 位置, 鞄 in enumerate(鞄々)
          if all(数 <= 目的.get(色, 0) for 色, 数 in 鞄.items())))
print(sum(map(鞄の力, 鞄々)))

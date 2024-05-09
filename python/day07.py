from functools import cmp_to_key

with open("input07.txt", "r") as fp:
    lines = [line.strip() for line in fp]

def eval_hand(s):
    counts = [s.count(c) for c in s]
    if max(counts) == 5:
        return 6
    elif max(counts) == 4:
        return 5
    elif (3 in counts) and (2 in counts):
        return 4
    elif max(counts) == 3:
        return 3
    elif counts.count(2) == 4:
        return 2
    elif max(counts) == 2:
        return 1
    else:
        return 0

def eval_hand_joker(s):
    joker_opts = '23456789TQKA'
    max_eval = 0
    for opt in joker_opts:
        new_s = s.replace('J', opt)
        tmp = eval_hand(new_s)
        if tmp > max_eval:
            max_eval = tmp
    return max_eval

def is_better_than(a, b):
    if type(a) == tuple:
        a = a[0]
    if type(b) == tuple:
        b = b[0]
    # returns whether a > b
    atype = eval_hand(a)
    btype = eval_hand(b)
    if atype > btype:
        return 1
    elif atype < btype:
        return -1
    else: # atype == btype
        ranks = {'A': 14,
                 'K': 13,
                 'Q': 12,
                 'J': 11,
                 'T': 10}
        ranks.update({str(x): x for x in range(2, 10)})
        for i in range(5):
            a_rank = ranks[a[i]]
            b_rank = ranks[b[i]]
            if a_rank > b_rank:
                return 1
            elif a_rank < b_rank:
                return -1
    return 0

def is_better_than_joker(a, b):
    if type(a) == tuple:
        a = a[0]
    if type(b) == tuple:
        b = b[0]
    # returns whether a > b
    atype = eval_hand_joker(a)
    btype = eval_hand_joker(b)
    if atype > btype:
        return 1
    elif atype < btype:
        return -1
    else: # atype == btype
        ranks = {'A': 14,
                 'K': 13,
                 'Q': 12,
                 'J': 1,
                 'T': 10}
        ranks.update({str(x): x for x in range(2, 10)})
        for i in range(5):
            a_rank = ranks[a[i]]
            b_rank = ranks[b[i]]
            if a_rank > b_rank:
                return 1
            elif a_rank < b_rank:
                return -1
    return 0
            
# part 1
hands = []
for line in lines:
    words = line.split()
    hand = words[0]
    bid = int(words[1])
    hands.append((hand, bid))
hands.sort(key=cmp_to_key(is_better_than))
print(hands)

total = 0
for rank, pair in enumerate(hands):
    hand, bid = pair
    total += (rank + 1)*bid
print(total)

# part 2
hands.sort(key=cmp_to_key(is_better_than_joker))

total = 0
for rank, pair in enumerate(hands):
    hand, bid = pair
    total += (rank + 1)*bid
print(total)


from math import gcd
from functools import reduce

with open("input20.txt", "r") as fp:
    lines = [line.strip() for line in fp]

lines_ = """broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a""".split("\n")

CONJUNCTION = "conjunction"
FLIPFLOP = "flipflop"

def push_button(network):
    sent_pulses = [0, 0, False]
    packet_queue = [("button", "broadcaster", 0)] # from, to, value
    while packet_queue:
        #print(packet_queue)
        new_queue = []
        while packet_queue:
            #print(packet_queue[0])
            sender, target, value = packet_queue.pop(0)
            sent_pulses[value] += 1
            if target not in network:
                if target == "rx" and value == 0:
                    return [0, 0, True]
            elif network[target]["type"] == "broadcaster":
                for t in network[target]["targets"]:
                    new_queue.append((target, t, value))
            elif network[target]["type"] == FLIPFLOP:
                if value == 0:
                    network[target]["state"] = 1 - network[target]["state"]
                    for t in network[target]["targets"]:
                        new_queue.append((target, t, network[target]["state"]))
            elif network[target]["type"] == CONJUNCTION:
                network[target]["inputs"][sender] = value
                if all((v == 1) for v in network[target]["inputs"].values()):
                    #print(target, "sent 0")
                    pulse_value = 0
                else:
                    pulse_value = 1
                for t in network[target]["targets"]:
                    new_queue.append((target, t, pulse_value))
            else:
                print('panic!!')
                return
        packet_queue = new_queue
    return sent_pulses
        

def make_network(lines):
    network = {}
    for line in lines:
        words = line.split(" -> ")
        targets = words[1].split(", ")
        if words[0] == "broadcaster":
            network[words[0]] = {
                "type": "broadcaster",
                "targets": targets
            }
        elif words[0].startswith("%"):
            # flip flop
            name = words[0].lstrip("%")
            network[name] = {
                "type": FLIPFLOP,
                "state": 0,
                "targets": targets
            }
        elif words[0].startswith("&"):
            name = words[0].lstrip("&")
            network[name] = {
                "type": CONJUNCTION,
                "inputs": {}, # initialised later
                "targets": targets
            }
    for name, item in network.items():
        for t in item["targets"]:
            if t in network and network[t]["type"] == CONJUNCTION:
                network[t]["inputs"][name] = 0
    return network

'''
def print_network_names(network, t):
    for k,v in network.items():
        if v["type"] == t:
            print(f"{k:2}", end=" ")
    print()
def print_network(network, t):
    for k, v in network.items():
        if v["type"] == t:
            if t == FLIPFLOP:
                print(f"{v['state']:2d}", end=" ")
            elif t == CONJUNCTION:
                print(f"{list(v['inputs'].values())}", end=" ")
    print()

def get_flipflop_period(lines, name):
    # assume all flipflops are [n 0s] followed by [n 1s]
    network = make_network(lines)
    presses = 0
    while network[name]['state'] != 1:
        presses += 1
        push_button(network)
    return presses * 2
'''

network = make_network(lines)

sender_to_rx = next(
    name for name, sender in network.items()
    if 'rx' in sender['targets']
)
sender_sub_1 = [
    name for name, sender in network.items()
    if sender_to_rx in sender['targets']
]
sender_sub_2 = [
    name for name, sender in network.items()
    if any(s in sender['targets'] for s in sender_sub_1)
]

# Part 1
# Warm up the wires...
totals = [0, 0]
for _ in range(1000):
    res = push_button(network)
    totals = [totals[0] + res[0], totals[1] + res[1]]
print(totals[0] * totals[1])
presses = 1000

# Part 2
# When all sub_2 inputs get set to LOW at once,
# all sub_1 inputs get set to HIGH at once,
# and this causes 0 to get sent to rx.
# Hence, find the period at which each item of sub_2 becomes LOW.

periods = {}
unknowns = set(sender_sub_2)
while unknowns:
    for sender in unknowns:
        if (all(v == 0 for v in network[sender]['inputs'].values())):
            periods[sender] = presses
    unknowns -= set(periods)
    push_button(network)
    presses += 1

print(reduce((lambda x, y: x * y // gcd(x, y)), periods.values()))

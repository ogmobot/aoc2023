import re

with open("input01.txt", "r") as fp:
    lines = fp.readlines()

# original hasty version

def firstlast(text):
    digits = [int(n) for n in text if n in '0123456789']
    return 10*digits[0] + digits[-1]

def realfl(text):
    names = {
        'zero': 0,
        'one': 1,
        'two': 2,
        'three': 3,
        'four': 4,
        'five': 5,
        'six': 6,
        'seven': 7,
        'eight': 8,
        'nine': 9,
    }
    names.update({x: int(x) for x in '0123456789'})
    #digits = [names[x] for x in re.findall("|".join(names), text)]
    # ^ Can't do this, since word names might overlap
    digits = []
    while text:
        for n in names:
            if text.startswith(n):
                digits.append(names[n])
        text = text[1:]
    return 10*digits[0] + digits[-1]

print(sum(map(firstlast, lines)))
print(sum(map(realfl, lines)))
    
# cleaned up version

def digit_getter(parse_words):
    names = {x: int(x) for x in "0123456789"}
    if parse_words:
        names.update({
            "zero":  0,
            "one":   1,
            "two":   2,
            "three": 3,
            "four":  4,
            "five":  5,
            "six":   6,
            "seven": 7,
            "eight": 8,
            "nine":  9,
        })
    pattern = f"({'|'.join(names)})"
    findl = re.compile(pattern)
    findr = re.compile(".*" + pattern)
    return (lambda text: 10*names.get(findl.search(text).group(1))
                          + names.get(findr.search(text).group(1)))

print(sum(map(digit_getter(False), lines)))
print(sum(map(digit_getter(True), lines)))

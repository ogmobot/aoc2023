fs = require "fs"

mapMaker = (lines) ->
    res = {}
    lines.forEach((line) ->
        words = line.matchAll "\\w+"
        here = words.next().value
        if here != undefined
            left = words.next().value[0]
            right = words.next().value[0]
            res[here[0]] = {"L": left, "R": right}
    )
    res

# "Luckily", all possible paths only reach a single Z location and then 
# immediately return to their respective A location. This means there's
# no need to keep track of what nodes have been seen.

gcd = (a, b) ->
    if b == 0
        a
    else
        gcd b, a % b

pathFinder = (theMap, directions) ->
    (start) ->
        timer = 0
        loc = start
        while !(loc[loc.length - 1] == "Z")
            loc = theMap[loc][directions[timer % directions.length]]
            timer += 1
        timer

main = ->
    lines = (fs.readFileSync "input08.txt", "utf8").split "\n"
    directions = lines[0]
    theMap = mapMaker lines[2..]
    pf = pathFinder theMap, directions
    console.log pf "AAA"
    cycles = (
        k for k in Object.keys theMap when k[k.length - 1] == "A"
    ).map pf
    console.log cycles.reduce (a, b) -> a * b / gcd(a, b)

main()

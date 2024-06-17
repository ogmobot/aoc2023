import "dart:io" ;

enum Parity { even, odd }
enum Feature { rock, space }

class Terrain {
    // Lookup arbitrary coordinates
    Map<(int, int), Feature> _maptemplate = Map();
    (int, int) start = (-1, -1);
    int _width = 0;
    int _height = 0;

    Terrain(List<String> lines) {
        _maptemplate.clear();
        for (final (r, line) in lines.indexed) {
            _width = (line.length > _width ? line.length : _width);
            _height++;
            for (var c = 0; c < line.length; c++) {
                var ch = line.substring(c, c + 1);
                _maptemplate[(r, c)] = (
                    ch == "#" ? Feature.rock : Feature.space);
                if (ch == "S") {
                    start = (r, c);
                }
            }
        }
    }

   Iterable<(int, int)> get_adjacent((int, int) coord) {
        var (row, col) = coord;
        return [
            (row    , col + 1),
            (row + 1, col    ),
            (row    , col - 1),
            (row - 1, col    )
        ].where((adj) => (this[adj] == Feature.space));
    }

    Feature operator[]((int, int) coord) {
        var (row, col) = coord;
        // The result of % operator is always non-negative
        return _maptemplate[((row % _height), (col % _width))]
            as Feature; // This should never be null!
    }
}

Map<(int, int), Parity> stepOut(var terrain, int step_limit) {
    //terrain[(0, 0)] = "foo";
    Map<(int, int), Parity> steps = Map();
    Set<(int, int)> front = Set();
    front.add(terrain.start);
    var timer = 0;
    while (timer <= step_limit) {
        Set<(int, int)> new_front = Set();
        front.forEach((coord) {
            if (steps[coord] != null) { return; }
            steps[coord] = ((timer % 2 == 0) ? Parity.even : Parity.odd);
            new_front.addAll(terrain.get_adjacent(coord));
        });
        front = new_front;
        if (timer > 0 && (timer - 65) % 262 == 0) {
            print("Timer ${timer}, Odd:");
            print(steps.values.where((x) => x == Parity.odd).length);
            print(" even:");
            print(steps.values.where((x) => x == Parity.even).length);
        }
        timer++;
    }
    return steps;
}

void main() {
    List<String> input = File("input21.txt")
        .readAsLinesSync();
    var terrain = Terrain(input);
    for (var line in input) {
        //terrain[(0, 0)] = line;
    }
    const PART_1 = 64;
    var steps = stepOut(terrain, PART_1);
    print(steps.values.where((x) => x == Parity.even).length);

    const PART_2 = 26501365; // = 202300 * 131 + 65
    // Expect a quadratic pattern to appear periodically.
    // (E.g. let f(x) = number of tiles reached after (n * 131 + 65) steps
    // then find f(202300).)
    return;
}

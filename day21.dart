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

List<int> countSteps(var terrain, Iterable<int> step_counts) {
    // Result is returned in ascending order, regardless of the
    // order of step_counts
    var step_limit = step_counts.reduce((x, y) => (x > y ? x : y));
    List<int> result = [];
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
        if (step_counts.contains(timer)) {
            result.add(steps.values
                .where((x) =>
                    (x == (timer % 2 == 0 ? Parity.even : Parity.odd)))
                .length
            );
        }
        front = new_front;
        timer++;
    }
    return result;
}

Function get_parabola(List<double> xs, List<double> ys) {
    var (x0, x1, x2) = (xs[0], xs[1], xs[2]);
    var (y0, y1, y2) = (ys[0], ys[1], ys[2]);
    // Found this formula for a on the internet, because figuring it out
    // by hand was getting tedious
    // (If x values aren't unique, this will divide by zero)
    var a = (
        ((y0 - y1) * (x0 - x2)) - ((y0 - y2) * (x0 - x1))
    ) / (
        (
            ((x0 * x0) - (x1 * x1)) * (x0 - x2)
        ) - (
            ((x0 * x0) - (x2 * x2)) * (x0 - x1)
        )
    );
    var b = ((y0 - y1) / (x0 - x1)) - (a * (x0 + x1));
    var c = y0 - (a * x0 * x0) - (b * x0);
    return ((x) => ((a * x * x) + (b * x) + c).toInt());
}

void main() {
    List<String> input = File("input21.txt")
        .readAsLinesSync();
    var terrain = Terrain(input);
    const PART_1 = 64;
    const PART_2 = 26501365; // = 202300 * 131 + 65

    print(countSteps(terrain, [PART_1]).first);

    // Expect a quadratic pattern to appear periodically.
    // (xs must be in ascending order!)
    const xs = [
        (0 * 131) + 65,
        (1 * 131) + 65,
        (2 * 131) + 65
    ];
    var ys = countSteps(terrain, xs);
    var f = get_parabola(
        xs.map((x) => x.toDouble()).toList(),
        ys.map((y) => y.toDouble()).toList()
    );
    print(f(PART_2));
    return;
}

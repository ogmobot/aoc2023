Object subclass: GridFeature [
    | row col s isNum |
    adjacentInGrid: grid [
        "TODO: find abutting grid features"
    ]
]

Object subclass: Grid [
    | text features |
    setText: data [
        text := data.
        features := [].
    ]
    parseFeatures [
        | index row col |
        index := 0.
        row := 0.
        col := 0.
        [
            "TODO: get substring at index"
            "TODO: create relevant features"
            "TODO: update index, row, col"
            "TODO: end with boolean for loop"
            false.
        ] whileTrue.
    ]
]

| inFile inText theGrid |
inFile := FileStream open: 'input03.txt' mode: FileStream read.
inText := inFile contents.
inFile finalize.

theGrid := Grid new.
theGrid setText: inText.
theGrid parseFeatures.

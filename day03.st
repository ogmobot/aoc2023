Object subclass: Feature [
    | row col s |
    setRow: r [ row := r. ]
    setCol: c [ col := c. ]
    setS: str [ s := str. ]
    adjacentInGrid: features [
        "TODO: find abutting grid features"
    ]
    getAdjNums: features [
        "TODO: filter nums"
        #('*')
    ]
    getAdjSymbs: features [
        "TODO: filter non-nums"
    ]
    isAtRow: r andAtCol: c [
        (c = col) & (r >= row) & (r < ((s size) + row)).
    ]
    isGear [ s = '*'. ]
    getNum [
        | res |
        res := 0.
        ((s byteAt: 1) asCharacter isDigit) ifTrue: [ res := s asNumber. ]
        res.
    ]
]

Object subclass: Grid [
    | rows features |
    setText: data [
        rows := data lines.
        features := Bag new.
    ]
    parseFeatures [
        | index symb feature partNum |
        rows keysAndValuesDo: [ :row :line |
            partNum := ''.
            "Append a . to ensure numbers end"
            (line , '.') keysAndValuesDo: [ :col :gotCh |
                symb := gotCh asCharacter.
                (symb isDigit) ifTrue: [
                    partNum := partNum , symb.
                ].
                (((symb isDigit) not) & ((partNum size) > 0)) ifTrue: [
                    feature := Feature new.
                    feature setRow: row.
                    feature setCol: (col - (partNum size)).
                    feature setS: partNum.
                    features add: feature.
                    partNum := ''.
                ].
                ((symb ~= $.) & ((symb isDigit) not)) ifTrue: [
                    feature := Feature new.
                    feature setRow: row.
                    feature setCol: col.
                    feature setS: symb asString.
                    features add: feature.
                ].
            ].
        ].
    ]
    solveP1 [
        | total adjSymbs numVal |
        total := 0.
        features do: [ :feature |
            numVal := feature getNum.
            (numVal > 0) ifTrue: [
                adjSymbs := feature getAdjSymbs: features.
                ((adjSymbs size) > 0) ifTrue: [
                    total := total + numVal.
                ].
            ].
        ].
        total printNl.
    ]
]

| inFile inText theGrid |
inFile := FileStream open: 'input03.txt' mode: FileStream read.
inText := inFile contents.
inFile finalize.

theGrid := Grid new.
theGrid setText: inText.
theGrid parseFeatures.
theGrid solveP1.

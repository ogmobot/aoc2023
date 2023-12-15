Object subclass: Feature [
    | row col s valid adjNums |
    setRow: r [ row := r. ]
    setCol: c [ col := c. ]
    setS: str [ s := str. valid := false. ]
    setValid [ valid := true. ]
    isGear [ ^(s = '*') ]
    isValid [ ^valid ]

    init: features [
        adjNums := Set new.
        (self getNum = 0) ifTrue: [
            #(-1 0 1) do: [ :r |
                #(-1 0 1) do: [ :c |
                    features do: [ :feature |
                        ((feature getNum > 0)
                        & (feature isAtRow: (row + r)
                                   andAtCol: (col + c))) ifTrue: [
                            adjNums add: feature.
                            feature setValid.
                        ].
                    ].
                ].
            ].
        ].
    ]
    isAtRow: r andAtCol: c [
        ^((r = row) & (c >= col) & (c < ((s size) + col)))
    ]
    getNum [
        ((s valueAt: 1) asCharacter isDigit) ifTrue: [
            ^(s asNumber)
        ] ifFalse: [
            ^0
        ].
    ]
    gearValue [
        "Only do this if I'm a gear"
        (s = '*') ifFalse: [ ^0 ].
        (adjNums size = 2) ifFalse: [ ^0 ].
        ^(adjNums inject: 1 into: [ :product :feature |
            product * (feature getNum)
        ])
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
                    partNum := (partNum , (symb asString)).
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
                    feature setS: (symb asString).
                    features add: feature.
                ].
            ].
        ].
        features do: [ :feature | feature init: features ].
    ]
    solveP1 [
        ^(features inject: 0 into: [ :total :feature |
            total + ((feature isValid) ifTrue: [
                feature getNum.
            ] ifFalse: [
                0.
            ])
        ])
    ]
    solveP2 [
        ^(features inject: 0 into: [ :total :feature |
            total + (feature gearValue)
        ])
    ]
]

| inFile inText theGrid |
inFile := FileStream open: 'input03.txt' mode: FileStream read.
inText := inFile contents.
inFile finalize.

theGrid := Grid new.
theGrid setText: inText.
theGrid parseFeatures.
"Takes ~3 minutes"
theGrid solveP1 printNl.
theGrid solveP2 printNl.

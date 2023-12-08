counter:{{#x}'=x} / divide hand into groups
pad5:{x,(5-#x)#0} / ensure group set is 5 elements long
sortup:  {x@<x}   / sort in ascending order
sortdown:{x@>x}   / sort in descending order

cardranks:"j23456789TJQKA"?
linetohand:{{[s]((x@!s);(0$(s+1)_(x)))}x?" "}

handler:{({x@>x}pad5 counter x),(cardranks'x)}
sortindex:{<handler'{*x}'x}
/ There is probably a neater way...
solve:{+/(1+!#x)*({*|x}'x@(sortindex x))}

data:0:"input07.txt"
data_:("32T3K 765";"T55J5 684";"KK677 28";"KTJJT 220";"QQQJA 483")
hands:linetohand'data

/ part 1
`0:($solve hands),"\n"


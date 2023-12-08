counter:{{#x}'=x} / divide hand into groups
pad5:{x,(5-#x)#0} / ensure group set is 5 elements long
sortup:  {x@<x}   / sort in ascending order
sortdown:{x@>x}   / sort in descending order

cardranks:"23456789TJQK"?
linetohand:{{[s]((x@!s);(0$(s+1)_(x)))}x?" "}
p1handler:{(sortdown pad5 counter x),(cardranks'x)}

/ TODO get rank of each line with 1+>p1handler'("AAAAA";"12345")
/ then multiply each rank with the corresponding bid

data:0:"input07.txt"
hands:linetohand'data

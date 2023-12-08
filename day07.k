counter:{{#x}'=x} / divide hand into groups
pad5:{x,(5-#x)#0} / ensure group set is 5 elements long

cardranks:"j23456789TJQKA"?
linetohand:{{[s]((x@!s);(0$(s+1)_(x)))}x?" "} / to ("A2345", 123)

/ There is probably a neater way for a lot of this...

jokify:{[pair](({(x,"j")@"J"=x}'*pair);(*|pair))} / J(ack) to j(oker)

/ Rank hand by sorting rank frequency.
/ (e.g. 50000 = five-of-a-kind, 32000 = full house, 22100 = two pair)
/ (j)oker count gets added to most frequent rank.
handler:{[h]({((5-+/x)+*x),(1_ x)}{x@>x}pad5 counter{x@&~"j"=x}h),(cardranks'h)}
sortindex:{<handler'{*x}'x} / i.e. apply handler to string, then grade
solve:{+/(1+!#x)*({*|x}'x@(sortindex x))}

data:0:"input07.txt"
data_:("32T3K 765";"T55J5 684";"KK677 28";"KTJJT 220";"QQQJA 483")
hands:linetohand'data

/ part 1
`0:($solve hands),"\n"
/ part 2
`0:($solve (jokify'hands)),"\n"

\\

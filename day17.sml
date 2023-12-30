datatype Direction = horizontal | vertical ;
type RowCol = int * int ;
type SearchNode = {
    pos: RowCol,
    cost: int,
    facing: Direction
} ;
datatype 'a Leftist =
    empty
  | treeNode of 'a * 'a Leftist * 'a Leftist ;

val sum = foldl op+ 0 ;
val maxInt = Option.valOf Int.maxInt ;

fun treeRank empty = 0
  | treeRank (treeNode(_, _, r)) = treeRank r + 1 ;

fun mergeTrees (_, left,  empty) = left
  | mergeTrees (_, empty, right) = right
  | mergeTrees (lessThan, left,  right) = let
        val (treeNode(lroot, subleft, subright)) = left ;
        val (treeNode(rroot, _, _)) = right ;
    in
        if (lessThan (rroot, lroot)) then
            mergeTrees (lessThan, right, left)
        else
            let
                val subtree = mergeTrees (lessThan, right, subright) ;
            in
                if (treeRank subtree) > (treeRank subleft) then
                    treeNode(lroot, subtree, subleft)
                else
                    treeNode(lroot, subleft, subtree)
            end
    end ;

fun pushPQ (lessThan, mainTree, item) = mergeTrees (
    lessThan,
    treeNode(item, empty, empty),
    mainTree
) ;

fun lowerCost ({pos=_, cost=a, facing=_}, {pos=_, cost=b, facing=_}) =
    a < b ;

(* This isn't exhaustive -- there's no pattern for replaceAt([], _, _). *)
fun replaceAt (x :: xs, n, y) = case n of
    0 => y :: xs
  | k => x :: replaceAt (xs, k - 1, y) ;

fun gridAt (grid : 'a list list, (r, c) : RowCol) =
    List.nth ((List.nth (grid, r)), c) ;

fun setDist (
    distances: 'a list list list, (r, c) : RowCol, dIndex : int, d : 'a
) =
    replaceAt (distances, r,
        (replaceAt ((List.nth (distances, r)), c,
            (replaceAt ((gridAt (distances, (r, c))), dIndex, d))
        ))
    ) ;

fun getLines filename = let
    val inputFile = TextIO.openIn filename ;
    fun nextLine fp = case TextIO.inputLine fp of
        SOME line => line :: nextLine fp
      | NONE => [] ;
in
    nextLine inputFile before TextIO.closeIn inputFile
end ;

fun bottomRight grid = ((length grid) - 1, (length (List.hd grid)) - 1) ;

fun getAdj (
    grid : int list list,
    {pos=(thisrow, thiscol), cost=thiscost, facing=thisfacing} : SearchNode,
    minMove : int , maxMove : int
) = let
    val moveAmounts = List.tabulate (
        1 + maxMove - minMove,
        (fn x => minMove + x)
    ) ;
    val posRanges = List.map
        (fn move => List.tabulate (move, fn x => 1 + x))
        moveAmounts ;
    val negRanges = List.map (List.map op~) posRanges;
    val mapper = case thisfacing of
        horizontal => List.map (fn m => (thisrow + m, thiscol))
      | vertical   => List.map (fn m => (thisrow, thiscol + m))
    ;
    val attemptedCoords =
        (List.map mapper posRanges) @ (List.map mapper negRanges) ;
    val (maxRow, maxCol) = bottomRight grid ;
    val allowedCoords = List.filter
        (fn coordList =>
            (List.all (fn (r, c) => (0 <= r andalso r <= maxRow
                             andalso 0 <= c andalso c <= maxCol))
                      coordList
            )
        )
        attemptedCoords ;
in
    List.map
        (fn coordList => {
            pos=List.last coordList,
            cost=thiscost + (sum
                (List.map (fn rc => gridAt (grid, rc)) coordList)
            ),
            facing=(if thisfacing = vertical then horizontal else vertical)
        })
        allowedCoords
end ;

fun findBestPath (minMove : int, maxMove : int) =
    fn (grid : int list list, start : RowCol, dest : RowCol) => let
        fun search (toSearch, distances) = case toSearch of
            empty => foldl Int.min maxInt (gridAt (distances, dest))
          | treeNode(this, qleft, qright) => let
                val q = mergeTrees (lowerCost, qleft, qright) ;
                val {pos=thispos, cost=thiscost, facing=thisfacing} = this ;
                val dIndex = case thisfacing of
                    horizontal => 0
                  | vertical   => 1 ;
                val currBest = List.nth (
                    (gridAt (distances, thispos)), dIndex
                ) ;
            in
                if (currBest <= thiscost) then
                    (* Drop this node *)
                    search (q, distances)
                else
                    let
                        val newDists =
                            setDist (distances, thispos, dIndex, thiscost) ;
                        val adjs = getAdj (grid, this, minMove, maxMove) ;
                        val qAdjs = (foldl
                            (fn (item, tree) => mergeTrees (
                                lowerCost,
                                tree,
                                treeNode(item, empty, empty)))
                            q
                            adjs
                        ) ;
                    in
                        search (qAdjs, newDists)
                    end
            end ;
        val initToSearch = treeNode(
            {pos=start, cost=0, facing=horizontal},
            (treeNode({pos=start, cost=0, facing=vertical}, empty, empty)),
            empty
        ) ;
    in
        search (initToSearch, (
            List.map (List.map (fn _ => [maxInt, maxInt])) grid
        ))
    end ;

fun main filename = let
    val grid =
        List.map
            (List.filter (fn x => 0 <= x andalso x <= 9)
                o List.map (fn c => Char.ord c - Char.ord #"0")
                o String.explode)
            (getLines filename) ;
    val params = (grid, (0, 0), (bottomRight grid)) ;
    val part1 = findBestPath (1,  3) params ;
    val part2 = findBestPath (4, 10) params ;
in
    (* Takes ~16s *)
    Format.formatf "%d\n" print [Format.INT part1] ;
    Format.formatf "%d\n" print [Format.INT part2]
end ;

main "input17.txt" ;

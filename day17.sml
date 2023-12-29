datatype Direction = horizontal | vertical ;
type RowCol = int * int ;
type SearchNode = {
    pos: RowCol,
    cost: int,
    facing: Direction
} ;

fun getLines filename = let
    val inputFile = TextIO.openIn filename ;
    fun nextLine fp = case TextIO.inputLine fp of
        SOME line => line :: nextLine fp
      | NONE => [] ;
in
    nextLine inputFile before TextIO.closeIn inputFile
end ;

fun bottomRight grid = ((length grid), (length (List.hd grid))) ;

fun getAdj (
    grid : int list list,
    {pos=thispos, cost=thiscost, facing=thisfacing} : SearchNode,
    minMove : int , maxMove : int
) = let
    (* TODO use List.tabulate (n, f) or similar
        to get the indices needed for grid lookup.
        E.g.,
            [(r, c+1)],
            [(r, c+1), (r, c+2)],
            [(r, c+1), (r, c+2), (r, c+3)]
    *)
    val (maxRow, maxCol) = bottomRight grid ;
in
    case thisfacing of
        (* TODO use range calculated above *)
        (* (maybe this case statement should be up there?) *)
        horizontal => []
      | vertical   => []
end ;

fun findBestPath (minMove : int, maxMove : int) =
    (* Currently uses DFS *)
    (* TODO use priority queue*)
    fn (grid : int list list, start : RowCol, dest : RowCol) => let
        fun search (toSearch, best) = case toSearch of
            [] => best
          | {pos=thispos, cost=thiscost, facing=thisfacing} :: q => let
            in
                if (thiscost >= best) then
                    (* Drop this node *)
                    search (q, best)
                else
                    if (thispos = dest) then
                        search (q, thiscost)
                    else
                        let
                            val adjs = getAdj (
                                grid, {
                                    pos=thispos,
                                    cost=thiscost,
                                    facing=thisfacing
                                },
                                minMove, maxMove
                            ) ;
                        in
                            search (List.concat [adjs, q], best)
                        end
            end
    in
        search ([
            {pos=start, cost=0, facing=horizontal},
            {pos=start, cost=0, facing=horizontal}
        ], Option.valOf Int.maxInt)
    end ;

fun main filename = let
    val grid =
        List.map
            (List.filter (fn x => 0 <= x andalso x <= 9)
                o List.map (fn c => Char.ord c - Char.ord #"0")
                o String.explode)
            (getLines filename) ;
    val part1 = findBestPath (1, 3) (grid, (0, 0), (bottomRight grid)) ;
    val part2 = findBestPath (4, 10) (grid, (0, 0), (bottomRight grid)) ;
in
    Format.formatf "%d\n" print [Format.INT part1] ;
    Format.formatf "%d\n" print [Format.INT part2]
end ;

main "input17.txt" ;

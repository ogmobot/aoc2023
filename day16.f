' noop is prompt

( 10 constant GRID_SIZE ) ( testing - expect 46 )
 110 constant GRID_SIZE ( but *in memory*, each row is 111 bytes long )
    GRID_SIZE dup 1 + * constant FILE_BUFFER_SIZE
  -1 constant SENTINEL
( Low nibble of grid cell keeps symbol )
   0 constant S_.
   1 constant S_backslash
   2 constant S_/
   3 constant S_-
   4 constant S_|
   5 constant S_newline ( This instantly kills the beam )
( High nibble of grid cell keeps direction flags )
  16 constant D_<
  32 constant D_>
  64 constant D_^
 128 constant D_v
( Grid needs additional top/bottom rows for border, )
( plus two corner tiles for beginning and SENTINEL )
FILE_BUFFER_SIZE GRID_SIZE dup + + 2 + constant GRID_BUFFER_SIZE
variable GRID GRID_BUFFER_SIZE allot

: mirror/ ( dir coord -- dir' coord )
    ( s" mirror/ " type )
    >r
    dup D_< = if
        drop D_v
    else
        dup D_> = if
            drop D_^
        else
            D_^ = if
                D_>
            else ( D_v )
                D_<
            then
        then
    then
    r>
;
: mirrorB ( dir coord -- dir' coord )
    ( s" mirrorB " type )
    >r
    dup D_< = if
        drop D_^
    else
        dup D_> = if
            drop D_v
        else
            D_^ = if
                D_<
            else ( D_v )
                D_>
            then
        then
    then
    r>
;
: splitter- ( dir coord -- dir' coord [dir" coord] )
    >r
    dup D_< = over D_> = or if
        ( Don't change direction or coord )
        ( s" [splitter-] " type )
        r>
    else
        ( s" splitter- " type )
        drop
        D_< r>
        D_> over
    then
;
: splitter| ( dir coord -- dir' coord [dir" coord] )
    >r
    dup D_^ = over D_v = or if
        ( Don't change direction or coord )
        ( s" [splitter-] " type )
        r>
    else
        ( s" splitter| " type )
        drop
        D_^ r>
        D_v over
    then
;

: interact ( dir coord symbol -- [dir' coord [dir" coord]] )
    dup S_newline = if
        drop drop drop
    else
        dup S_backslash = if
            drop mirrorB
        else
            dup S_/ = if
                drop mirror/
            else
                dup S_- = if
                    drop splitter-
                else
                    S_| = if
                        splitter|
                    else
                        ( empty space: do nothing )
                    then
                then
            then
        then
    then
;

: move-top-beam ( dir coord -- [dir' coord' [dir" coord']] )
    ( If top beam moves into a newline, it is destroyed )
    ( 2dup . . )
    >r
    dup D_> = if
        r> 1 +
    else
        dup D_< = if
            r> 1 -
        else
            dup D_^ = if
                r> GRID_SIZE 1 + -
            else
                r> GRID_SIZE 1 + +
            then
        then
    then
    ( dir coord' )
    2dup c@ and if
        ( if already seen this coordinate... )
        drop drop
    else
        2dup c@ or over c!
        dup c@ 15 and
        interact ( may destroy beam )
    then
;

: colrow>coord ( col row -- coord )
    GRID_SIZE 1 + * +
    GRID GRID_SIZE 1 + + +
;

: count-covered-tiles ( -- tile-count )
    0 ( total )
    GRID_BUFFER_SIZE 0 do
        GRID i + c@
        dup SENTINEL 255 and <> if
            dup 15 and S_newline <> if ( bitmask for low nibble )
                240 and if ( bitmask for high nibble )
                    1 +
                then
            else
                drop
            then
        else
            drop
        then
    loop
;

: trace-from-start ( dir coord -- )
    >r >r SENTINEL r> r>
    begin
        move-top-beam
        dup SENTINEL =
    until
    drop
;

: reset-grid ( -- )
    ( clears all is-visited? flags )
    GRID_BUFFER_SIZE 0 do
        GRID i + c@
        15 and
        GRID i + c!
    loop
;

: char>symb ( character -- symbol-constant )
    dup 46 = if ( . )
        drop S_.
    else
        dup 47 = if ( / )
            drop S_/
        else
            dup 92 = if ( backslash )
                drop S_backslash
            else
                dup 45 = if ( - )
                    drop S_-
                else
                    dup 124 = if ( | )
                        drop S_|
                    else
                        S_newline
                    then
                then
            then
        then
    then
;

: file>grid ( filename -- )
    ( Pads GRID with newlines, appends SENTINEL, )
    ( then loads the contents of file into GRID, )
    ( then translates each char in GRID to a 4-bit constant. )
    filename
    GRID_BUFFER_SIZE 0 do
        10 GRID i + c! ( newline )
    loop
    GRID GRID_SIZE + 1 + FILE_BUFFER_SIZE fileread
    drop ( Ignore bytes read and assume successful )
    GRID_BUFFER_SIZE 0 do
        GRID i + c@ char>symb
        GRID i + c!
    loop
    SENTINEL GRID GRID_BUFFER_SIZE + 1 - c!
;

: print-grid ( -- )
    cr
    GRID_BUFFER_SIZE 0 do 
        i GRID + c@
        dup 10 >= if
            drop 35 emit bl emit
        else
            .
        then
        i 1 + GRID_SIZE 1 + mod 0= if
            cr
        then
    loop
;

: part-1 ( -- )
    D_> -1 0 colrow>coord
    trace-from-start
    count-covered-tiles
    reset-grid
;

: part-2 ( -- )
    0
    ( from top row )
    GRID_SIZE 0 do
        D_v i -1 colrow>coord
        trace-from-start
        count-covered-tiles max
        reset-grid
    loop
    ( from bottom row )
    GRID_SIZE 0 do
        D_^ i GRID_SIZE colrow>coord
        trace-from-start
        count-covered-tiles max
        reset-grid
    loop
    ( from left column )
    GRID_SIZE 0 do
        D_> -1 i colrow>coord
        trace-from-start
        count-covered-tiles max
        reset-grid
    loop
    ( from right column )
    GRID_SIZE 0 do
        D_> GRID_SIZE i colrow>coord
        trace-from-start
        count-covered-tiles max
        reset-grid
    loop
;

" input16.txt" file>grid

part-1 . cr
part-2 . cr

bye

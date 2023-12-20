program day14;

{$mode objfpc}

uses
    SysUtils,
    Classes;

type Coord = record
    row: Integer;
    col: Integer;
end;

type StressPair = record
    key: Integer;
    value: Int64;
end;

type Platform = record
    width: Integer;
    height: Integer;
    sliders: TFPList;
    (* Hash sets are annoying to set up; so store a character array in
       addition to the list of sliders *)
    data: Array of Array of Char;
end;

function readGrid(filename: String): Platform;
var
    inFile: TextFile; buffer: String; ch: Char;
    row: Integer; col: Integer;
    coordPtr: ^Coord;
begin
    result.width := 0;
    result.sliders := TFPList.create;

    AssignFile(inFile, filename);
    reset(inFile); (* Opens the file for reading *)

    row := 0;
    while not eof(inFile) do
    begin
        if result.height <= row + 1 then
            result.height := row + 1;
            setLength(result.data, result.height, result.width);
        col := 0;
        readln(inFile, buffer);
        for ch in buffer do
        begin
            if result.width <= col + 1 then
            begin
                result.width := col + 1;
                setLength(result.data, result.height, result.width);
            end;
            result.data[row][col] := ch;
            if ch = 'O' then
            begin
                new(coordPtr);
                coordPtr^.row := row;
                coordPtr^.col := col;
                result.sliders.add(coordPtr);
            end;
            col := col + 1;
        end;
        row := row + 1;
    end;
    close(inFile);
end;

const STRESS_MOD = 200000;
function stressHash(grid: Platform): Int64;
(*
    The stress of a configuration alone is not unique enough to hash it.
    Instead, add a number derived from the column-coordinates. The actual
    stress of a configuration can be found by taking
        (stressHash(configuration) mod (STRESS_MOD)).
    Note that STRESS_MOD must be greater than the greatest possible value
    of stress. Also note that STRESS_MOD * (sum of (x^.col mod 100))
    must fit into a 64-bit integer.
*)
var x: ^Coord;
begin
    result := 0;
    for x in grid.sliders do
    begin
        result := result + grid.height - x^.row;
        result := result + (STRESS_MOD * (x^.col mod 100))
    end;
end;

function searchByKey(alist: TFPList; key: Integer): Pointer;
var item: ^StressPair;
begin
    result := nil;
    for item in alist do
        if item^.key = key then
        begin
            result := item;
            break;
        end;
end;

function searchByValue(alist: TFPList; value: Int64): Pointer;
var item: ^StressPair;
begin
    result := nil;
    for item in alist do
        if item^.value = value then
        begin
            result := item;
            break;
        end;
end;

procedure slide(grid: Platform; dr: Integer; dc: Integer);
(* Mutates grid *)
var moved: Boolean; item: ^Coord; candidate: Coord;
begin
    moved := true;
    while moved do
    begin
        moved := false;
        for item in grid.sliders do
        begin
            candidate.row := item^.row + dr;
            candidate.col := item^.col + dc;
            while (candidate.row >= 0) and (candidate.row < grid.height)
                and (candidate.col >= 0) and (candidate.col < grid.width)
                and (grid.data[candidate.row][candidate.col] = '.') do
            begin
                grid.data[item^.row][item^.col] := '.';
                grid.data[candidate.row][candidate.col] := 'O';
                item^.row := candidate.row;
                item^.col := candidate.col;
                candidate.row := candidate.row + dr;
                candidate.col := candidate.col + dc;
                moved := true;
            end;
        end;
    end;
end;

procedure spin(grid: Platform);
(* Takes ~6s for 1000 cycles *)
begin
    slide(grid, -1,  0);
    slide(grid,  0, -1);
    slide(grid,  1,  0);
    slide(grid,  0,  1);
end;

function stressAt(grid: Platform; numSpins: LongInt): LongInt;
var
    spun: LongInt; cycleLength: Integer;
    history: TFPList; pair: ^StressPair; stress: Int64;
begin
    spun := 0;
    history := TFPList.create;
    while spun < numSpins do
    begin
        stress := stressHash(grid);
        pair := searchByValue(history, stress);
        if nil <> pair then
        begin
            cycleLength := spun - pair^.key;
            pair := searchByKey(
                history,
                spun + ((numSpins - spun) mod cycleLength) - cycleLength
            );
            result := (pair^.value) mod STRESS_MOD;
            break;
        end;
        new(pair);
        pair^.key := spun;
        pair^.value := stressHash(grid);
        history.add(pair);

        spin(grid);
        spun := spun + 1;
    end;
    history.clear;
end;

var grid: Platform;
begin
    grid := readGrid('input14.txt');
    slide(grid, -1, 0);
    writeln(stressHash(grid) mod STRESS_MOD);

    writeln(stressAt(grid, 1000000000));

    grid.sliders.clear;
end.

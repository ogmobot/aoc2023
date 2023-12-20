program day14;

{$mode objfpc}

uses
    SysUtils,
    (*Contnrs,*)
    Classes;

type Coord = record
    row: Integer;
    col: Integer;
end;

type LongPair = record
    first: LongInt;
    second: LongInt;
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
                result.width := col + 1;
                setLength(result.data, result.height, result.width);
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
    result.height := row;
end;

function getStress(grid: Platform): Integer;
var x: ^Coord;
begin
    result := 0;
    for x in grid.sliders do
        result := result + grid.height - x^.row;
end;

procedure slide(grid: Platform; dr: Integer; dc: Integer);
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

procedure spinCycle(grid: Platform);
(* Takes ~6s for 1000 cycles *)
begin
    slide(grid, -1, 0);
    slide(grid, 0, -1);
    slide(grid, 1, 0);
    slide(grid, 0, 1);
end;

const NUM_CYCLES = 1000000000;

var grid: Platform;
begin
    grid := readGrid('input14.txt');
    slide(grid, -1, 0);
    writeln(getStress(grid));
end.

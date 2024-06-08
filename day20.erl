-module(day20).
-export([
    % TODO export only main and examples
    % General utility
    foreach/2,
    map/2,
    wait_for_queue/0,
    % Parsing
    read_all_lines/1,
    % Puzzle-specific utility
    flipstate/1,
    conjunct/1,
    send_all/2,
    send_n_times/3,
    connect/3,
    % Puzzle objects
    obj_broadcaster/0,
    obj_broadcaster/1,
    obj_pulsetracker/2,
    obj_flipflop/0,
    obj_flipflop/2,
    obj_conjunction/0,
    obj_conjunction/2,
    % Main function
    example_1/0,
    example_2/0,
    main/0]).

%% General utility %%

foreach(Fun, [H|T]) -> Fun(H), foreach(Fun, T);
foreach(_, []) -> ok.

map(Fun, [H|T]) -> [Fun(H)|map(Fun, T)];
map(_, []) -> [].

wait_for_queue() ->
    % This often fails (e.g. when a process has recieved a message,
    % but not yet sent a reply). Treat it as sleep(500).
    timer:sleep(500),
    {_, QueueLen} = erlang:process_info(self(), message_queue_len),
    if
        QueueLen == 0 ->
            ok;
        true ->
            wait_for_queue()
    end.

%% Parsing %%

read_from_fp(Fp) ->
    Line = file:read_line(Fp),
    case Line of
        {ok, Text} ->
            ChompText = string:chomp(Text),
            case string:is_empty(ChompText) of
                true ->
                    read_from_fp(Fp);
                false ->
                    [ChompText|read_from_fp(Fp)]
            end;
        eof -> []
    end.

read_all_lines(Filename) ->
    {ok, Fp} = file:open(Filename, [read_ahead]),
    Lines = read_from_fp(Fp),
    file:close(Fp),
    Lines.

line_to_data(Text) ->
    % "%jb -> fz\n" returns {"jb", <obj>, ["fz"]}
    {}.

%% Puzzle-specific utility %%

flipstate(lo) -> hi;
flipstate(hi) -> lo.

% conjunction sends lo iff all inputs are hi; else sends hi.
conjunct([]) -> lo;
conjunct([{_, lo}|_]) -> hi;
conjunct([{_, hi}|Parents]) -> conjunct(Parents).

update_parents(_, []) -> [];
update_parents({Signal, Who}, [{Who, _}|Rest]) ->
    [{Who, Signal}|Rest];
update_parents({Signal, Who}, [H|Rest]) ->
    [H|update_parents({Signal, Who}, Rest)].

send_all(X, Recipients) -> foreach(fun (R) -> R ! X end, Recipients).

send_n_times(_, _, 0) -> ok;
send_n_times(Target, Message, Count) ->
    Target ! Message,
    send_n_times(Target, Message, Count - 1).

connect(Parent, Child, Tracker) ->
    Parent ! {add_child, Child}, % This calls add_parent
    Parent ! {add_child, Tracker}.

build_connections(ObjMap, [], _) -> ObjMap;
build_connections(ObjMap, [{Name, _, Connections}|Rest], Tracker) ->
    foreach(
        fun (C) ->
            connect(maps:get(Name, ObjMap), maps:get(C, ObjMap), Tracker)
        end,
        Connections
    ),
    build_connections(ObjMap, Rest, Tracker).

build_obj_map(FileData, Tracker) ->
    % FileData is of the form [{name, obj, connections}, ...]
    ObjMap = maps:from_list(
        map(fun ({Name, Obj, _}) -> {Name, Obj} end, FileData)
    ),
    build_connections(ObjMap, FileData, Tracker).

%% Puzzle Objects %%
% Each object receives messages from parents
% and sends messages to children.

obj_broadcaster(Children) ->
    receive
        {lo, _} ->
            send_all({lo, self()}, Children),
            obj_broadcaster(Children);
        {hi, _} ->
            send_all({hi, self()}, Children),
            obj_broadcaster(Children);
        {add_child, Child} ->
            Child ! {add_parent, self()},
            obj_broadcaster([Child|Children])
    end.
obj_broadcaster() -> obj_broadcaster([]).

obj_pulsetracker(Lo, Hi) ->
    % Creature - Vampire Rogue
    % When Pulse Tracker attacks, each opponent loses 1 life.
    receive
        {hi, _} -> obj_pulsetracker(Lo, Hi + 1);
        {lo, _} -> obj_pulsetracker(Lo + 1, Hi);
        finish ->
            io:format("~p~n", [Lo * Hi]),
            ok
    end.

obj_flipflop(State, Children) ->
    receive
        {lo, _} ->
            % Flip state and send new state
            NewState = flipstate(State),
            send_all({NewState, self()}, Children),
            obj_flipflop(NewState, Children);
        {add_child, Child} ->
            Child ! {add_parent, self()},
            obj_flipflop(State, [Child|Children])
    end.
obj_flipflop() -> obj_flipflop(lo, []).

obj_conjunction(Children, Parents) ->
    receive
        {hi, Sender} ->
            NewParents = update_parents({hi, Sender}, Parents),
            send_all({conjunct(NewParents), self()}, Children),
            obj_conjunction(Children, NewParents);
        {lo, Sender} ->
            NewParents = update_parents({lo, Sender}, Parents),
            send_all({conjunct(NewParents), self()}, Children),
            obj_conjunction(Children, NewParents);
        {add_child, Child} ->
            Child ! {add_parent, self()},
            obj_conjunction([Child|Children], Parents);
        {add_parent, Parent} ->
            obj_conjunction(Children, [{Parent, lo}|Parents])
    end.
obj_conjunction() -> obj_conjunction([], []).

%% Main function

example_1() ->
    Tracker = spawn(day20, obj_pulsetracker, [0, 0]),
    Button = spawn(day20, obj_broadcaster, []),
    Broadcaster = spawn(day20, obj_broadcaster, []),
    A = spawn(day20, obj_flipflop, []),
    B = spawn(day20, obj_flipflop, []),
    C = spawn(day20, obj_flipflop, []),
    Inv = spawn(day20, obj_conjunction, []),
    connect(Button, Broadcaster, Tracker),
    connect(Broadcaster, A, Tracker),
    connect(Broadcaster, B, Tracker),
    connect(Broadcaster, C, Tracker),
    connect(A, B, Tracker),
    connect(B, C, Tracker),
    connect(C, Inv, Tracker),
    connect(Inv, A, Tracker),
    % Push the button
    send_n_times(Button, {lo, self()}, 1000),
    wait_for_queue(),
    Tracker ! finish.

example_2() ->
    Tracker = spawn(day20, obj_pulsetracker, [0, 0]),
    Button = spawn(day20, obj_broadcaster, []),
    ObjMap = build_obj_map([
        {"broadcaster", spawn(day20, obj_broadcaster, []), ["a","b","c"]},
        {"a", spawn(day20, obj_flipflop, []), ["b"]},
        {"b", spawn(day20, obj_flipflop, []), ["c"]},
        {"c", spawn(day20, obj_flipflop, []), ["inv"]},
        {"inv", spawn(day20, obj_conjunction, []), ["a"]}
    ], Tracker),
    connect(Button, maps:get("broadcaster", ObjMap), Tracker),
    send_n_times(Button, {lo, self()}, 1000),
    wait_for_queue(),
    Tracker ! finish.

main() ->
    % TODO
    % Parse input file
    % Set up an obj_whatever for each line
    % Set up a pulse_tracker that is a child of every object
    % (Pulse tracker must be added an additional time for additional child!)
    % Send a button-pulse to broadcaster (and pulse tracker) 1000 times
    % Set up cycle detectors for grandchildren of "rx"
    % Every few hundred presses, query the cycle detectors
    % If all of them report done, output their GCD and finish
    Lines = read_all_lines("input20.txt"),
    FileData = map(line_to_data, Lines),
    Tracker = spawn(day20, obj_pulsetracker, [0, 0]),
    ObjMap = build_obj_map(FileData, Tracker),
    Button = spawn(day20, obj_broadcaster, []),
    connect(Button, maps:get("broadcaster", ObjMap), Tracker),
    % Part 1
    send_n_times(Button, {lo, self()}, 1000),
    wait_for_queue(),
    Tracker ! finish.

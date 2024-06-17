-module(day20).
-export([
    % Puzzle objects exported so they can be spawned
    obj_counter/1,
    obj_broadcaster/0,
    obj_pulsetracker/2,
    obj_cycledetector/2,
    obj_flipflop/0,
    obj_conjunction/0,
    gcd/2,
    delayed_send/2,
    % Main function
    main/0,
    main/1]). % i.e. main([command line args])

%% General utility %%

wait_for_queue() ->
    timer:sleep(1),
    monomon ! {get_count, self()},
    receive
        0 -> ok;
        _ -> wait_for_queue()
    end.

delayed_send(Target, Signal) ->
    timer:sleep(1),
    Target ! Signal.

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

%% Parsing %%

read_from_fp(Fp) ->
    Res = file:read_line(Fp),
    case Res of
        {ok, Text} ->
            ChompText = string:chomp(Text),
            case string:is_empty(ChompText) of
                true  -> read_from_fp(Fp);
                false -> [ChompText|read_from_fp(Fp)]
            end;
        eof -> []
    end.

read_all_lines(Filename) ->
    {ok, Fp} = file:open(Filename, [read_ahead]),
    Lines = read_from_fp(Fp),
    file:close(Fp),
    Lines.

line_to_data(Line) ->
    % "%jb -> fz" returns {"jb", <obj>, ["fz"]}
    IdConns = string:split(Line, " -> "),
    Id = lists:nth(1, IdConns),
    Conns = string:split(lists:nth(2, IdConns), ", ", all),
    {Key, ObjType} = case string:prefix(Id, "%") of
        nomatch ->
            case string:prefix(Id, "&") of
                nomatch -> {Id, obj_broadcaster};
                Name -> {Name, obj_conjunction}
            end;
        Name -> {Name, obj_flipflop}
    end,
    {Key, spawn(day20, ObjType, []), Conns}.

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

send_all(X, Recipients) -> lists:foreach(fun (R) -> R ! X end, Recipients).

send_n_times(_, _, 0) -> ok;
send_n_times(Target, Message, Count) ->
    Target ! Message,
    send_n_times(Target, Message, Count - 1).

connect(Parent, Child, Tracker) ->
    Parent ! {add_child, Child}, % This calls add_parent
    Parent ! {add_child, Tracker}.

build_connections(ObjMap, [], _) -> ObjMap;
build_connections(ObjMap, [{Name, _, Connections}|Rest], Tracker) ->
    lists:foreach(
        fun (C) ->
            connect(maps:get(Name, ObjMap), maps:get(C, ObjMap), Tracker)
        end,
        Connections),
    build_connections(ObjMap, Rest, Tracker).

build_obj_map(FileData, Tracker) ->
    % FileData is of the form [{name, obj, connections}, ...]
    ObjMap = maps:from_list(
        lists:map(fun ({Name, Obj, _}) -> {Name, Obj} end, FileData)),
    ObjMapRx = maps:put("rx", spawn(day20, obj_broadcaster, []), ObjMap),
    build_connections(ObjMapRx, FileData, Tracker).

find_parents(Child, FileData) ->
    lists:filtermap(
        fun ({Name, _, Conns}) ->
            case lists:member(Child, Conns) of
                true  -> {true, Name};
                false -> false
            end
        end,
        FileData).

detect_cycles(Button, CycleDetectors) ->
    Button ! {lo, self()},
    wait_for_queue(),
    Cycles = lists:map(
        fun (Cd) ->
            Cd ! {query, self()},
            receive
                Count -> Count
            end
        end,
        CycleDetectors),
    case lists:member(0, Cycles) of
        true  -> detect_cycles(Button, CycleDetectors);
        false -> lists:foldl(fun (A, B) -> A * B div gcd(A, B) end, 1, Cycles)
    end.

obj_counter(Counter) ->
    receive
        {adj_count, Amount} -> obj_counter(Counter + Amount);
        {get_count, Sender} ->
            Sender ! Counter,
            obj_counter(Counter)
    end.

%% Puzzle Objects %%
% Each object receives messages from parents
% and sends messages to children.

obj_broadcaster(Children) ->
    receive
        {HiLo, _} when (HiLo == hi orelse HiLo == lo) ->
            monomon ! {adj_count, 1},
            send_all({HiLo, self()}, Children),
            spawn(day20, delayed_send, [monomon, {adj_count, -1}]),
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
        {finish, Sender} -> Sender ! (Lo * Hi)
    end.

obj_cycleanswer(PressCount) ->
    receive
        {query, Sender} ->
            Sender ! PressCount,
            obj_cycleanswer(PressCount)
    end.

obj_cycledetector(ButtonId, ButtonCount) ->
    receive
        {lo, Sender} ->
            if
                Sender == ButtonId ->
                    obj_cycledetector(ButtonId, ButtonCount + 1);
                true ->
                    obj_cycledetector(ButtonId, ButtonCount)
            end;
        {hi, _} ->
                %io:format("got cycle: ~p from ~p~n", [ButtonCount, Sender]),
                obj_cycleanswer(ButtonCount);
        {query, Sender} ->
            Sender ! 0,
            obj_cycledetector(ButtonId, ButtonCount)
    end.

obj_flipflop(State, Children) ->
    receive
        {lo, _} ->
            monomon ! {adj_count, 1},
            % Flip state and send new state
            NewState = flipstate(State),
            send_all({NewState, self()}, Children),
            spawn(day20, delayed_send, [monomon, {adj_count, -1}]),
            obj_flipflop(NewState, Children);
        {add_child, Child} ->
            Child ! {add_parent, self()},
            obj_flipflop(State, [Child|Children])
    end.
obj_flipflop() -> obj_flipflop(lo, []).

obj_conjunction(Children, Parents) ->
    receive
        {HiLo, Sender} when (HiLo == hi orelse HiLo == lo) ->
            monomon ! {adj_count, 1},
            NewParents = update_parents({HiLo, Sender}, Parents),
            send_all({conjunct(NewParents), self()}, Children),
            spawn(day20, delayed_send, [monomon, {adj_count, -1}]),
            obj_conjunction(Children, NewParents);
        {add_child, Child} ->
            Child ! {add_parent, self()},
            obj_conjunction([Child|Children], Parents);
        {add_parent, Parent} ->
            obj_conjunction(Children, [{Parent, lo}|Parents])
    end.
obj_conjunction() -> obj_conjunction([], []).

%% Main function

main() ->
    register(monomon, spawn(day20, obj_counter, [0])),
    % (the Watcher)
    Lines = read_all_lines("input20.txt"),
    FileData = lists:map(fun (L) -> line_to_data(L) end, Lines),
    Tracker = spawn(day20, obj_pulsetracker, [0, 0]),
    ObjMap = build_obj_map(FileData, Tracker),
    Button = spawn(day20, obj_broadcaster, []),
    connect(Button, maps:get("broadcaster", ObjMap), Tracker),
    RxGrandparents = lists:append(
        lists:map(
            fun (N) -> find_parents(N, FileData) end,
            find_parents("rx", FileData))),
    CycleDetectors = lists:map(
        fun (N) ->
            D = spawn(day20, obj_cycledetector, [Button, 0]),
            maps:get(N, ObjMap) ! {add_child, D},
            Button ! {add_child, D},
            D
        end,
        RxGrandparents),
    % Part 1
    send_n_times(Button, {lo, self()}, 1000),
    wait_for_queue(),
    Tracker ! {finish, self()},
    receive
        N -> io:format("~p~n", [N])
    end,
    % Part 2
    io:format("~p~n", [detect_cycles(Button, CycleDetectors)]),
    unregister(monomon).

main(_) -> main().

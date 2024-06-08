-module(day20).
-export([
    % Puzzle objects exported so they can be spawned
    obj_broadcaster/0,
    obj_pulsetracker/2,
    obj_cycledetector/2,
    obj_flipflop/0,
    obj_conjunction/0,
    gcd/2,
    % Main function
    main/0,
    main/1]). % i.e. main([command line args])

%% General utility %%

wait_for_queue() ->
    % This often fails (e.g. when a process has recieved a message,
    % but not yet sent a reply). Treat it as sleep(500).
    timer:sleep(500),
    {_, QueueLen} = erlang:process_info(self(), message_queue_len),
    if
        QueueLen == 0 -> ok;
        true -> wait_for_queue()
    end.

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
    timer:sleep(5), % wait for signal to propagate
    Cycles = lists:map(
        fun (Cd) ->
            Cd ! {query, self()},
            receive
                Count -> Count
            end
        end,
        CycleDetectors),
    case lists:any(fun (X) -> X == 0 end, Cycles) of
        true  -> detect_cycles(Button, CycleDetectors);
        false ->
            %io:format("~p~n", [Cycles]),
            GcdProduct = lists:foldl(
                fun (A, B) -> A * B div gcd(A, B) end, 1, Cycles),
            io:format("~p~n", [GcdProduct])
    end.

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
        finish -> io:format("~p~n", [Lo * Hi])
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

main() ->
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
    Tracker ! finish,
    % Part 2
    % (Must take long enough for Tracker to send output)
    % Cycle lengths are 3793, 3911, 3917, 3929
    wait_for_queue(),
    detect_cycles(Button, CycleDetectors).

main(_) -> main().

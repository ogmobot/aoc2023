-module(day20).
-export([
    % General utility
    foreach/2,
    % Puzzle-specific utility
    flipstate/1,
    conjunct/1,
    connect/3,
    send_to_children/2,
    % Puzzle objects
    obj_broadcaster/1,
    obj_pulsetracker/1,
    obj_flipflop/2,
    obj_conjunction/2,
    % Main function
    main/0]).

foreach(Fun, [H|T]) -> Fun(H), foreach(Fun, T);
foreach(_, []) -> ok.

flipstate(lo) -> hi;
flipstate(hi) -> lo.

conjunct([]) -> lo;
conjunct([{_, lo}|_]) -> hi;
conjunct([{_, hi}|Parents]) -> conjunct(Parents).

update_parents(_, []) -> [];
update_parents({Signal, Who}, [{Who, _}|Rest]) ->
    [{Who, Signal}|Rest];
update_parents({Signal, Who}, [H|Rest]) ->
    [H|update_parents({Signal, Who}, Rest)].

send_to_children(Signal, Children) ->
    foreach(fun (Child) -> Child ! Signal end, Children).

connect(Parent, Child, Tracker) ->
    Parent ! {add_child, Child}, % This calls add_parent
    Parent ! {add_child, Tracker}.

% Puzzle Objects
% Each object receives messages from parents
% and sends messages to children.

obj_broadcaster(Children) ->
    receive
        {lo, _} ->
            send_to_children({lo, self()}, Children),
            obj_broadcaster(Children);
        {hi, _} ->
            send_to_children({hi, self()}, Children),
            obj_broadcaster(Children);
        {add_child, Child} ->
            Child ! {add_parent, self()},
            obj_broadcaster([Child|Children]);
    end.

obj_pulsetracker(Count) ->
    % A Zendikari vampire?
    receive
        {hi, _} -> obj_pulsetracker(Count + 1);
        {lo, _} -> obj_pulsetracker(Count + 1);
        finish ->
            io:format("~p~n", [Count]),
            done
    end.

obj_flipflop(State, Children) ->
    receive
        {lo, _} ->
            % Flip state and send new state
            NewState = flipstate(State),
            send_to_children({NewState, self()}, Children),
            obj_flipflop(NewState, Children);
        {add_child, Child} ->
            Child ! {add_parent, self()},
            obj_flipflop(State, [Child|Children]);
    end.

obj_conjunction(Children, Parents) ->
    receive
        {hi, Sender} ->
            NewParents = update_parents({hi, Sender}, Parents),
            send_to_children({conjunct(NewParents), self()}, Children),
            obj_conjunction(Children, NewParents);
        {lo, Sender} ->
            NewParents = update_parents({hi, Sender}, Parents),
            send_to_children({conjunct(NewParents), self()}, Children),
            obj_conjunction(Children, NewParents);
        {add_child, Child} ->
            Child ! {add_parent, self()},
            obj_conjunction([Child|Children], Parents);
        {add_parent, Parent} ->
            obj_conjunction(Children, [{Parent, lo}|Parents])
    end.

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
    io:format("Hello, world!~n", []).

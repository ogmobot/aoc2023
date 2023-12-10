:- module day09.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, string, list, require.

% compile with --infer-modes

% There's probably a way to write this as an in-line lambda
:- pred is_zero(int::in) is semidet.
is_zero(N) :- (N = 0).

:- pred delta_map(list(int)::in, list(int)::uo) is det.
delta_map([], _) :- error("Can't find delta map of empty list").
delta_map([_], []).
delta_map([X1, X2 | Xs], [Y | Ys]) :-
    Y = X2 - X1,
    delta_map([X2 | Xs], Ys).

% Each sequence is the result of a polynomial function of degree N
:- pred poly_degree(list(int)::in, int::uo) is det.
poly_degree(Xs, N) :-
    (all_true(is_zero, Xs) ->
        N = -1
    ;
        (delta_map(Xs, Ys),
        poly_degree(Ys, N - 1))
    ).

% x0 - x1 + x2 - x3 + ...
:- pred alternating_sum(list(int)::di, int::uo) is det.
alternating_sum([], 0).
alternating_sum([X | Xs], Total) :-
    alternating_sum(Xs, Subtotal),
    Total = X - Subtotal.

% Can't find choose(n, k) in library

% Pascal's identity
:- pred choose(int, int, int).
choose(N, K, Res) :-
    ((K < 0 ; K > N) ->
        (Res = 0)
    ;
        ((K = 0 ; K = N) ->
            (Res = 1)
        ;
            (choose(N - 1, K, A),
            choose(N - 1, K - 1, B),
            Res = A + B)
        )
    ).

% Multiply x0, x1, x2, ... by (n choose 1), (n choose 2), (n choose 3), ...
% The new leftmost element is the alternating sum of these products.
:- pred get_coeffs(list(int), int, list(int), int).
get_coeffs([], _, [], _).
get_coeffs([X | Xs], N, [Y | Ys], Start) :-
    choose(N, Start, Res),
    Y = Res * X,
    get_coeffs(Xs, N, Ys, Start + 1).

:- pred get_coeffs_left(list(int), int, list(int)).
get_coeffs_left(Xs, N, Ys) :- get_coeffs(Xs, N, Ys, 1).
:- pred get_coeffs_right(list(int), int, list(int)).
get_coeffs_right(Xs, N, Ys) :- reverse(Xs, Sx), get_coeffs(Sx, N, Ys, 1).

:- pred prediction(list(int)::in, int::uo, int::uo).
prediction(Xs, L, R) :-
    poly_degree(Xs, N),
    get_coeffs_left(Xs, N + 1, Ys),
    alternating_sum(Ys, L),
    get_coeffs_right(Xs, N + 1, Zs),
    alternating_sum(Zs, R).

:- pred solve(list(list(int))::in, int::uo, int::uo) is det.
solve([], 0, 0).
solve([Xs | Xss], Part1, Part2) :-
    prediction(Xs, L, R),
    solve(Xss, Ls, Rs),
    Part1 = L + Ls,
    Part2 = R + Rs.

%:- pred numbersFromLine(string, list(int), int).
%numbersFromLine(Line, [1, 2, 3], _).
%:- pred numbersFromLine(string, list(int)).
%numbersFromLine(Line, Xs) :- numbersFromLine(Line, Xs, 0).

main(!IO) :-
    %io.write_string("Hello, world!\n", !IO),
    %poly_degree([2, 8, 18, 32], N), % N=2
    %prediction([1, 3, 5], X, Y), % X=-1 Y=7

    read_named_file_as_lines("input09.txt", FileResult, !IO),
    (FileResult = ok(Lines) ->
        %(map(numbersFromLine, Lines, Xss),
        (Xss = [[1, 2, 3], [2, 8, 18, 32]],
        solve(Xss, Part1, Part2),
        io.format("%d\n%d\n", [i(Part1), i(Part2)], !IO))
    ;
        error("Failed to read from file.")
    ).


:- module day09.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, integer, string, list, require.

% compile with --infer-modes

% There's probably a way to write this as an in-line lambda
:- pred is_zero(int::in) is semidet.
is_zero(N) :- (N = 0).

:- pred delta_map(list(int)::in, list(int)::uo) is det.
delta_map([], _) :- error("Failed to find delta map (list is empty).").
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
% (is this just foldr (-) 0?)
:- pred alternating_sum(list(int)::di, int::uo) is det.
alternating_sum([], 0).
alternating_sum([X | Xs], Total) :-
    alternating_sum(Xs, Subtotal),
    Total = X - Subtotal.

% Can't find choose(n, k) in standard library
% (Pascal's identity isn't efficient, but it's easy)
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
    solve(Xss, Rs, Ls),
    Part1 = R + Rs,
    Part2 = L + Ls.

:- pred numbersFromLine(string, list(int)).
numbersFromLine(Line, Xs) :-
    Words = words(Line),
    (map(from_string, Words, BigInts) ->
        map(to_int, BigInts, Xs)
    ;
        error("Failed to parse line.")
    ).

% map(numbersFromLine, Lines, Xss) is not working the way I expect it to
:- pred linesToSeqs(list(string), list(list(int))).
linesToSeqs([], []).
linesToSeqs([L | Ls], [Xs | Xss]) :-
    numbersFromLine(L, Xs),
    linesToSeqs(Ls, Xss).

main(!IO) :-
    read_named_file_as_lines("input09.txt", FileResult, !IO),
    (FileResult = ok(Lines) ->
        (linesToSeqs(Lines, Xss) ->
            (solve(Xss, Part1, Part2),
            io.format("%d\n%d\n", [i(Part1), i(Part2)], !IO))
        ;
            error("Failed to convert line to ints.")
        )
    ;
        error("Failed to read from file.")
    ).

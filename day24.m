1; % Starting a file with a non-function statement indicates it's not a library

% Let P be our rock's starting position, and V be its starting velocity.
% Our input data are hailstones p1, v1; p2, v2; p3, v3; ..., pn, vn
% and we know that there exists some t1, t2, t3, ..., tn such that
%   P + V(t1) == p1 + (v1)(t1)
%   P + V(t2) == p2 + (v2)(t2)
%   ...
%   P + V(tn) == pn + (vn)(tn)
%
% Our goal: develop a matrix A and a column vector B such that Ax = B,
% where x the column vector [Px Py Pz Vx Vy Vz].
%
% To find this matrix, start by rearranging the equations above:
%   (pk - P) + (vk - V)(tk) == 0 (for k = 1, 2, ..., n)
% So (pk - P) is parallel to (vk - V). This makes sense, because from our
% rock's frame of reference, every hailstone's initial displacement must
% be parallel to its velocity vector if it will eventually strike the rock.
%
% Since (pk - P) and (vk - V) are parallel,
% 0 == (pk - P) x (vk - V)
%   == (pk x vk) - (P x vk) - (pk x V) + (P x V)
% Choose two specific hailstones, i and j. Equating (P x V), we get:
%   -(pi x vi) + (P x vi) + (pi x V) == -(pj x vj) + (P x vj) + (pj x V)
% or
%   (P x vi) - (P x vj) + (pi x V) - (pj x V) == (pi x vi) - (pj x vj)
%   P x (vi - vj) + (pi - pj) x V == (pi x vi) - (pj x vj)
%
% Equating the x, y and z components of this equation gets us three
% rows of the matrix A and the column vector B. Hence, we only need
% two different pairs of (i,j) to produce a 6 x 6 matrix A and solve
% the problem.


function retval = matrix_triple_row (p1, p2, v1, v2)
    % Computes three rows of the matrix A
    % (Where Ax = B)
    dv = v1 - v2;
    dp = p1 - p2;
    % This matrix comes from the x, y and z components of
    % (P x dv) + (dp x V)
    % (so (our result) . (PV) == (P x dv) + (dp x V))
    retval = [
        % x component
           0    dv(3)      -dv(2)      0   -dp(3)   dp(2);
        % y component
       -dv(3)      0        dv(1)   dp(3)      0   -dp(1);
        % z component
        dv(2)  -dv(1)          0   -dp(2)   dp(1)      0
    ];
endfunction

function retval = result_triple (p1, p2, v1, v2)
    % Computes three entries of the column vector B
    % (Where Ax = B)
    retval = (cross(p1, v1) - cross(p2, v2))';
endfunction

function retval = solve_p2 (plist, vlist)
    a1 = matrix_triple_row(plist{1}, plist{2}, vlist{1}, vlist{2});
    a2 = matrix_triple_row(plist{1}, plist{3}, vlist{1}, vlist{3});
    A = vertcat (a1, a2);
    b1 = result_triple(plist{1}, plist{2}, vlist{1}, vlist{2});
    b2 = result_triple(plist{1}, plist{3}, vlist{1}, vlist{3});
    B = vertcat (b1, b2);
    retval = A\B;
endfunction

function main ()
    plist = {
        [19 13 30],
        [18 19 22],
        [20 25 34]
    };
    vlist = {
        [-2  1 -2],
        [-1 -1 -2],
        [-2 -2 -4]
    };
    p2 = round (solve_p2 (plist, vlist))
    disp (round (p2(1) + p2(2) + p2(3)))
endfunction

main ();

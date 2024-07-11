1; % Starting a file with a non-function statement indicates it's not a library

global LOWERBOUND = 200000000000000;
global UPPERBOUND = 400000000000000;

function [plist, vlist] = parseFile(filename)
    fp = fopen(filename);
    res = textscan (fp, "%n, %n, %n @ %n, %n, %n");
    fclose(fp);
    plist = {};
    vlist = {};
    %for i = 1:length(res)
        %plist{i} = [res{1}, res{2}, res{3}](1, :);
        %vlist{i} = [res{4}, res{5}, res{6}](1, :);
    %endfor
    plist = [res{1}, res{2}, res{3}];
    vlist = [res{4}, res{5}, res{6}];
endfunction

function retval = intersectLines (L1, L2)
    % Where each L is [x0 y0 dx dy]
    px = L1(1); py = L1(2); ux = L1(3); uy = L1(4);
    qx = L2(1); qy = L2(2); vx = L2(3); vy = L2(4);
    if (ux * vy == vx * uy)
        retval = [Inf Inf];
    else
        retval = [
            ((((ux * vy * qx) - (vx * uy * px)) - (vx * ux * (qy - py)))
            /((ux * vy) - (vx * uy)))
        ,
            ((((uy * vx * qy) - (vy * ux * py)) - (vy * uy * (qx - px)))
            /((uy * vx) - (vy * ux)))
        ];
    endif
endfunction

function retval = solvePart1 (plist, vlist)
    global LOWERBOUND;
    global UPPERBOUND;
    retval = 0;
    for i = 1:length (plist)
        p1 = plist(i, :);
        v1 = vlist(i, :);
        for j = i + 1:length (plist)
            p2 = plist(j, :);
            v2 = vlist(j, :);
            I = intersectLines(
                % Part 1 deals with x and y values only
                [p1(1) p1(2) v1(1) v1(2)],
                [p2(1) p2(2) v2(1) v2(2)]
            );
            if (LOWERBOUND <= I(1) && I(1) <= UPPERBOUND)
                if (LOWERBOUND <= I(2) && I(2) <= UPPERBOUND)
                    if ((I(1) - p1(1))/v1(1) >= 0) % time can't be < 0
                        if ((I(1) - p2(1))/v2(1) >= 0)
                            retval += 1;
                        endif
                    endif
                endif
            endif
        endfor
    endfor
endfunction

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


function retval = matrixTripleRow (p1, p2, v1, v2)
    % Computes three rows of the matrix A
    % (Where Ax = B)
    dv = v1 - v2;
    dp = p1 - p2;
    % This matrix comes from the x, y and z components of
    % (P x dv) + (dp x V)
    % (so (our result)(P|V) == (P x dv) + (dp x V))
    retval = [
        % x component
           0    dv(3)      -dv(2)      0   -dp(3)   dp(2);
        % y component
       -dv(3)      0        dv(1)   dp(3)      0   -dp(1);
        % z component
        dv(2)  -dv(1)          0   -dp(2)   dp(1)      0
    ];
endfunction

function retval = resultTriple (p1, p2, v1, v2)
    % Computes three entries of the column vector B
    % (Where Ax = B)
    retval = (cross(p1, v1) - cross(p2, v2))';
endfunction

function retval = solvePart2 (plist, vlist)
    % Only need 3 rows to solve this
    p1 = plist(1, :); p2 = plist(2, :); p3 = plist(3, :);
    v1 = vlist(1, :); v2 = vlist(2, :); v3 = vlist(3, :);

    a1 = matrixTripleRow(p1, p2, v1, v2);
    a2 = matrixTripleRow(p1, p3, v1, v3);
    A = vertcat (a1, a2);
    b1 = resultTriple(p1, p2, v1, v2);
    b2 = resultTriple(p1, p3, v1, v3);
    B = vertcat (b1, b2);
    retval = A\B; % Returns column vector x when Ax = B
endfunction

function main ()
    plist = [
        19 13 30;
        18 19 22;
        20 25 34;
        12 31 28;
        20 19 15
    ];
    vlist = [
        -2  1 -2;
        -1 -1 -2;
        -2 -2 -4;
        -1 -2 -1;
         1 -5 -3
    ];
    [plist, vlist] = parseFile ("input24.txt");
    part1 = solvePart1 (plist, vlist);
    disp (part1);
    part2 = int64 (solvePart2 (plist, vlist))
    disp (part2(1) + part2(2) + part2(3));
endfunction

% 387...002 but we get ...006 (+4)
% 371...742 but we get ...747 (+5)
% 171...512 but we get ...508 (-4)
% -220      and we get -220
% -167      and we get -167
%  214      and we get  214

main ();

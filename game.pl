:- use_module(library(lists)).
:- use_module(io).
:- use_module(fill).


black('x').
white('o').
empty_square('.').

is_black(Char) :-
    black(Char).

is_white(Char):-
    white(Char).

is_empty(Char) :-
    empty_square(Char).

is_piece(Char) :-
    is_black(Char);
    is_white(Char).

other_player(Player1, Player2) :-
    is_black(Player1), is_white(Player2);
    is_white(Player1), is_black(Player2).

row(N, Board, row( N, A, B, C, D, E, F, G, H )) :-
    between(1, 8, N),
    nth1(N, Board, [A, B, C, D, E, F, G, H]).

column(N, Board, col(N, A, B, C, D, E, F, G, H)) :-
    between(1, 8, N),
    maplist(nth1(N), Board, ColumnList),
    ColumnList = [A, B, C, D, E, F, G, H].

square(X, Y, Board, squ(X, Y, Piece)) :-
    between(1, 8, X), between(1, 8, Y),
    row(Y, Board, row(Y, A, B, C, D, E, F, G, H)),
    nth1(X, [A, B, C, D, E, F, G, H], Piece).

empty_square(X, Y, Board) :-
    square(X, Y, Board, squ(X, Y, Piece)),
    is_empty(Piece).

empty_board(Board) :-
    Board = [
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _]].


initial_board(Board) :-
   Board = [
   [E, E, E, E, E, E, E, E],
   [E, E, E, E, E, E, E, E],
   [E, E, E, E, E, E, E, E],
   [E, E, E, W, B, E, E, E],
   [E, E, E, B, W, E, E, E],
   [E, E, E, E, E, E, E, E],
   [E, E, E, E, E, E, E, E],
   [E, E, E, E, E, E, E, E]],
   is_empty(E),
   is_black(B),
   is_white(W).


% Main predicate
count_pieces(Board, BlackCount, WhiteCount) :-
    count_pieces(Board, 0, 0, BlackCount, WhiteCount).

% Helper predicate with accumulators
count_pieces([], BlackAcc, WhiteAcc, BlackAcc, WhiteAcc). % Base case: empty board
count_pieces([Row|Rows], BlackAcc, WhiteAcc, BlackCount, WhiteCount) :-
    count_row(Row, BlackAcc, WhiteAcc, NewBlackAcc, NewWhiteAcc),
    count_pieces(Rows, NewBlackAcc, NewWhiteAcc, BlackCount, WhiteCount).

% Count pieces in a single row
count_row([], BlackAcc, WhiteAcc, BlackAcc, WhiteAcc). % Base case: empty row
count_row([Piece|Pieces], BlackAcc, WhiteAcc, NewBlackAcc, NewWhiteAcc) :-
    update_counts(Piece, BlackAcc, WhiteAcc, UpdatedBlackAcc, UpdatedWhiteAcc),
    count_row(Pieces, UpdatedBlackAcc, UpdatedWhiteAcc, NewBlackAcc, NewWhiteAcc).

% Update the counts based on the piece type
update_counts(Piece, BlackAcc, WhiteAcc, BlackAcc, WhiteAcc) :-
    is_empty(Piece), !. % Do not change counts for empty squares
update_counts(Piece, BlackAcc, WhiteAcc, NewBlackAcc, WhiteAcc) :-
    is_black(Piece), !, NewBlackAcc is BlackAcc + 1.
update_counts(Piece, BlackAcc, WhiteAcc, BlackAcc, NewWhiteAcc) :-
    is_white(Piece), NewWhiteAcc is WhiteAcc + 1.

and_the_winner_is(Board, Winner) :-
    count_pieces(Board, BlackCount, WhiteCount),
    (
        BlackCount > WhiteCount, Winner = 'black';
        BlackCount < WhiteCount, Winner = 'white';
        BlackCount = WhiteCount, Winner = 'draw'
    ).

enclosing_piece(X, Y, Player, Board, U, V, N) :-
    % Check each direction individually
    (   check_direction(X, Y, Player, Board, U, V, N, 1, 0)  % Horizontal right
    ;   check_direction(X, Y, Player, Board, U, V, N, -1, 0) % Horizontal left
    ;   check_direction(X, Y, Player, Board, U, V, N, 0, 1)  % Vertical down
    ;   check_direction(X, Y, Player, Board, U, V, N, 0, -1) % Vertical up
    ;   check_direction(X, Y, Player, Board, U, V, N, 1, 1)  % Diagonal down-right
    ;   check_direction(X, Y, Player, Board, U, V, N, 1, -1) % Diagonal up-right
    ;   check_direction(X, Y, Player, Board, U, V, N, -1, 1) % Diagonal down-left
    ;   check_direction(X, Y, Player, Board, U, V, N, -1, -1)% Diagonal up-left
    ).

% check_direction(X, Y, Player, Board, U, V, N, DX, DY)
check_direction(X, Y, Player, Board, U, V, N, DX, DY) :-
    next_square(X, Y, DX, DY, NextX, NextY),
    valid_square(NextX, NextY),
    square(NextX, NextY, Board, squ(NextX, NextY, NextPiece)),
    other_player(Player, NextPiece), % Check if next piece is of the other player
    count_enclosed_pieces(NextX, NextY, Player, Board, U, V, 1, N, DX, DY).

% next_square(X, Y, DX, DY, NextX, NextY) - Calculate next square coordinates
next_square(X, Y, DX, DY, NextX, NextY) :-
    NextX is X + DX,
    NextY is Y + DY.

% valid_square(X, Y) - Check if square is within the board
valid_square(X, Y) :-
    between(1, 8, X),
    between(1, 8, Y).

% count_enclosed_pieces(X, Y, Player, Board, U, V, Count, N, DX, DY)
% Continue in the direction (DX, DY) counting enclosed pieces
count_enclosed_pieces(X, Y, Player, Board, U, V, Count, N, DX, DY) :-
    next_square(X, Y, DX, DY, NextX, NextY),
    valid_square(NextX, NextY),
    square(NextX, NextY, Board, squ(NextX, NextY, NextPiece)),
    is_piece(NextPiece),
    (   other_player(Player, NextPiece)
    ->  NewCount is Count + 1,
        count_enclosed_pieces(NextX, NextY, Player, Board, U, V, NewCount, N, DX, DY)
    ; U = NextX, V = NextY, N = Count
    ).

is_same_player(Player, Piece) :-
    is_black(Player), is_black(Piece);
    is_white(Player), is_white(Piece).

no_more_legal_squares(Board) :-
    is_white(Player), no_more_legal_squares(Player, Board).

no_more_legal_squares(Board) :-
    is_black(Player), no_more_legal_squares(Player, Board).

no_more_legal_squares(Player, Board) :-
    \+ (empty_square(X, Y, Board), enclosing_piece(X, Y, Player, Board, _, _, _)).

fill_board(Board) :-
    Board = [
    ['o', 'x', 'o', 'x', 'o', 'x', 'o', 'x'],
    ['x', 'o', 'x', 'o', 'x', 'o', 'x', 'o'],
    ['o', 'x', 'o', 'x', 'o', 'x', 'o', 'x'],
    ['x', 'o', 'x', 'x', 'x', 'o', 'x', 'o'],
    ['o', 'x', 'o', 'x', 'o', 'x', 'o', 'x'],
    ['x', 'o', 'x', 'o', 'x', 'o', 'x', 'o'],
    ['o', 'x', 'o', 'x', 'o', 'x', 'o', 'x'],
    ['x', 'o', 'x', 'o', 'x', 'o', 'x', 'o']].

% Test predicate to run all tests
test :-
    test_no_moves_available,
    test_moves_available,
    test_enclosing_piece.

% Test case where no moves are available
test_no_moves_available :-
    Board = [
    ['o', 'x', 'o', 'x', 'o', 'x', 'o', 'x'],
    ['x', 'o', 'x', 'o', 'x', 'o', 'x', 'o'],
    ['o', 'x', 'o', 'x', 'o', 'x', 'o', 'x'],
    ['x', 'o', 'x', 'x', 'x', 'o', 'x', 'o'],
    ['o', 'x', 'o', 'x', 'o', 'x', 'o', 'x'],
    ['x', 'o', 'x', 'o', 'x', 'o', 'x', 'o'],
    ['o', 'x', 'o', 'x', 'o', 'x', 'o', 'x'],
    ['x', 'o', 'x', 'o', 'x', 'o', 'x', 'o']],
    no_more_legal_squares(Board) ->
    write('test_no_moves_available: passed'), nl;
    write('test_no_moves_available: failed'), nl.

% Test case where moves are still available
test_moves_available :-
    Board =  [
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', 'o', 'x', '.', '.', '.'],
    ['.', '.', '.', 'x', 'o', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.']],
    \+ no_more_legal_squares(Board) ->
    write('test_moves_available: passed'), nl;
    write('test_moves_available: failed'), nl.

test_enclosing_piece :-
    Board =  [
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', 'o', '.', '.', '.'],
    ['.', '.', 'x', 'x', 'o', '.', '.', '.'],
    ['.', '.', '.', 'x', 'o', '.', '.', '.'],
    ['.', '.', '.', '.', 'x', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.']],
    enclosing_piece(2, 4, 'o', Board, _, _, _)  ->
    write('test_enclosing_piece: passed'), nl;
    write('test_enclosing_piece: failed'), nl.

play :-
    welcome,
    initial_board(Board),
    display_board(Board),
    is_black(Black),
    play(Black, Board).

play(Player, Board) :-
    no_more_legal_squares(Board),
    and_the_winner_is(Board, Winner),
    report_winner(Winner).

play(Player, Board) :-
    no_more_legal_squares(Board),
    count_pieces(Board, BlackCount, WhiteCount),
    BlackCount=WhiteCount,
    report_stalemate.

play(Player, Board) :-
    no_more_legal_squares(Player, Board),
    other_player(Player, OtherPlayer),
    play(OtherPlayer, Board).

play(Player, Board) :-
    fill:get_legal_move( Player, X, Y, Board ),
    fill:fill_and_flip_squares(X, Y, Player, Board, NewBoard ),
    other_player(Player, OtherPlayer),
    display_board(NewBoard),
    play(OtherPlayer, NewBoard).
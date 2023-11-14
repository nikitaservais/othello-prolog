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
    row(X, Board, row(X, A, B, C, D, E, F, G, H)),
    nth1(Y, [A, B, C, D, E, F, G, H], Piece).

empty_square(X, Y, Board) :-
    square(X, Y, Board, Piece),
    is_empty(Piece).

empty_board(Board) :-
    initial_board(Board).

initial_board(Board) :-
   EmptyRow       = ['.', '.', '.', '.', '.', '.', '.', '.'],
   UpperMiddleRow = ['.', '.', '.', 'o', 'x', '.', '.', '.'],
   LowerMiddleRow = ['.', '.', '.', 'x', 'o', '.', '.', '.'],
   Board = [EmptyRow, EmptyRow, EmptyRow, UpperMiddleRow, LowerMiddleRow, EmptyRow, EmptyRow, EmptyRow].

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

play :-
    welcome,
    initial_board(Board),
    empty_board(Board),
    display_board(Board).
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

% choose_move_corner(Player, X, Y, Board)
% Succeeds if there's a legal move in a corner.
choose_move_corner(Player, X, Y, Board) :-
    empty_square(X, Y, Board),
    corner_square(X, Y),
    enclosing_piece(X, Y, Player, Board, _, _, _).

% corner_square(X, Y)
% Succeeds if (X, Y) is a corner of the board.
corner_square(1, 1). % Top-left corner
corner_square(1, 8). % Top-right corner
corner_square(8, 1). % Bottom-left corner
corner_square(8, 8). % Bottom-right corner

% block_corner_move(Player, X, Y, Board)
% Succeeds if moving to (X, Y) blocks the opponent from taking a corner.
choose_block_corner_move(Player, X, Y, Board) :-
    other_player(Player, Opponent),
    choose_move_corner(Opponent, CornerX, CornerY, Board),
    % opposite of the enclosing_piece that would be on X, Y
    enclosing_piece(CornerX, CornerY, Opponent, Board, U, V, _),
    enclose_piece(X, Y, Player, Board, U, V).

% enclose_piece(X, Y, Player, Board, U, V)
% Succeeds if moving to (U, V) would encloses a piece at (X, Y).
enclose_piece(X, Y, Player, Board, U, V) :-
    Y=V,
    between(1, 8, X),
    enclosing_piece(X, V, Player, Board, _, V, _).

enclose_piece(X, Y, Player, Board, U, V) :-
    X=U,
    between(1, 8, Y),
    enclosing_piece(U, Y, Player, Board, U, _, _).


% adjacent_to_corner(X, Y)
% Succeeds if (X, Y) is adjacent to a corner of the board.
adjacent_to_corner(X, Y) :-
    adjacent_to(1, 1, X, Y);  % Adjacent to top-left corner
    adjacent_to(1, 8, X, Y);  % Adjacent to top-right corner
    adjacent_to(8, 1, X, Y);  % Adjacent to bottom-left corner
    adjacent_to(8, 8, X, Y).  % Adjacent to bottom-right corner

% adjacent_to(CornerX, CornerY, X, Y)
% Succeeds if (X, Y) is adjacent to the corner (CornerX, CornerY).
adjacent_to(CornerX, CornerY, X, Y) :-
    X >= CornerX - 1, X =< CornerX + 1,
    Y >= CornerY - 1, Y =< CornerY + 1,
    (X \= CornerX; Y \= CornerY). % Ensure (X, Y) is not the corner itself

% choose_move_edge(Player, X, Y, Board)
% Succeeds if there's a legal move on an edge.
choose_move_edge(Player, X, Y, Board) :-
    empty_square(X, Y, Board),
    edge_square(X, Y),
    enclosing_piece(X, Y, Player, Board, _, _, _).

% edge_square(X, Y)
% Succeeds if (X, Y) is an edge (but not corner) of the board.
edge_square(X, 1) :-
    X > 1, X < 8. % Top edge excluding corners
edge_square(X, 8) :-
    X > 1, X < 8. % Bottom edge excluding corners
edge_square(1, Y) :-
    Y > 1, Y < 8. % Left edge excluding corners
edge_square(8, Y) :-
    Y > 1, Y < 8. % Right edge excluding corners

% block_edge_move(Player, X, Y, Board)
% Succeeds if moving to (X, Y) blocks the opponent from taking an edge.
choose_block_edge_move(Player, X, Y, Board) :-
    other_player(Player, Opponent),
    choose_move_edge(Opponent, EdgeX, EdgeY, Board),
    % opposite of the enclosing_piece that would be on X, Y
    enclosing_piece(EdgeX, EdgeY, Opponent, Board, U, V, _),
    enclose_piece(X, Y, Player, Board, U, V).

% near_edge_square(X, Y)
% Succeeds if (X, Y) is adjacent to an edge, but is not an edge or corner square.
near_edge_square(X, 2) :-
    X > 1, X < 8. % One row inward from the top edge excluding corners
near_edge_square(X, 7) :-
    X > 1, X < 8. % One row inward from the bottom edge excluding corners
near_edge_square(2, Y) :-
    Y > 1, Y < 8. % One column inward from the left edge excluding corners
near_edge_square(7, Y) :-
    Y > 1, Y < 8. % One column inward from the right edge excluding corners


%choose_max_flip_move(Player, X, Y, Board) :-
%    findall(FlipCount-Pos, (empty_square(X, Y, Board),
%                            enclosing_piece(X, Y, Player, Board, _, _, FlipCount),
%                            Pos = (X, Y)),
%            Moves),
%    max_member(_-BestMove, Moves),
%    BestMove = (X, Y).

choose_max_flip_move(Player, X, Y, Board) :-
    empty_square(X, Y, Board),
    choose_max_flip_move(Player, X, Y, Board, MaxFlip).

choose_max_flip_move(Player, X, Y, Board, MaxFlip) :-
    empty_square(X, Y, Board),
    enclosing_piece(X, Y, Player, Board, _, _, FlipCount),
    MaxFlip < FlipCount,
    MaxFlip is FlipCount.


% choose_move(Player, X, Y, Board)
% Chooses a move based on the described heuristics.
choose_move(Player, X, Y, Board) :-
    choose_move_corner(Player, X, Y, Board) ;
    choose_block_corner_move(Player, X, Y, Board) ;
    choose_move_edge(Player, X, Y, Board) ;
    choose_block_edge_move(Player, X, Y, Board) ;
    choose_max_flip_move(Player, X, Y, Board).


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

/* code for section 3.6
play(Player, Board) :-
    fill:get_legal_move( Player, X, Y, Board ),
    fill:fill_and_flip_squares(X, Y, Player, Board, NewBoard ),
    other_player(Player, OtherPlayer),
    display_board(NewBoard),
    play(OtherPlayer, NewBoard).
*/

play(Player, Board) :-
    is_black(Player),
    fill:get_legal_move( Player, X, Y, Board ),
    fill:fill_and_flip_squares(X, Y, Player, Board, NewBoard ),
    other_player(Player, OtherPlayer),
    display_board(NewBoard),
    tty_clear,
    play(OtherPlayer, NewBoard).

play(Player, Board) :-
    is_white(Player),
    choose_move(Player, X, Y, Board),
    report_move(Player, X, Y),
    fill_and_flip_squares(X, Y, Player, Board, NewBoard),
    other_player(Player, OtherPlayer),
    display_board(NewBoard),
    play(OtherPlayer, NewBoard).






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
    test_choose_block_edge_move,
    test_choose_block_corner_move,
    test_choose_max_flip_move,
    test_no_moves_available,
    test_moves_available,
    test_enclosing_piece.

test_choose_block_edge_move :-
   Board =  [
      ['.', '.', '.', '.', '.', '.', '.', '.'],
      ['.', '.', '.', '.', '.', '.', '.', '.'],
      ['.', '.', 'x', 'o', 'o', 'o', 'o', '.'],
      ['.', '.', 'o', 'o', 'x', '.', '.', '.'],
      ['.', '.', '.', 'x', '.', '.', '.', '.'],
      ['.', '.', '.', '.', '.', '.', '.', '.'],
      ['.', '.', '.', '.', '.', '.', '.', '.'],
      ['.', '.', '.', '.', '.', '.', '.', '.']],
    choose_block_edge_move('o', X, Y, Board),
    (X = 3, Y = 2; X = 2, Y = 3) ->
    write('test_choose_block_edge_move: passed'), nl;
    write('test_choose_block_edge_move: failed'), nl.

test_choose_block_corner_move :-
     Board =  [
           ['.', '.', '.', '.', '.', '.', '.', '.'],
           ['.', '.', '.', '.', '.', '.', '.', '.'],
           ['.', '.', 'x', 'o', '.', '.', '.', '.'],
           ['.', 'o', 'o', 'o', 'x', '.', '.', '.'],
           ['.', '.', '.', 'x', 'o', '.', '.', '.'],
           ['.', '.', '.', '.', '.', 'o', '.', '.'],
           ['.', '.', '.', '.', '.', '.', 'o', '.'],
           ['.', '.', '.', '.', '.', '.', '.', '.']],
    choose_block_corner_move('o', X, Y, Board),
    (X = 3, Y = 2; X = 2, Y = 3) ->
    write('test_choose_block_corner_move: passed'), nl;
    write('test_choose_block_corner_move: failed'), nl.

test_choose_max_flip_move :-
   Board =  [
       ['.', '.', '.', '.', '.', '.', '.', '.'],
       ['.', '.', '.', '.', '.', '.', '.', '.'],
       ['.', '.', 'x', 'o', '.', '.', '.', '.'],
       ['.', 'o', 'o', 'o', 'x', '.', '.', '.'],
       ['.', '.', '.', 'x', 'o', '.', '.', '.'],
       ['.', '.', '.', '.', '.', 'o', '.', '.'],
       ['.', '.', '.', '.', '.', '.', 'o', '.'],
       ['.', '.', '.', '.', '.', '.', '.', '.']],
       choose_max_flip_move('x', X, Y, Board) ->
       write('test_choose_max_flip_move: passed'), nl;
       write('test_choose_max_flip_move: failed'), nl.

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

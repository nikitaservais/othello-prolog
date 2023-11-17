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
    is_black(Char).
is_piece(Char) :-
    is_white(Char).

% other_player/2
% other_player(Player1, Player2) - Succeeds if Player2 is the other Player
other_player(Player1, Player2) :-
    is_black(Player1), is_white(Player2).
other_player(Player1, Player2) :-
    is_white(Player1), is_black(Player2).

% row/3 predicate
% This predicate is used to retrieve a specific row from the board.
% N is the index of the row you want to retrieve.
% Board is the list of lists that represents the game board.
% row(N, A, B, C, D, E, F, G, H) is the structure that will hold the retrieved row.
% The first element N is the index of the row, and A, B, C, D, E, F, G, H are the elements of the row.
row(N, Board, row( N, A, B, C, D, E, F, G, H )) :-
    % Ensure that N is between 1 and 8, inclusive. This is because the game board is 8x8, so the row index should be within this range.
    between(1, 8, N),
    % Retrieve the Nth element from Board (which is a row), and unify it with the list [A, B, C, D, E, F, G, H].
    % This means that A, B, C, D, E, F, G, H will be the elements of the Nth row of the board.
    nth1(N, Board, [A, B, C, D, E, F, G, H]).

column(N, Board, col(N, A, B, C, D, E, F, G, H)) :-
    between(1, 8, N),
    maplist(nth1(N), Board, ColumnList),
    ColumnList = [A, B, C, D, E, F, G, H].

% square/4
% This predicate is used to retrieve a specific square from the board.
square(X, Y, Board, squ(X, Y, Piece)) :-
    between(1, 8, X), between(1, 8, Y),
    row(Y, Board, row(Y, A, B, C, D, E, F, G, H)),
    nth1(X, [A, B, C, D, E, F, G, H], Piece).

% empty_square/1
% Succeeds when its first two arguments are coordinates on the board (which is specified in argument 3), and the square they name is empty.
empty_square(X, Y, Board) :-
    square(X, Y, Board, squ(X, Y, Piece)),
    is_empty(Piece).

% display_board/1
% Succeeds when its argument represents the initial state of the board.
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

% empty_board/1
%  Succeeds when its argument unifies with a representation of the board with distinct variables in the places where the pieces would normally go.
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

% count_pieces/3
% Succeeds when its first argument is a board representation and its second and third arguments are the number of black and white pieces.
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
    is_empty(Piece).
update_counts(Piece, BlackAcc, WhiteAcc, NewBlackAcc, WhiteAcc) :-
    is_black(Piece), NewBlackAcc is BlackAcc + 1.
update_counts(Piece, BlackAcc, WhiteAcc, BlackAcc, NewWhiteAcc) :-
    is_white(Piece), NewWhiteAcc is WhiteAcc + 1.

% and_the_winner_is/1
% Succeeds when its first argument represents a board, and the second is a player who has won on that board. It fails if it is a draw.
and_the_winner_is(Board, Winner) :-
    count_pieces(Board, BlackCount, WhiteCount),
    BlackCount > WhiteCount,
    is_black(Winner).

and_the_winner_is(Board, Winner) :-
    count_pieces(Board, BlackCount, WhiteCount),
    BlackCount < WhiteCount,
    is_white(Winner).

% enclosing_piece/7
% Succeeds if a piece of type Player placed at (X,Y) would enclose N opponent’s pieces between itself and the piece belonging to Player at (U,V).
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

% check_direction/9
% Succeeds if a piece of type Player placed at (X,Y) would enclose N opponent’s pieces between itself and the piece belonging to Player at (U,V) in the direction DX, DY.
check_direction(X, Y, Player, Board, U, V, N, DX, DY) :-
    next_square(X, Y, DX, DY, NextX, NextY),
    valid_square(NextX, NextY),
    square(NextX, NextY, Board, squ(NextX, NextY, NextPiece)),
    other_player(Player, NextPiece), % Check if next piece is of the other player
    count_enclosed_pieces(NextX, NextY, Player, Board, U, V, 1, N, DX, DY).

% next_square(X, Y, DX, DY, NextX, NextY) - Calculate next square coordinates given a direction
next_square(X, Y, DX, DY, NextX, NextY) :-
    NextX is X + DX,
    NextY is Y + DY.

% valid_square/2
% valid_square(X, Y) - Check if square is within the board
valid_square(X, Y) :-
    between(1, 8, X),
    between(1, 8, Y).

% count_enclosed_pieces/10
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

% no_more_legal_squares/1
% Succeeds if there are no legal moves for any player
no_more_legal_squares(Board) :-
    is_black(Black),
    is_white(White),
    no_more_legal_squares(Black, Board),
    no_more_legal_squares(White, Board).

% no_more_legal_squares/2
% Succeeds if Player has no legal moves on Board
no_more_legal_squares(Player, Board) :-
    \+ (empty_square(X, Y, Board), enclosing_piece(X, Y, Player, Board, _, _, _)).

% choose_move/4
% Succeeds when it can find a move for the player named in its first ar- gument, at the (X,Y)-coordinates given in its second and third arguments, respectively.
choose_move(Player, X, Y, Board) :-
    choose_move_corner(Player, X, Y, Board) ;
    choose_block_corner_move(Player, X, Y, Board) ;
    choose_move_edge(Player, X, Y, Board) ;
    choose_block_edge_move(Player, X, Y, Board) ;
    choose_max_flip_move(Player, X, Y, Board).

% choose_move_corner/4
% Succeeds if there's a legal move in a corner.
choose_move_corner(Player, X, Y, Board) :-
    empty_square(X, Y, Board),
    corner_square(X, Y),
    enclosing_piece(X, Y, Player, Board, _, _, _).

% corner_square/2
% Succeeds if (X, Y) is a corner of the board.
corner_square(1, 1).
corner_square(1, 8).
corner_square(8, 1).
corner_square(8, 8).

% block_corner_move/4
% Succeeds if moving to (X, Y) blocks the opponent from taking a corner.
choose_block_corner_move(Player, X, Y, Board) :-
    other_player(Player, Opponent),
    choose_move_corner(Opponent, CornerX, CornerY, Board),
    enclosing_piece(CornerX, CornerY, Opponent, Board, U, V, _),
    enclose_piece(X, Y, Player, Board, U, V).

% enclose_piece/7
% Succeeds if moving to (U, V) would encloses a piece at (X, Y).
enclose_piece(X, Y, Player, Board, _, V) :-
    Y=V,
    between(1, 8, X),
    enclosing_piece(X, V, Player, Board, _, V, _).

enclose_piece(X, Y, Player, Board, U, _) :-
    X=U,
    between(1, 8, Y),
    enclosing_piece(U, Y, Player, Board, U, _, _).

% choose_move_edge/4
% Succeeds if there's a legal move on an edge.
choose_move_edge(Player, X, Y, Board) :-
    empty_square(X, Y, Board),
    edge_square(X, Y),
    enclosing_piece(X, Y, Player, Board, _, _, _).

% edge_square/2
% Succeeds if (X, Y) is an edge of the board.
edge_square(X, 1) :-
    between(1,8,X).
edge_square(X, 8) :-
    between(1,8,X).
edge_square(1, Y) :-
    between(1,8,Y).
edge_square(8, Y) :-
    between(1,8,Y).

% block_edge_move/4
% Succeeds if moving to (X, Y) blocks the opponent from taking an edge.
choose_block_edge_move(Player, X, Y, Board) :-
    other_player(Player, Opponent),
    choose_move_edge(Opponent, EdgeX, EdgeY, Board),
    enclosing_piece(EdgeX, EdgeY, Opponent, Board, U, V, _),
    enclose_piece(X, Y, Player, Board, U, V).

% choose_max_flip_move/4
% Succeeds if (X, Y) is the square that would flip the most pieces for Player.
choose_max_flip_move(Player, X, Y, Board) :-
    findall(
        FlipCount-Pos,
        (empty_square(X, Y, Board), enclosing_piece(X, Y, Player, Board, _, _, FlipCount), Pos = (X, Y)),
        Moves),
    max_member(_-BestMove, Moves),
    BestMove = (X, Y).


play :-
    welcome,
    initial_board(Board),
    display_board(Board),
    is_black(Black),
    play(Black, Board).

play(_, Board) :-
    no_more_legal_squares(Board),
    count_pieces(Board, BlackCount, WhiteCount),
    BlackCount=WhiteCount,
    report_stalemate.

play(_, Board) :-
    no_more_legal_squares(Board),
    and_the_winner_is(Board, Winner),
    report_winner(Winner).

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
    play(OtherPlayer, NewBoard).

play(Player, Board) :-
    is_white(Player),
    choose_move(Player, X, Y, Board),
    report_move(Player, X, Y),
    fill_and_flip_squares(X, Y, Player, Board, NewBoard),
    other_player(Player, OtherPlayer),
    display_board(NewBoard),
    play(OtherPlayer, NewBoard).


% Test predicate to run all tests
test :-
    test_choose_block_edge_move,
    test_choose_block_corner_move,
    test_choose_max_flip_move,
    test_no_moves_available,
    test_moves_available,
    test_enclosing_piece.

test_choose_block_edge_move :-
    is_empty(E),
    is_black(B),
    is_white(W),
    Board =  [
    [E, E, E, E, E, E, E, E],
    [E, E, E, E, E, E, E, E],
    [E, E, B, W, W, W, W, E],
    [E, E, W, W, B, E, E, E],
    [E, E, E, B, E, E, E, E],
    [E, E, E, E, E, E, E, E],
    [E, E, E, E, E, E, E, E],
    [E, E, E, E, E, E, E, E]],
    choose_block_edge_move(W, X, Y, Board),
    (X = 3, Y = 2; X = 2, Y = 3) ->
    write('test_choose_block_edge_move: passed'), nl;
    write('test_choose_block_edge_move: failed'), nl.

test_choose_block_corner_move :-
    is_empty(E),
    is_black(B),
    is_white(W),
    Board =  [
    [E, E, E, E, E, E, E, E],
    [E, E, E, E, E, E, E, E],
    [E, E, B, W, E, E, E, E],
    [E, W, W, W, B, E, E, E],
    [E, E, E, B, W, E, E, E],
    [E, E, E, E, E, W, E, E],
    [E, E, E, E, E, E, W, E],
    [E, E, E, E, E, E, E, E]],
    choose_block_corner_move(W, X, Y, Board),
    (X = 3, Y = 2; X = 2, Y = 3) ->
    write('test_choose_block_corner_move: passed'), nl;
    write('test_choose_block_corner_move: failed'), nl.

test_choose_max_flip_move :-
    is_empty(E),
    is_black(B),
    is_white(W),
    Board =  [
    [E, E, E, E, E, E, E, E],
    [E, E, E, E, E, E, E, E],
    [E, E, B, W, E, E, E, E],
    [E, W, W, W, B, E, E, E],
    [E, E, E, B, W, E, E, E],
    [E, E, E, E, E, W, E, E],
    [E, E, E, E, E, E, W, E],
    [E, E, E, E, E, E, E, E]],
    choose_max_flip_move(B, X, Y, Board),
    (X=8,Y=8)->
    write('test_choose_max_flip_move: passed'), nl;
    write('test_choose_max_flip_move: failed'), nl.

% Test case where no moves are available
test_no_moves_available :-
    is_empty(E),
    is_black(B),
    is_white(W),
    Board = [
    [W, B, W, B, W, B, W, B],
    [B, W, B, W, B, W, B, W],
    [W, B, W, B, W, B, W, B],
    [B, W, B, B, B, W, B, W],
    [W, B, W, B, W, B, W, B],
    [B, W, B, W, B, W, B, W],
    [W, B, W, B, W, B, W, B],
    [B, W, B, W, B, W, B, W]],
    no_more_legal_squares(Board) ->
    write('test_no_moves_available: passed'), nl;
    write('test_no_moves_available: failed'), nl.

% Test case where moves are still available
test_moves_available :-
    is_empty(E),
    is_black(B),
    is_white(W),
    Board =  [
      [B, B, B, B, B, B, B, B],
      [B, W, B, W, W, W, W, B],
      [B, B, B, B, B, B, B, B],
      [B, E, B, B, B, W, W, B],
      [E, E, B, B, B, B, B, B],
      [E, E, B, B, B, B, B, B],
      [E, E, E, E, E, E, E, B],
      [E, E, E, E, E, E, E, B]],
    \+ no_more_legal_squares(Board) ->
    write('test_moves_available: passed'), nl;
    write('test_moves_available: failed'), nl.

test_enclosing_piece :-
    is_empty(E),
    is_black(B),
    is_white(W),
    Board =  [
    [E, E, E, E, E, E, E, E],
    [E, E, E, E, E, E, E, E],
    [E, E, E, E, W, E, E, E],
    [E, E, B, B, W, E, E, E],
    [E, E, E, B, W, E, E, E],
    [E, E, E, E, B, E, E, E],
    [E, E, E, E, E, E, E, E],
    [E, E, E, E, E, E, E, E]],
    enclosing_piece(2, 4, W, Board, _, _, _)  ->
    write('test_enclosing_piece: passed'), nl;
    write('test_enclosing_piece: failed'), nl.

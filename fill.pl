/* abstract square filling predicate for Othello practical */

:- module( fill, [fill_and_flip_squares/5] ).

fill_and_flip_squares( X, Y, Player, Board, NewBoard ) :-
	findall( [A,B], user:enclosing_piece( X, Y, Player, Board, A, B, _ ), L ),
	fill_square( X, Y, Player, Board, MidBoard ),
	flip_lines( L, X, Y, X, Y, MidBoard, Player, NewBoard ).

flip_lines( [], _, _, _, _, Board, _, Board ).
flip_lines( [[A,B]|Rest], A, B, X, Y, BoardIn, Player, BoardOut ) :-
	flip_lines( Rest, X, Y, X, Y, BoardIn, Player, BoardOut ).
flip_lines( [[A1,B1]|Rest], A, B, X, Y, BoardIn, Player, BoardOut ) :-
	\+ ( A1 = A, B1 = B ),
	step_in_direction( A, A1, A2 ),
	step_in_direction( B, B1, B2 ),
	fill_square( A2, B2, Player, BoardIn, MidBoard ),
	flip_lines( [[A1,B1]|Rest], A2, B2, X, Y, MidBoard, Player, BoardOut ).

step_in_direction( X, X, X ).
step_in_direction( X, Y, Z ) :-
	X < Y,
	Z is X + 1.
step_in_direction( X, Y, Z ) :-
	X > Y,
	Z is X - 1.

fill_square( X, Y, Player, Board, NewBoard ) :-
	user:empty_board( NewBoard ),
	replace_in_rows( 8, X, Y, Player, Board, NewBoard ).

replace_in_rows( 0, _, _, _, _, _ ).
replace_in_rows( Row, X, Y, Player, Board, NewBoard ) :-
	Row > 0,
	replace_in_columns( 8, Row, X, Y, Player, Board, NewBoard ),
	NextRow is Row - 1,
	replace_in_rows( NextRow, X, Y, Player, Board, NewBoard ).

replace_in_columns( 0, _, _, _, _, _, _ ).
replace_in_columns( Column, Row, Column, Row, Player, Board, NewBoard ) :-
	Column > 0,
	user:square( Column, Row, NewBoard, squ( Column, Row, Player )),
	NextColumn is Column - 1,
	replace_in_columns( NextColumn, Row, Column, Row, Player,
							Board, NewBoard ).
replace_in_columns( Column, Row, X, Y, Player, Board, NewBoard ) :-
	Column > 0,
	\+ ( Column = X, Row = Y ),
	user:square( Column, Row, Board, squ( Column, Row, OldSquare )),
	user:square( Column, Row, NewBoard, squ( Column, Row, OldSquare )),
	NextColumn is Column - 1,
	replace_in_columns( NextColumn, Row, X, Y, Player, Board, NewBoard ).

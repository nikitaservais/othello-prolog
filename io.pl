/* IO module for Othello practical */

:- module( io, [display_board/1, report_move/3, report_no_move/1, welcome/0,
		get_legal_move/4, report_winner/1,
		report_stalemate/0] ).

display_board( Board ) :-
	format( '\n  12345678\n +--------+\n', [] ),
	display_rows( 1, Board ),
	format( ' +--------+\n\n', [] ).

display_rows( 9, _Board ).
display_rows( RowNumber, Board ) :-
	RowNumber > 0, RowNumber < 9,
	user:row( RowNumber, Board, Row ), 
	display_row( Row ),
	NextRowNumber is RowNumber + 1,
	display_rows( NextRowNumber, Board ).

display_row( row( N, A, B, C, D, E, F, G, H ) ) :-
	format( '~w|~w~w~w~w~w~w~w~w|\n', [N,A,B,C,D,E,F,G,H] ).

report_move( Player, X, Y ) :-
	format( 'Player ~w takes square at (~w,~w).\n', [Player,X,Y] ).

report_blocked :-
	format( '\nThat space is not available. Please try again.\n', [] ).

request_move( Player, Which, Answer ) :-
	format( 'Player ~w, please input ~w number: ', [Player,Which] ),
	ttyflush,
	read( Answer ),
	Answer > 0,
	Answer < 9,
	!.
request_move( Player, Which, Answer ) :-
	format( 'Please enter a number between 1 and 8.\n', [] ),
	request_move( Player, Which, Answer ).

report_winner( Winner ) :-
	format( 'Player ~w is the winner!\n', [Winner] ).

report_no_move( Player ) :-
	format( 'Player ~w cannot move!\n\n', [Player] ).

get_legal_move( Player, X, Y, Board ) :-
	request_move( Player, column, X ),
	request_move( Player, row, Y ),
	user:empty_square( X, Y, Board ),
	user:enclosing_piece( X, Y, Player, Board, _, _, _ ),
	!.
get_legal_move( Player, X, Y, Board ) :-
	report_blocked,
	get_legal_move( Player, X, Y, Board ).

report_stalemate :-
	format( 'Stalemate. The game is drawn.\n', [] ).

welcome :-
	format( '\nWelcome to Othello!\n', [] ).

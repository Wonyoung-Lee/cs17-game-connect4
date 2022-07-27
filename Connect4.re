open CS17SetupGame;   
open Game; 

 /* Data Definition and Examples
  
   whichPlayer is either player 1 or player 2
   type whichPlayer =
     | P1
     | P2;
 
   status shows if a player has won, it's a draw, or it's ongoing
   type status =
     | Win(whichPlayer)
         example: Win(P1)
     | Draw
     | Ongoing(whichPlayer);
         example: Ongoing(P2)
  
   the state of the game: the position, status, anything else associated
   with the game at a given turn
   status: shows whose turn it is / who won / if it's a draw
   list of list of integers: is a representation of the board
   where the inner list is the column of the board
   type state = State(status, (list(list(int))))
       example: State(Win(P1),
                 [[1, 1, 1, 1], [0, 0, 0, 2], [0, 0, 0, 2], [0, 0, 0, 2]])
  
   move describes a move that a player can make
   the integer i represents the ith list of the board (list (list (int)))
   so essentially, if a player wanted to insert a checker into
   column 4, the integer would be 3
   type move = Move(int)
       example: Move(0), Move(3), Move(18)
  
   list (float): an empty list or [a, ...alof] where a is a float value and
                 alof is a list (float)
        example: [], [0., 2., 3.67, 4.]
  
   list (int): an empty list or [a, ...alof] where a is a int value and
                 alof is a list (int)
        example: [], [0, 2, 3, -4]
 
   list (list (int)): an empty list or [a, ...alof] where a is a list (int)
                      and alof is a list (list (int))
        example: [], [[], [0, 2, 3, -4]]
 */


module Connect4 = {

    /* player 1 is P1, player 2 is P2 */
    type whichPlayer =
      | P1
      | P2;

    /* either a player has won, it's a draw, or it's ongoing */
    type status =
      | Win(whichPlayer)
      | Draw
      | Ongoing(whichPlayer);
      
    /* the state of the game: the position, status, anything else associated
    with the game at a given turn 

    status: shows whose turn it is / who won / if it's a draw
    list of list of integers: is a representation of the board
    where the inner list is the column of the board */
    type state = State(status, (list(list(int))));

    /* describes a move that a player can make
    
    the integer i represents the ith list of the board (list (list (int)))
    so essentially, if a player wanted to insert a checker into 
    column 4, the integer would be 3 */
    type move = Move(int);


    /* printing functions */
    
    /* 
    stringOfPlayer: whichPlayer -> string

    Input: wp, a whichPlayer
    Output: the string form of the input
    */
    let stringOfPlayer: whichPlayer => string = wp => 
      switch(wp){
        | P1 => "Player 1"
        | P2 => "Player 2"
      }

    /* 
    transpose: (list (list (int))) -> (list (list (int)))

    Input: brd, the board which is represented as a list (list (int))
    Output: the transposed version of the board

    1st Recursion Diagram
    OI: [[0, 0, 1, 1, 1], [0, 0, 0, 0, 2],
    [0, 0, 0, 0, 2], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
      RI: [[0, 0, 0, 0, 2], [0, 0, 0, 0, 2], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
      RO: [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], 
          [2, 2, 0, 0]]

    OO: [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [1, 0, 0, 0, 0], [1, 0, 0, 0, 0],
        [1, 2, 2, 0, 0]]


    ideation space: transforms columns list of lists into rows

    2nd Recursion Diagram
    OI: [[1, 2], [3, 4]]
      RI: [[3, 4]]
      RO: [[3], [4]]
    OO: [[1, 3], [2, 4]]
    ideation space: need to get head separately if there is one element left 
                    get tail in recursive call

    */
    let rec transpose : (list (list (int))) => (list (list (int))) = brd =>
      switch (brd) {
      | []
      | [[], ..._] => failwith("The board dimensions are incorrect.")
      | [[_], ..._] => [List.map(List.hd, brd)]
      | [[_, ..._], ..._] => [List.map(List.hd, brd), ... 
                                transpose(List.map(List.tl, brd))]
    };

    /* 
    stringOfState: state -> string 

    Input: s, a state 
    Output: the string representation of a state. It will tell the status 
            and have a representation of the board
    */
    let stringOfState: state => string = s => 
      {let rec stringList: (list (int)) => string = lst => 
        switch(lst){
          | [] => ""
          | [hd, ...tl] => string_of_int(hd) ++ " " ++ stringList(tl)
        }
      let rec stringBoard: (list (list (int))) => string = brd =>
        switch(brd){
          | [] => ""
          | [hd, ...tl] => stringList(hd) ++ "\n" ++ stringBoard(tl)
        }
      switch(s){
        | State(_, brd) =>  "Board: "
          ++ "\n" ++ stringBoard(transpose(brd))
      }}
    
    /* 
    stringOfMove: move -> string

    Input: Move(m), a move where m is the integer that represents the move
    Output: the string form og the integer m 
    */
    let stringOfMove: move => string = (Move(m)) => string_of_int(m)


    /* 
    for transforming human player input into internal representation of move

    moveOfString: string -> move

    Input: str, a string that represents a column number (2nd column = "2")
                (the input string is integer greater than 0 and less than or 
                equal to the number of columns in the board)
    Output: a move representation of the input string ("2" => Move(1))
    */
    let moveOfString: string => move = str => Move(int_of_string(str)-1)


    /* Game Logic */

    /* the state of the game when it begins 

    initialState: string -> state 

    Input: s, a string that indicates the dimension of the board (e.g. "5 7")
           the height is the first number and width is the second number 
    Output: the initial status, which is Ongoing(P1) since P1 starts first,
            and the board which is represented as a list of list of ints. 
    */
    let initialState: string => state = 
      s => {
        let boardDims = parseBoardDims(s);
        let boardHeight = getBoardHeight(boardDims);
        let boardWidth = getBoardWidth(boardDims);
        /* your initial state, using boardHeight and boardWidth, goes here */

        let rec cols : int => list(int) = height => 
          switch (height) {
            | 0 => []
            | _ => [0, ... cols(height-1)]
          };
 
        let rec rows : (int, int) => (list (list (int))) = (height, width) => 
          switch(width) {
            | 0 => []
            | _ => [cols(height), ... rows(height, width-1)]
          };
      
      State(Ongoing(P1), rows(boardHeight, boardWidth));
    }


    /* produces the list of legal moves at a state 

    legalMoves: state -> list(move)

    Input: s, the current state 
    Output: a list of moves that represent all possible legal moves
    */
    let legalMoves: state => list(move) = s => 
      {let rec findCol: (state, int) => list(move) = (State(sts, brd), mov) => 
        switch(brd){
          | [] => []
          | [hd, ...tl] => if (List.hd(hd) == 0) 
            {[Move(mov), ...findCol(State(sts, tl), (mov + 1))]}
            else {findCol(State(sts, tl), (mov + 1))};
        }
      switch(s) {
        | State(Ongoing(_), _) => findCol(s, 0)
        | State(Win(_), _) => []
        | State(Draw, _) => []
      }}

    /* returns the status of the game at the given state 

    gameStatus: state -> status 

    Input: (State(a, _)), which is a state and a is the status of the state
    Output: the status of the state
    */
    let gameStatus: state => status = (State(a, _)) => a;
    
    /* checks columns in a board to assign it a value from -100000 to 100000
    based on the positions of the pieces; the more positive, the more 
    advantageous for P1, the more negative, the more advantageous for P2
    
    input: a list of integers, which is a row/col represented by b
    output: a float, a number representing how advantageos a position is 
    for a certain player
    
    checkRow: (list (int)) => float

    Recursion Diagram
    OI: [1, 1, 1, 1]
      RI: [1, 1, 1]
      RO: 100000.
    OO: 100000.
    ideation space: finds the value of a row based on the placement of pieces

    OI: [4, 5, 6, 7, 1, 1, 1, 1]
      RI: [5, 6, 7, 1, 1, 1, 1]
      RO: 100000.
    OO: 100000.
    ideation space: finds the value of a row based on the placement of pieces

    */
    let rec checkRow: (list (int)) => float = b =>
      switch (b) {
        | [1, 1, 1, 1, ...tl] => 10000000. +. checkRow(tl)
        | [2, 2, 2, 2, ...tl] => -10000000. +. checkRow(tl)
        | [0, 1, 1, 1, 0, ...tl] => 5000. +. checkRow(tl)
        | [0, 2, 2, 2, 0, ...tl] => -5000. +. checkRow(tl)
        | [1, 1, 1, 0, ...tl] => 1000. +. checkRow(tl)
        | [2, 2, 2, 0, ...tl] => -1000. +. checkRow(tl)
        | [0, 1, 1, 1, ...tl] => 1000. +. checkRow(tl)
        | [0, 2, 2, 2, ...tl] => -1000. +. checkRow(tl)
        | [1, 0, 1, 1, ...tl] => 1000. +. checkRow(tl)
        | [2, 0, 2, 2, ...tl] => -1000. +. checkRow(tl)
        | [1, 1, 0, 1, ...tl] => 1000. +. checkRow(tl)
        | [2, 2, 0, 2, ...tl] => -1000. +. checkRow(tl)
        | [0, 0, 1, 1, 0, 0, ...tl] => 500. +. checkRow(tl)
        | [0, 0, 2, 2, 0, 0, ...tl] => -500. +. checkRow(tl)
        | [1, 1, 0, 0, ...tl] => 100. +. checkRow(tl)
        | [2, 2, 0, 0, ...tl] => -100. +. checkRow(tl)
        | [0, 0, 1, 1, ...tl] => 100. +. checkRow(tl)
        | [0, 0, 2, 2, ...tl] => -100. +. checkRow(tl)
        | [0, 1, 1, 0, ...tl] => 100. +. checkRow(tl)
        | [0, 2, 2, 0, ...tl] => -100. +. checkRow(tl)
        | [1, 0, 1, 0, ...tl] => 100. +. checkRow(tl)
        | [2, 0, 2, 0, ...tl] => -100. +. checkRow(tl)
        | [0, 0, 0, 1, 0, 0, 0, ...tl] => 10. +. checkRow(tl)
        | [0, 0, 0, 2, 0, 0, 0, ...tl] => -10. +. checkRow(tl)
        | [0, 0, 1, 0, 0, ...tl] => 1. +. checkRow(tl)
        | [0, 0, 2, 0, 0, ...tl] => -1. +. checkRow(tl)
        | [_, ...tl] => checkRow(tl)
        | [] => 0.
      }

  /*
  Input: brd, a board represented as a list of list of integers
  Output: a float value representing how advantagous the board is for player1

    checkB: (list (list (int))) => float

  1st Recursion Diagram
    OI: [[1, 1, 1, 1], [0, 1, 1, 1], [0, 1, 2, 1], [0, 0, 0, 1], [0, 0, 0, 2]]
      RI: [[0, 1, 1, 1], [0, 1, 2, 1], [0, 0, 0, 1], [0, 0, 0, 2]]  
      RO: 1000.
    OO: 10000000.
    ideation space: adds up the values of how advantageous each row is

  2nd Recursion Diagram
    OI: [[1, 1, 1, 1], [0, 1, 1, 1], [2, 0, 2, 2], [0, 0, 0, 1], [0, 0, 0, 2]]
      RI: [[0, 1, 1, 1], [2, 0, 2, 2], [0, 0, 0, 1], [0, 0, 0, 2]]
      RO: 1000. +. -1000
    OO: 100000.
    ideation space: adds up the values of how advantageous each row is

  */  
    let rec checkB: (list (list (int))) => float = brd => 
      switch(brd){
        | [] => 0. 
        | [hd, ...tl] => checkRow(hd) +. checkB(tl)
      }

      /*
       * adjusts pieces in a board in order to check for a diagonal win
       * (only checks for top right to bottom left diagonal)
       * 
       * input:
       *  an integer, beg, that represents the number of threes that will 
       *    be added to the column
       *  a board, which is a list of list of integers, represented by brd 
       *  another integer, endd, that represents the number of threes to be 
       *    added to the end of that column
       * 
       * output: a a list of lists of ints, representing a board, with the rows
       *  adjusted
       * 
       *  1st Recursion Diagram
            OI: [[1, 1, 1, 1], [0, 1, 1, 1], [0, 1, 2, 1], [0, 0, 0, 1], 
                [0, 0, 0, 2]]
              RI: [[0, 1, 1, 1], [0, 1, 2, 1], [0, 0, 0, 1], [0, 0, 0, 2]]  
              RO: [[0, 1, 1, 1, 3, 3, 3], [3, 0, 1, 2, 1, 3, 3], 
                  [3, 3, 0, 0, 0, 1, 3], [3, 3, 3, 0, 0, 0, 2]]

            OO: [[1, 1, 1, 1, 3, 3, 3, 3], [3, 0, 1, 1, 1, 3, 3, 3], 
                [3, 3, 0, 1, 2, 1, 3, 3], [3, 3, 3, 0, 0, 0, 1, 3], 
                [3, 3, 3, 3, 0, 0, 0, 2]]

            ideation space: adds threes to the board in order to look at the
              diagonal lines

          2nd Recursion Diagram
            OI: (0, [[1, 1, 1, 1], [0, 1, 1, 1], [2, 0, 2, 2], [0, 0, 0, 1]], 
                  3)
              RI: (0, ([2, 0, 2, 2], [0, 0, 0, 1], [0, 0, 0, 2]], 2)
              RO: [[2, 0, 2, 2, 3, 3], [3, 0, 0, 0, 1, 3], [3, 3, 0, 0, 0, 2]]

            OO: [[1, 1, 1, 1, 3, 3, 3], [3, 0, 1, 1, 1, 3, 3], 
                [3, 3, 2, 0, 2, 2, 3], [3, 3, 3, 0, 0, 0, 1]]

            ideation space: adds threes to the board in order to look at the
              diagonal lines
       * 
       * makeDiagonal: (list (list (int))) => (list (list (int))) 
       */
    let rec makeDiagonal: (int, (list (list (int))), int) => 
    (list (list (int))) = (beg, brd, endd) =>
      /* a helper that adds threes to the columns */
      {let rec addThrees: int => list (int) = n => 
        switch(n){
          | 0 => []
          | _ => [3, ...addThrees(n-1)]
        }
      switch(brd) {
        | [] => []
        | [hd, ...tl] => 
        [List.append(List.append(addThrees(beg), hd), addThrees(endd)), ...
                          makeDiagonal(beg+1, tl, endd-1)]
      }}

      /*
       * checks rows, columns, and diagonals to determine the value of the 
       *  inputted current board 
       * 
       * input: a list of lists of ints, representing a board, represented by
       *  brd
       * 
       * output: a float, representing the total value of the board,
       * 
       * 
       * checkFour: (list (list (int))) => float 
       * 
       */
    let checkFour: (list (list (int))) => float = b => 
      checkB(b) +. checkB(transpose(b)) +. 
        checkB(transpose(makeDiagonal(0, b, List.length(b)-1))) +.
        checkB(transpose(makeDiagonal(0, List.rev(b), List.length(b)-1)))

    /* estimates the value of a given state (static evaluation) 
    Input: s, a state 
    Output: a float number that represents how advantageous it is for a Player
            to make that move. The greater the float number, the better option 
            it is for Player 1. The smaller (or bigger negative number) the
            float, the better option it is for PLayer 2.*/
    let estimateValue: state => float = (State(_, brd)) => checkFour(brd)

    /* given a state and a legal move, yields the next state 
    Input: s, m , where s is the current state and m is a legal move 
    Output: a state with the move applied to the input state. 
    */
    let nextState: (state, move) => state = (s, m) =>
      {/* takes in whichPlayer and returns opponent */
      let opponent: whichPlayer => whichPlayer = w => 
        switch(w){
          | P1 => P2
          | P2 => P1
        }

      /* takes in a board and returns true if the board is not full */
      let rec fullBoardQ: (list (list (int))) => bool = b =>
        switch (b) {
          | [] => false
          | [hd, ...tl] => List.mem(0, hd) || fullBoardQ(tl)
        }  

      /* takes a column and swaps the bottom most 0 with the checker 
         the INPUT COLUMN IS REVERSED though. so it will iterate through the 
         column bottom first*/
      let rec putChecker: (list(int), whichPlayer) => list(int) = (lst, p) => 
        switch (lst, p) {
          | ([], P1) => [] 
          | ([], P2) => []
          | ([a, ...tl], P1) => if (a == 0) {[1, ...tl]} 
            else {[a, ...putChecker(tl, p)]};
          | ([a, ...tl], P2) => if (a == 0) {[2, ...tl]} 
            else {[a, ...putChecker(tl, p)]}
        }
    
      /* takes in board, move, and player and gives back board with appropriate
      checker inserted */
      let rec nextBoard: (list (list (int)), move, whichPlayer) => 
      (list (list (int))) = (brd, Move(m), p) =>
        switch (brd){
          | [a, ...tl] => if (m == 0) 
            {[List.rev(putChecker(List.rev(a), p)), ...tl]}
            else {[a, ...nextBoard(tl, Move(m-1), p)]};
          | _ => failwith("The move was invalid. No possible next board!")
        }

      switch(s){
        | State(Ongoing(a), brd) => 
          let newB = nextBoard(brd, m, a);
          if (checkFour(newB) >= 10000.) {State(Win(P1), newB)}
          else if (checkFour(newB) <= -10000.) {State(Win(P2), newB)}
          else if (!fullBoardQ(newB)) {State(Draw, newB)}
          else {State(Ongoing(opponent(a)), newB)}
       | _ => failwith("The game has already ended!")
      }};
  };


module MyGame : Game = Connect4;
/* or, equivalently, module MyGame = (Connect4 : Game); */
open Connect4;

  /* Test stringOfPlayer */
  checkExpect(stringOfPlayer(P1), "Player 1", "stringOfPlayer 1");
  checkExpect(stringOfPlayer(P2), "Player 2", "stringOfPlayer 2");

  /* Test stringOfState */
  checkExpect(stringOfState(initialState("4 4")), 
    "Board: \n0 0 0 0 \n0 0 0 0 \n0 0 0 0 \n0 0 0 0 \n", 
    "stringOfState 1");
  checkExpect(stringOfState(State(Ongoing(P2), 
    [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 1]])), 
    "Board: \n0 0 0 0 \n0 0 0 0 \n0 0 0 0 \n0 0 0 1 \n", "stringOfState 2");

  /* Test stringOfMove */
  checkExpect(stringOfMove(Move(5)), "5", "stringOfMove 1");
  checkExpect(stringOfMove(Move(0)), "0", "stringOfMove 2");

  /* Test moveOfString */
  checkExpect(moveOfString("5"), Move(4), "moveOfString 1");
  checkExpect(moveOfString("1"), Move(0), "moveOfString 2");
  checkExpect(moveOfString("20"), Move(19), "Column 20 = Move(19)");

  /* Test initialState */
  checkExpect(initialState("0 0"), State(Ongoing(P1), []), "0x0 board");
  checkExpect(initialState("5 0"), State(Ongoing(P1), []), "5x0 board");
  checkExpect(initialState("0 9"), State(Ongoing(P1),
    [[], [], [], [], [], [], [], [], []]), "0x9 board");
  checkExpect(initialState("5 7"), State(Ongoing(P1), 
    [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]), "5x7 board");
  checkExpect(initialState("7 5"), State(Ongoing(P1), 
    [[0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0]]), "7x5 board");
  checkExpect(initialState("4 4"), State(Ongoing(P1), 
    [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]), "4x4 board");

  /* Test legalMoves */
  checkExpect(legalMoves(State(Ongoing(P1), [])), [], "legalMoves on empty"); 
  checkExpect(legalMoves(State(Win(P1), [[0, 1, 1, 1, 1], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0]])), [], "game has been won");
  checkExpect(legalMoves(State(Win(P2), [[0, 0, 1, 1, 1], [0, 2, 2, 2, 2], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0]])), [], "game has been won");
  checkExpect(legalMoves(State(Draw, [[1, 2, 1, 1], [2, 1, 2, 2], [1, 2, 1, 2], 
    [2, 1, 1, 1]])), [], "game has been drawn");
  checkExpect(legalMoves(State(Ongoing(P1), [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0]])), [Move(0), Move(1), Move(2), Move(3), Move(4), Move(5),
     Move(6)], "legalMoves 5x7");
  checkExpect(legalMoves(State(Ongoing(P1), [[1, 2, 2, 1, 2], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]])), 
    [Move(1), Move(2), Move(3), Move(4), Move(5)], "legalMoves 5x6");

  /* Test gameStatus*/
  checkExpect(gameStatus(State(Win(P1), [[0, 1, 1, 1, 1], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0]])), Win(P1), "P1 wins");
  checkExpect(gameStatus(State(Win(P2), [[0, 0, 1, 1, 1], [0, 2, 2, 2, 2], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0]])), Win(P2), "P2 wins");
  checkExpect(gameStatus(State(Draw, [[0, 0, 1, 1, 1], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0]])), Draw, "Draw");
  checkExpect(gameStatus(initialState("4 4")), Ongoing(P1), "Ongoing P1");

  /* Test transpose */
  checkExpect(transpose([[1, 2, 3], [4, 5, 6]]), 
    [[1, 4], [2, 5], [3, 6]], "correct flip of nums");
  checkExpect(transpose([[0, 0, 1, 1, 1], [0, 0, 0, 1, 2], 
    [0, 0, 1, 2, 2], [0, 0, 0, 2, 1], [0, 0, 0, 0, 1], [0, 0, 0, 0, 2], 
    [0, 0, 0, 0, 1]]), [[0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0], 
    [1, 0, 1, 0, 0, 0, 0], [1, 1, 2, 2, 0, 0, 0], [1, 2, 2, 1, 1, 2, 1]], 
    "flip 5x7 board");
  checkExpect(transpose([[0, 0, 1, 1, 1], [0, 0, 0, 2, 1], 
    [0, 0, 1, 2, 1], [0, 0, 0, 2, 1], [0, 0, 0, 0, 2], [0, 0, 0, 0, 2], 
    [0, 0, 0, 0, 1]]), [[0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0], 
    [1, 0, 1, 0, 0, 0, 0], [1, 2, 2, 2, 0, 0, 0], [1, 1, 1, 1, 2, 2, 1]], 
    "flip winning row");
  checkError(() => transpose([]), 
    "The board dimensions are incorrect.");
  checkError(() => transpose([[], [0, 0, 0, 0, 0]]), 
    "The board dimensions are incorrect.");
  checkError(() => transpose([[], [0, 0, 0], [0, 0, 0, 0, 0]]), 
    "The board dimensions are incorrect.");

  /* Test checkRow */
  checkExpect(checkRow([1, 1, 1, 1]), 10000000., "checkRow 1");
  checkExpect(checkRow([2, 2, 2, 2]), -10000000., "checkRow 2");
  checkExpect(checkRow([1, 1, 1, 1, 0, 1, 1, 1, 1]), 10001000., "checkRow 3");
  checkExpect(checkRow([1, 1, 1, 1, 0]), 10000000., "checkRow 4");
  checkExpect(checkRow([0, 0, 0, 0]), 0., "checkRow 5");

  /* Test checkB */
  checkExpect(checkB([[1, 1, 1, 1], [0, 0, 0, 0], [0, 0, 2, 2], 
    [0, 0, 2, 2]]), (10000000. +. -100. +. -100.), "checkB 1");
  checkExpect(checkB([[0, 0, 2, 1], [0, 0, 2, 1], [0, 0, 0, 1], 
    [0, 2, 2, 1]]), 0., "checkB 2");
  checkExpect(checkB([[2, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2, 2], 
    [2, 2, 2, 2]]), (-10000000. *. 4.), "checkB 3");
  checkExpect(checkFour([[0, 0, 0, 1], [0, 0, 2, 1], [0, 0, 0, 1], 
    [0, 0, 0, 1]]), 10000000., "checkB 4");
  checkExpect(checkFour([[0, 0, 0, 1], [0, 0, 1, 3], [0, 1, 3, 3], 
    [1, 3, 3, 3]]), 10000000., "checkB 5");

  /* Test makeDiagonal */
  checkExpect(makeDiagonal(0, [], -1), [], "makeDiagonal 1");
  checkExpect(makeDiagonal(0, [[1, 4], [2, 5], [3, 6]], 2), 
    [[1, 4, 3, 3], [3, 2, 5, 3], [3, 3, 3, 6]], "makeDiagonal 2");

  /* Test checkFour */
  checkExpect(checkFour([[1, 1, 1, 1], [0, 0, 0, 0], [0, 0, 2, 2], 
    [0, 0, 2, 2]]), (10000000. +. -200.), "checkFour1");
  checkExpect(checkFour([[0, 0, 2, 1], [0, 0, 2, 1], [0, 0, 0, 1], 
    [0, 2, 2, 1]]), (10000000. +. -1000.), "checkFour2");
  checkExpect(checkFour([[2, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2, 2], 
    [2, 2, 2, 2]]), (-100000000.), "checkFour3");

  /* Test nextState */
  checkExpect(nextState(initialState("5 7"), Move(1)), 
    State(Ongoing(P2), [[0, 0, 0, 0, 0], [0, 0, 0, 0, 1], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]), 
    "nextState 1");
  checkExpect(nextState(State(Ongoing(P1), [[0, 0, 2, 1], [0, 0, 2, 1], 
    [0, 0, 0, 0], [0, 2, 2, 1]]), Move(2)), 
    State(Win(P1), [[0, 0, 2, 1],[0, 0, 2, 1], [0, 0, 0, 1], [0, 2, 2, 1]]),
    "nextState2"); 
  checkExpect(nextState(State(Ongoing(P2), [[0, 1, 2, 1], [1, 2, 2, 1], 
    [2, 1, 1, 2], [1, 2, 2, 1]]), Move(0)), State(Draw, [[2, 1, 2, 1], 
    [1, 2, 2, 1], [2, 1, 1, 2], [1, 2, 2, 1]]), "nextState3"); 
  checkExpect(nextState(initialState("5 7"), Move(1)), 
    State(Ongoing(P2), [[0, 0, 0, 0, 0], [0, 0, 0, 0, 1], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]), 
    "nextState 4");
  checkExpect(nextState(State(Ongoing(P1), 
    [[0, 0, 0, 0, 1], [0, 0, 0, 0, 2], [0, 0, 0, 0, 2], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]), 
    Move(4)), State(Ongoing(P2), 
    [[0, 0, 0, 0, 1], [0, 0, 0, 0, 2], [0, 0, 0, 0, 2], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 1], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]), 
    "nextState 5");
  checkExpect(nextState(State(Ongoing(P1), [[0, 0, 1, 1, 1], [0, 0, 0, 0, 2],
    [0, 0, 0, 0, 2], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0]]), Move(0)), State(Win(P1), [[0, 1, 1, 1, 1], 
    [0, 0, 0, 0, 2], [0, 0, 0, 0, 2], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]), "nextState 6");
  checkExpect(nextState(State(Ongoing(P2), 
    [[0, 1, 2, 1, 1], [1, 2, 1, 2, 1], 
    [2, 2, 1, 1, 2], [1, 2, 1, 1, 2], [1, 2, 1, 2, 1], [2, 1, 1, 1, 2], 
    [1, 2, 1, 2, 1]]), Move(0)), 
    State(Draw,[[2, 1, 2, 1, 1], [1, 2, 1, 2, 1], 
    [2, 2, 1, 1, 2], [1, 2, 1, 1, 2], [1, 2, 1, 2, 1], [2, 1, 1, 1, 2], 
    [1, 2, 1, 2, 1]]), 
    "nextState 7");
  checkError(() => nextState(State(Win(P1), [[1, 1, 1, 1], [0, 0, 2, 2], 
    [0, 0, 2, 2], [0, 0, 0, 0]]), Move(0)), "The game has already ended!");
    
  /* Test estimateValue */
  checkExpect(estimateValue(State(Win(P1), [[1, 1, 1, 1]])),
    10000000., "P1 Wins 4x1");
  checkExpect(estimateValue(State(Win(P2), [[2, 2, 2, 2]])),
    -10000000., "P2 Wins 4x1");
  checkExpect(estimateValue(State(Draw, [[1, 2, 1, 2]])), 0., "Draw 4x1");
  checkExpect(estimateValue(State(Ongoing(P1), [[0, 1, 1, 1]])),
    1000., "P1 Ongoing 3 in a row = 1000.");
  checkExpect(estimateValue(State(Ongoing(P2), [[0, 2, 2, 2]])),
    -1000., "P2 Ongoing 3 in a row = -1000.");
  checkExpect(estimateValue(State(Ongoing(P2), [[0, 1, 1, 1]])),
    1000., "P2 Ongoing 3 in a row = 1000.");
  checkExpect(estimateValue(State(Ongoing(P1), [[0, 2, 2, 2]])),
    -1000., "P1 Ongoing 3 in a row = -1000.");

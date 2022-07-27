open CS17SetupGame;
open Game;
 
module AIPlayer = (MyGame: Game) => {
 module PlayerGame = MyGame
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
 
 /*
 getMin: list (float) -> float
 
 Input: lst, a list of floats
 Output: the smallest float value in the list
  Recursion Diagram
 OI: []
   RI: -
   RO: -
 OO: []
 ideation space: empty -> empty
 
 OI: [0. , 1.]
   RI: [1.]
   RO: 1.
 OO: 1.
 ideation space: if the value is greater, leave it out and recur
 */
 let rec getMin: list(float) => float = lst =>
   switch(lst){
     | [] => failwith("Cannot get minimum because list is empty.")
     | [a] => a
     | [a, b, ...tl] => if (a > b) {getMin([b, ...tl])}
       else {getMin([a, ...tl])}
   }
 
 /*
 getMax: list (float) -> float
 
 Input: lst, a list of floats
 Output: the greatest float value in the list
  Recursion Diagram
 OI: []
   RI: -
   RO: -
 OO: []
 ideation space: empty -> empty
 
 OI: [0. , 1.]
   RI: [1.]
   RO: 1.
 OO: 1.
 ideation space: if the value is smaller, leave it out and recur
 */
 let rec getMax: list(float) => float = lst =>
   switch(lst){
     | [] => failwith("Cannot get maximum because list is empty.")
     | [a] => a
     | [a, b, ...tl] => if (a < b) {getMax([b, ...tl])}
       else {getMax([a, ...tl])}
   }
 /*
 miniMax: state * int -> float
  Input: s, n: s is a state and n is an integer that keeps track of the depth
 Output: a float value of the state that is achieved by 4 more moves from
         the current input state
  Recursion Diagram:
 OI: 4
   RI: -
   RO: -
 OO: estimateValue(s)
 ideation space: if the depth reaches 4, call estimateValue and return float
 
 OI: (s, 0)
   RI: (nextState(s, one legalMove), 1)
   RO: estimateValue(of the depth 4 state)
 OO: estimateValue(of the depth 4 state)
 ideation space: if n is not 4, then recur
                 call minimax on each possible nextState using List.map
  */
 let rec minimax: (PlayerGame.state, int) => float = (s, n) =>
   switch(n){
     | 4 => PlayerGame.estimateValue(s)
     | _ =>
       switch(PlayerGame.gameStatus(s)){
         | Ongoing(P1) =>
           getMax(List.map(a => minimax(a, n+1),
             List.map(b => PlayerGame.nextState(s, b),
             PlayerGame.legalMoves(s))))
         | Ongoing(P2) =>
           getMin(List.map(a => minimax(a, n+1),
             List.map(b => PlayerGame.nextState(s, b),
             PlayerGame.legalMoves(s))))
         | Win(P1) => 10000.
         | Win(P2) => -10000.
         | Draw => 0.
       }
   }
 
 /*
 maxMove: int * list(float) -> int
  Input: a, lst: a which is an integer that keeps track of the index
                lst which is a list of floats
 Output: returns the index of the biggest float in the list
  Recursion Diagram:
 OI: []
   RI: -
   RO: -
 OO: failwith
 ideation space: failwith if the list is empty because there is supposed to
                 be a legalMove / nextState
 
 OI: (0, [1., 2.])
   RI: (1, [2.])
   RO: 1
 OO: 1
 ideation space: if the head of the list does not equal getMax(lst)
                 then a +1  and recur on tail
 */
 let rec maxMove: (int, list (float)) => int = (a, lst) => {
   let m = getMax(lst);
   switch(a, lst) {
     | (a, [hd, ... tl]) => if (hd == m) {a} else {maxMove(a + 1, tl)};
     | (_, []) => failwith("There is no move related to this max float.");
   };
 };
  /*
 minMove: int * list(float) -> int
  Input: a, lst: a which is an integer that keeps track of the index
                lst which is a list of floats
 Output: returns the index of the smallest float in the list
  Recursion Diagram:
 OI: []
   RI: -
   RO: -
 OO: failwith
 ideation space: failwith if the list is empty because there is supposed to
                 be a legalMove / nextState
 
 OI: (0, [2., 1.])
   RI: (1, [1.])
   RO: 1
 OO: 1
 ideation space: if the head of the list does not equal getMin(lst)
                 then a +1  and recur on tail
 */
 let rec minMove: (int, list (float)) => int = (a, lst) => {
   let m = getMin(lst);
   switch(a, lst) {
     | (a, [hd, ... tl]) => if (hd == m) {a} else {minMove(a + 1, tl)};
     | (_, []) => failwith("There is no move related to this min float.");
   };
 };
 
 /*
 nextMove: state -> move
  Input: s, a state of a game
 Output: the best move that the AI can make depending on whether the AI is
         player 1 or player 2
 */
 let nextMove: (PlayerGame.state => PlayerGame.move) = s =>
   switch(PlayerGame.gameStatus(s)){
     | Ongoing(P1) => List.nth(PlayerGame.legalMoves(s),
                     maxMove(0, List.map(b => minimax(b, 0),
                     List.map(a => PlayerGame.nextState(s, a),
                     PlayerGame.legalMoves(s)))))
     | Ongoing(P2) => List.nth(PlayerGame.legalMoves(s), 
                     minMove(0, List.map(b => minimax(b, 0),
                     List.map(a => PlayerGame.nextState(s, a),
                     PlayerGame.legalMoves(s)))))
     | Win(_) => failwith("The game has been won")
     | Draw => failwith("The game has been drawn")
 };
  /* put your team name here! */
 let playerName = "Kaitlyn & Wonyoung :)";
 };

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame); 
module MyAIPlayer:Player = TestAIPlayer;
open TestAIPlayer; 

/* check expects for getmin */

checkError(() => getMin([]), "Cannot get minimum because list is empty.")
checkExpect(getMin([-100.]), -100., "return -100.")
checkExpect(getMin([10000., 100., 10., 0., -10., -100., -10000.]), -10000., 
  "return -10000.")

/* check expects for getmax */

checkError(() => getMax([]), "Cannot get maximum because list is empty.")
checkExpect(getMax([100.]), 100., "return 100")
checkExpect(getMax([10000., 100., 10., 0., -10., -100., -10000.]), 10000., 
  "return 10000")

/* check expects for maxmove */

checkExpect(maxMove(0, [10000., 1000., 100., 10., -10000.]), 0, "maxMove 0")
checkExpect(maxMove(0, [-1000., -100., -10.]), 2, "maxMove 3");
checkError(() => maxMove(0, []), "Cannot get maximum because list is empty.")

/* check expects for minmove */

/* Test minMove*/
checkExpect(minMove(0, [1., -3., 50.]), 1, "minMove 1");
checkExpect(minMove(0, [1., -3., -50.]), 2, "minMove 2");
checkExpect(minMove(0, [-1000., -3., -50.]), 0, "minMove 3");
checkError(() => minMove(0, []),
"Cannot get minimum because list is empty.");
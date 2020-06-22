module TicTacToe (tic_tac_toe, Player, Move, Controller(Controller)) where

data Player = X | O

instance Show Player where
  show X = "X"
  show O = "O"

other_player :: Player -> Player
other_player X = O
other_player O = X

type Row = (Maybe Player, Maybe Player, Maybe Player)
type Board = (Row, Row, Row)

empty_board :: Board
empty_board = 
  (
    (Nothing, Nothing, Nothing),
    (Nothing, Nothing, Nothing),
    (Nothing, Nothing, Nothing))

horizontal_separator :: String
horizontal_separator = "  ---+---+---\n"

print_board :: Board -> IO ()
print_board (t, m, b) = putStr (
    "   a   b   c\n" ++ 
    "1  " ++ (print_row t) ++ "\n" ++ horizontal_separator ++
    "2  " ++ (print_row m) ++ "\n" ++ horizontal_separator ++
    "3  " ++ (print_row b) ++ "\n"
  )
  where 
    print_row (l, c, r) = (show_symbol l) ++ " | " ++ (show_symbol c) ++ " | " ++ (show_symbol r)
    
    show_symbol Nothing = " "
    show_symbol (Just p) = show p


type Move = (Integer, Char)

update_board :: Board -> Player -> Move -> Board
update_board board@(top, middle, bottom) player (v, h) =
  
  case v of 
    1 -> (update_row h top, middle, bottom)
    2 -> (top, update_row h middle, bottom)
    3 -> (top, middle, update_row h bottom)
    _ -> board
  
  where 
    update_row 'a' (_, c, r) = (Just player, c, r)
    update_row 'b' (l, _, r) = (l, Just player, r)
    update_row 'c' (l, c, _) = (l, c, Just player)
    update_row _ row = row

data EndGameState = Win | Tie
data Turn = Turn (Either (Move -> Turn) EndGameState) Player (() -> IO ())

create_turn :: Board -> Player -> Turn
create_turn board player =
  Turn (Left $ (next_turn)) player (\_ -> print_board board)
  where
    next_turn move =
      create_turn (update_board board player move) (other_player player)

data Controller = Controller {
  play :: Player -> IO (Maybe Move),
  on_win :: Player -> IO (),
  on_draw :: Player -> IO ()
}
   
tic_tac_toe :: Player -> Controller -> IO ()
tic_tac_toe starting_player controller = do
  game $ create_turn empty_board starting_player

  where
    game (Turn (Left next_turn) current_player show_board) = do
      show_board ()

      input <- (play controller) current_player

      case input of
        Nothing -> return ()
        (Just move) -> game $ next_turn move
    

    game (Turn _ _ show_board) = do
      show_board ()


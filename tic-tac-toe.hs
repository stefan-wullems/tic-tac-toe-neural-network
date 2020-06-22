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

tic_tac_toe :: Player -> Turn
tic_tac_toe starting_player = create_turn empty_board starting_player
   
start_game :: Player -> (() -> IO (Maybe Move), () -> IO (Maybe Move)) -> IO ()
start_game starting_player (get_move_x, get_move_o) = do
  game $ tic_tac_toe starting_player

  where
    game (Turn (Left next_turn) current_player show_board) = do
      show_board ()

      case current_player of
        X -> do
          move <- get_move_x ()
          play_turn move
        O -> do
          move <- get_move_o ()
          play_turn move

      where
        play_turn Nothing = do return ()
        play_turn (Just move) = do game $ next_turn move
    game (Turn _ _ show_board) = do
      show_board ()

player_turn :: () -> IO (Maybe Move)
player_turn _ = do
  inp <- getLine
  if('q' `elem` inp) 
    then return Nothing
    else return $ Just (read([inp !! 0]) :: Integer, (inp !! 1))
  
main :: IO ()
main = do 
  start_game X (player_turn, player_turn)
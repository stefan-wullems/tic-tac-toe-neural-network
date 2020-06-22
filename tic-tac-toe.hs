data Player = X | O

instance Show Player where
  show X = "X"
  show O = "O"


type Row = (Maybe Player, Maybe Player, Maybe Player)
type Board = (Row, Row, Row)

create_board :: () -> Board
create_board _ = 
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


type Move = (Player, Integer, Char)

update_board :: Move -> Board -> Board
update_board (player, v, h) (t, m, b) =
  
  case v of 
    1 -> (update_row h t, m, b)
    2 -> (t, update_row h m, b)
    3 -> (t, m, update_row h b)
    _ -> (t, m, b)
  
  where 
    update_row 'a' (_, c, r) = (Just player, c, r)
    update_row 'b' (l, _, r) = (l, Just player, r)
    update_row 'c' (l, c, _) = (l, c, Just player)
    update_row _ row = row

    

    -- player_turn = start_tic_tac_toe starting_player
    
    -- result <- get_player_turn
    -- case of player_turn "1a"
    --  (Left ai_turn) -> 
    --  (Right Winner) -> print "Player won"
    --  (Right Tie) -> print "Tied"

data EndGameState = Win | Tie

data Turn = Turn (String -> Either EndGameState Turn)

tic_tac_toe :: Player -> Turn
tic_tac_toe starting_player = 
  where
    initial_board = create_board ()
    play board starting_player = 
  


  
  
turn :: Player -> Board -> IO (Maybe Board)
turn X = X_turn
turn O = ai_turn

player_turn :: Board -> IO (Maybe Board)
player_turn board = do
  inp <- getLine
  if('q' `elem` inp) 
    then return Nothing
    else return (Just (update_board (X, (read([inp !! 0]) :: Integer), (inp !! 1)) board))

ai_turn :: Board -> IO (Maybe Board)
ai_turn board = return (Just board)

main :: IO ()
main = do 
  game X (create_board ())

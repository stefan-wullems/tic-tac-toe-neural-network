import System.Random

data Player = Human | AI

instance Show Player where
  show Human = "X"
  show AI = "O"


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

game :: Board -> IO ()
game board = do
  print_board board
  inp <- getLine
  if('q' `elem` inp) 
    then return ()
    else do
      game (update_board (Human, (read([inp !! 0]) :: Integer), (inp !! 1)) board)

turn :: Player -> Board -> IO Board
turn Human board = human_turn board
turn AI board = ai_turn board

human_turn :: Board -> IO Board
human_turn board = do return board 

ai_turn :: Board -> IO Board
ai_turn board = do return board


main :: IO ()
main = do 
  g <- getStdGen
  print . take 1 $ (randomRs (0.0 :: Float, 1.0 :: Float) g)
  -- print . take 10 $ (randomRs (0.0, 1.0) g)
  game (create_board ())

      
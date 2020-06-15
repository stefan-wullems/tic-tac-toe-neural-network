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

game :: Player -> Board -> IO ()
game player board = do
  print_board board

  result <- turn player board

  case (result, player) of
    (Nothing, _) -> return ()
    (Just b, Human) -> game AI b
    (Just b, AI) -> game Human b
  
turn :: Player -> Board -> IO (Maybe Board)
turn Human = human_turn
turn AI = ai_turn

human_turn :: Board -> IO (Maybe Board)
human_turn board = do
  inp <- getLine
  if('q' `elem` inp) 
    then return Nothing
    else return (Just (update_board (Human, (read([inp !! 0]) :: Integer), (inp !! 1)) board))

ai_turn :: Board -> IO (Maybe Board)
ai_turn board = return (Just board)


main :: IO ()
main = do 
  g <- getStdGen
  print . take 1 $ (randomRs (0.0 :: Float, 1.0 :: Float) g)
  -- print . take 10 $ (randomRs (0.0, 1.0) g)
  game Human (create_board ())

      
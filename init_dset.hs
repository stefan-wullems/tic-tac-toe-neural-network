import Data.Maybe

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

data NaryTree a = Leaf | Node a [NaryTree a]
  deriving (Eq, Show)

count_nary_tree :: NaryTree a -> Int
count_nary_tree node = do_count 0 node
  where 
    do_count count Leaf = count
    do_count count (Node _ nodes) = count + (foldl (\c n -> do_count c n) (length nodes) nodes)

data Turn = X | O

instance Show Turn where
  show X = "1"
  show O = "2"

next_turn :: Turn -> Turn
next_turn X = O
next_turn O = X

replaceRange :: [a] -> (Int, Int) -> [a] -> [a]
replaceRange list _ [] = list
replaceRange [] _ list = list
replaceRange (x:xs) (start, end) list =
  first ++ list ++ second
  where
      (first, _) = splitAt start (x:xs)
      (_, second) = splitAt end (x:xs)

some :: (a -> Bool) -> [a] -> Bool
some _ [] = False
some predicate list = foldl (\b e ->  b || (predicate e)) False list

every :: (a -> Bool) -> [a] -> Bool
every _ [] = True
every predicate list = foldl (\b e ->  b && (predicate e)) True list

end_game_patterns :: [[Int]]
end_game_patterns = [
    [0,1,2],
    [3,4,5],
    [6,7,8],
    [1,3,6],
    [2,4,7],
    [3,5,8],
    [0,4,8],
    [2,4,6]
  ]

is_end_game_pattern :: Turn -> String -> Bool
is_end_game_pattern turn pattern = 
  some (\end_game_pattern -> every (\i -> [pattern !! i] == (show turn)) end_game_pattern) end_game_patterns

create_board_state_node :: String -> Turn -> NaryTree String
create_board_state_node state turn = 
  if (not ('0' `elem` state)) || is_end_game_pattern turn state
    then Leaf
    else Node state nodes
      where
        empty_column_indices = filter (\i -> state !! i == '0') [0..(length state - 1)]
        nodes = map (\i -> create_board_state_node (replaceRange state (i, i + 1) (show turn)) (next_turn turn)) empty_column_indices
  

initial_board :: String
initial_board = "000000000"

initial_turn :: Turn
initial_turn = X

data Line = Line {
  lineID   :: String,
  childIds :: [(String, Int)]
} deriving (Show)

nary_to_list :: (NaryTree a -> b) -> NaryTree a -> [b]
nary_to_list node_to_elem tree = to_list [] tree
  where 
    to_list l node = 
      case node of
        (Node _ children) -> l ++ [node_to_elem node] ++ concatMap (to_list []) children
        _ -> l

extract_justs :: [Maybe a] -> [a]
extract_justs list = ((map fromJust) . (filter isJust)) list

default_ratio :: Int
default_ratio = 5

node_to_line :: NaryTree String -> Maybe Line
node_to_line Leaf = Nothing
node_to_line (Node pattern children) = Just (Line { lineID = pattern, childIds = map (\x -> (x, default_ratio)) $ extract_justs $ map extract_id $ children })
  where
    extract_id (Node p _) = Just p
    extract_id Leaf = Nothing

line_to_cols :: Line -> [String]
line_to_cols line = [lineID line, show $ childIds line]

data CSV = CSV String deriving (Show)

list_to_csv :: [String] -> (a -> [String]) -> [a] -> CSV
list_to_csv headers el_to_cols list = CSV (join_cols headers ++ concatMap (join_cols . el_to_cols) list)
  where
    join_cols [] = ""
    join_cols (x:xs) = ((++ "\n") . foldl (\line col -> line ++ ":" ++ col) x) xs


write_csv :: String -> CSV -> IO ()
write_csv file_name (CSV csv) = do
  writeFile file_name csv
   

init_dset :: () -> IO ()
init_dset _ = do
  let states_tree = create_board_state_node initial_board initial_turn
  let states_list = extract_justs $ nary_to_list node_to_line states_tree
  let csv = list_to_csv ["ID", "children"] line_to_cols states_list

  write_csv "./dset.csv" csv
  

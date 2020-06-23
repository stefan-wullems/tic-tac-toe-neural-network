module Main where

import TicTacToe

player_turn :: () -> IO (Maybe Move)
player_turn _ = do
  inp <- getLine
  if('q' `elem` inp) 
    then return Nothing
    else return $ Just (read([inp !! 0]) :: Integer, (inp !! 1))
  
main :: IO ()
main = do 
  let starting_player = X
  let controller = Controller{
    play = (\_ -> player_turn ()),
    on_draw = (\_ -> return ()),
    on_win = (\_ -> return ()) 
  }
  tic_tac_toe starting_player controller
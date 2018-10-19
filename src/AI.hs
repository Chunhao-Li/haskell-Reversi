{-|
Module      : AI
Description : The AI for Othello
Copyright   : (c) Robert 'Probie' Offner, <your name here>, 2018
License     : GPL-3
-}
module AI where

import Game
import GameState
-- |  An AI is a function from the time given and the current state of
-- the game to a (potentially infinite) list of increasingly better
-- moves.
type AI = Double -> Game -> [(Int, Int)]

-- | A list of known AIs and their names.
ais :: [(String, AI)]
ais = [("default", makeBestMove), ("helloWorld", makeFirstLegalMove), ("greedyBot", greedyBot)]

-- | The default AI. It just repeatedly applies `makeAMove` to
-- increasingly higher depths.
makeBestMove :: AI
makeBestMove _timeout game = map (makeAMove game) [1..]

-- | Given a `Game` and a lookahead, return the best move to play
makeAMove :: Game -> Int -> (Int, Int)
makeAMove = undefined
-- makeAMove (Game (Just p) board) depth
--     | depth == 1    = find_moves (maximum score_ls) legal_move_ls
--     | depth `mod` 2 == 0 = minimum score_ls
--      depth > 0     =   undefined
--     where
--     legal_moves_ls = legalMoves (Game (Just p) board)
--     score_ls = [currentScore p x | x <- board_ls ]
--     board_ls = playAllMoves legal_moves_ls p board
-- playAllMoves :: [(Int, Int)] -> Player -> Board -> [Board]
-- playAllMoves ls p board = case ls of
--     []      ->  [board]
--     ((a,b):xs) -> playMove a b p board:playAllMoves xs p board
--------------------------------------------------------------
newGame :: Game -> [Game]
newGame (Game Nothing _) = []
newGame game@(Game (Just p) board) = map (Game (Just (opponent p))) (newBoard board moves)
  where
    moves = legalMoves game
    newBoard :: Board -> [(Int, Int)] -> [Board]
    newBoard b move_ls = case (fst_ls, snd_ls) of
      ([],[])   -> []
      ([], _)    -> []
      (_, [])   -> []
      (x:xs, y:ys)    ->  playMove x y (opponent p) b :
                            newBoard b (zip xs ys)

      where
        fst_ls = map fst move_ls
        snd_ls = map snd move_ls



-------------------------------------------------------------
-- countAllScores :: Player -> [Board] -> [Int]
-- countAllScores p ls = case ls of
--     []          -> []
--     x:xs        -> currentScore p x : countAllScores p xs
-- | A list of possible moves for the players.
possibleMoves :: [(Int, Int)]
possibleMoves = [(row, col) | row <- [0..7], col <- [0..7]]

-- | A list of valid moves from all possible moves for a given game.
legalMoves :: Game -> [(Int, Int)]
legalMoves (Game Nothing _) = [] -- The game is over
legalMoves (Game (Just player) board) =
    filter legalForThisGame possibleMoves
    where
        legalForThisGame (row, col) = legalMove row col player board

-- | Pick the first legal move for the game.
makeFirstLegalMove :: AI
makeFirstLegalMove _ game = case legalMoves game of
    []          -> error "makeFirstLegalMove: No Legal moves"
    move:_      -> [move]

greedyBot :: AI
greedyBot _ (Game Nothing _) = []
greedyBot _ game@(Game (Just player) board) = case legalMoves game of
        []   -> error "greedyBot: No Legal moves"
        x:xs ->  find_moves (maximum (calculate_score (x:xs))) (x:xs)
        where
           calculate_score :: [(Int, Int)] -> [Int]
           calculate_score move_ls = case move_ls of
                    []          -> []
                    (a,b):xs    ->
                        currentScore player (playMove a b player board):
                        calculate_score xs


           find_moves :: Int -> [(Int, Int)] -> [(Int, Int)]
           find_moves max_n move_ls = case move_ls of
                []          -> []
                (a,b):xs
                    | currentScore player (playMove a b player board) == max_n ->
                        (a,b): find_moves max_n xs
                    | otherwise -> find_moves max_n xs
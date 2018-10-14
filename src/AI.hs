{-|
Module      : AI
Description : The AI for Othello
Copyright   : (c) Robert 'Probie' Offner, <your name here>, 2018
License     : GPL-3
-}
module AI where

import Game

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
makeAMove = error "makeAMove: Unimplemented"

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
           find_moves max_n move_ls = case move_ls of
                []          -> []
                (a,b):xs
                    | currentScore player (playMove a b player board) == max_n ->
                        (a,b): find_moves max_n xs
                    | otherwise -> find_moves max_n xs
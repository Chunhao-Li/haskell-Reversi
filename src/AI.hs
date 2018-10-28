{-
Module  : AI
Copy right from ANU assignment 3
Date : Oct. 14th
-}
module AI where

import Game
import GameState

-- | An AI is a function from the time given and the current state of
-- the game to a (potentially infinite) list of increasingly better
-- moves.
type AI = Double -> Game -> [(Int, Int)]

data Tree a = Node a [Tree a]
type Score = Int
type Move = (Int, Int)
type GameState = (Score, [Move], Game)

-- | A list of known AIs and their names.
ais :: [(String, AI)]
ais = [("default", makeBestMove), ("helloWorld", makeFirstLegalMove),
      ("greedyBot", greedyBot), ("alphaBetaBot", alphaBetaBot),
      ("minimaxBot", minimaxBot)]

-- | The default AI. It just repeatedly applies `makeAMove` to
-- increasingly higher depths.
makeBestMove :: AI
makeBestMove _timeout game = map (makeAMove game) [1..]

-- | Given a `Game` and a lookahead, return the best move to play
makeAMove :: Game -> Int -> (Int, Int)
makeAMove game d = last $ snd' (alphaBetaMinimax (prune d (othelloTreeMobility game)))

-- | This AI can find the next move which gives best board position.
greedyBot :: AI
greedyBot _ (Game Nothing _) = []
greedyBot _ game@(Game (Just _) _) = case legalMoves game of
        []   -> error "greedyBot: No Legal moves"
        _ -> getMoves maxScore valueMoves
        where
            valueMoves = valueNextPositions game
            maxScore = maximum $ map fst' valueMoves
            getMoves :: Int -> [GameState] -> [Move]
            getMoves maxS lst = case lst of
                []        -> []
                x:xs
                  | fst' x == maxS    -> [head (snd' x)]
                  | otherwise         -> getMoves maxS xs

-- | A simple AI with minimax algorithm.
minimaxBot :: AI
minimaxBot _timeout game = map (minimax game) [1..]

-- | It is the basic function of minimaxBot which can make a move.
minimax :: Game -> Int -> (Int, Int)
minimax game d = last $ (snd' . maximise . prune d .othelloTreeGameState) game

-- | A better AI with alpha beta pruning.
alphaBetaBot :: AI
alphaBetaBot _timeout game = map (alphaBetaMove game) [1..]

-- | It is the basic function of alphaBetaBot which can make a move.
alphaBetaMove :: Game -> Int -> (Int, Int)
alphaBetaMove game d = last $ snd' (alphaBetaMinimax (prune d (othelloTreeGameState game)))

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


gameTree :: (a -> [a]) -> a -> Tree a
gameTree f a = Node a (map (gameTree f) (f a))

maximise :: (Ord a) => Tree a -> a
maximise (Node v [])= v
maximise (Node _ subtrees) = maximum $  map minimise subtrees

minimise :: (Ord a) => Tree a -> a
minimise (Node v []) = v
minimise (Node _ subtrees) = minimum $ map maximise subtrees

prune :: Int -> Tree a -> Tree a
prune _ (Node a []) = Node a []
prune 0 (Node a _) = Node a []
prune n (Node a children) = Node a (map (prune (n-1)) children)


alphaBetaMinimax :: (Ord a) => Tree a -> a
alphaBetaMinimax = maximum . alphaBetaMaximise

alphaBetaMaximise :: (Ord a) => Tree a -> [a]
alphaBetaMaximise (Node gs []) = [gs]
alphaBetaMaximise (Node _ subtrees) = mapmin (map alphaBetaMinimise subtrees)
    where
        mapmin []     = []
        mapmin (x:xs) = minimum x : omitMin (minimum x) xs

omitMin :: (Ord a) => a -> [[a]] -> [a]
omitMin _ [] = []
omitMin pot (x:xs)
    | minleq x pot = omitMin pot xs
    | otherwise = minimum x : omitMin (minimum x) xs

minleq :: (Ord a) => [a] -> a -> Bool
minleq [] _ = False
minleq (x:xs) pot
    | x <=  pot   = True
    | otherwise   = minleq xs pot

alphaBetaMinimise :: (Ord a) => Tree a -> [a]
alphaBetaMinimise (Node gs []) = [gs]
alphaBetaMinimise (Node _ subtrees) = mapmax (map alphaBetaMaximise subtrees)
    where
        mapmax [] = []
        mapmax (x:xs) = maximum x : omitMax (maximum x) xs

omitMax :: (Ord a) => a -> [[a]] -> [a]
omitMax _ [] = []
omitMax pot (x:xs)
    | maxleq x pot  = omitMax pot xs
    | otherwise = maximum x : omitMax (maximum x) xs

maxleq :: (Ord a) => [a] -> a -> Bool
maxleq [] _ = False
maxleq (x:xs) pot
    | x >= pot  = True
    | otherwise = maxleq xs pot


playAllMoves :: [(Int, Int)] -> Player -> Board -> [Board]
playAllMoves lst p board = case lst of
    []      ->  [board]
    ((a,b):xs) -> playMove a b p board:playAllMoves xs p board

-- | This function can generate all possible game states after making a move.
nextGame :: Game -> [Game]
nextGame (Game Nothing _) = []
nextGame game@(Game (Just p) board)
    | null moves    = []
    | otherwise     = map (Game (Just (opponent p))) (playAllMoves moves p board)
    where
        moves = legalMoves game


fst' :: (a, b, c) -> a
fst' (s, _, _) = s

snd' :: (a, b, c) -> b
snd' (_, m, _) = m

-- | A heuristic method of evaluating pieces positions
valuePositions :: Player -> Game -> Score
valuePositions player (Game _ board) = countValues (concat boardWithWeights)
    where
        countValues lst = case lst of
            []    -> 0
            ((w, pl):xs)
                | pl == Just player -> w + countValues xs
                | pl == Just (opponent player) -> (-w) + countValues xs
                | otherwise -> 0
        boardWithWeights = [zip x y | (x,y) <- zip weights board]
        weights = [ [500, -25, 10, 5, 5, 10, -25, 500],
                    [-25, -45, 1, 1, 1, 1, -45, -25],
                    [10, 1, 3, 2, 2, 3, 1, 10],
                    [5, 1, 2, 1, 1, 2, 1, 5],
                    [5, 1, 2, 1, 1, 2, 1, 5],
                    [10, 1, 3, 2, 2, 3, 1, 10],
                    [-25, -45, 1, 1, 1, 1, -45, -25],
                    [500, -25, 10, 5, 5, 10, -25, 500] ]

-- | This function can make a list of GameState with the input Game,
-- while the scores are evaluated based on the board positions.
valueNextPositions :: Game -> [GameState]
valueNextPositions game  =  case game of
    (Game Nothing b)    -> [(-10000, [], Game Nothing b)]
    (Game (Just pl) board)    -> map (valueAMove pl board) moves
    where
        moves = legalMoves game
        valueAMove :: Player -> Board -> Move -> GameState
        valueAMove p b (x,y) = (score, [(x,y)], newGame)
            where
                newBoard = playMove x y p b
                score = valuePositions p $ Game (Just p) newBoard
                newGame
                 | legalMoves (Game (Just (opponent p)) newBoard) /= [] = Game (Just (opponent p)) newBoard
                 | otherwise = Game Nothing newBoard

-- | A heuristic method of evaluating pieces mobility.
valueMobility :: Player -> Game -> Score
valueMobility _ (Game Nothing _) = 0
valueMobility p game@(Game (Just pl) b)
    | p == pl   = length (legalMoves game) - length (legalMoves opponentGame)
    | otherwise   = length (legalMoves opponentGame) - length (legalMoves game)
    where
        opponentGame = Game (Just (opponent pl)) b

-- |This function can make a list of GameState with the input Game,
-- while the scores are evaluated based on the pieces mobility and board positions.
valueNextMobility :: Game -> [GameState]
valueNextMobility game  =  case game of
    (Game Nothing b)    -> [(-10000, [], Game Nothing b)]
    (Game (Just pl) board)    -> map (valueMove pl board) moves
    where
        moves = legalMoves game
        valueMove :: Player -> Board -> Move -> GameState
        valueMove p b (x,y) = (score, [(x,y)], newGame)
            where
                newBoard = playMove x y p b
                score = 15 *valueMobility p (Game (Just p) b) + valuePositions p (Game (Just p) b)
                newGame
                 | legalMoves (Game (Just (opponent p)) newBoard) /= [] = Game (Just (opponent p)) newBoard
                 | otherwise = Game Nothing newBoard

-- | The main function of making a game tree, the scores are evaluated by pieces' positions.
othelloTreeGameState :: Game -> Tree GameState
othelloTreeGameState (Game Nothing board) = Node (-10000, [], Game Nothing board) []
othelloTreeGameState game@(Game (Just pl) _) = Node (-10000, [], game) (map (gameTree nextGameStateList) firstGameState)
    where
        firstGameState = valueNextPositions game
        nextGameStateList :: GameState -> [GameState]
        nextGameStateList (_, moveList, g) = case g of
            Game (Just _) _ ->  zip3 scores (newMoveList moves moveList) games
            Game Nothing _ -> []
            where
                moves = legalMoves g
                games = nextGame g
                scores = map (valuePositions pl) games

-- | The main function of making a game tree, the scores are evaluated by pieces' positions and mobility.
othelloTreeMobility :: Game -> Tree GameState
othelloTreeMobility (Game Nothing board) = Node (-10000, [], Game Nothing board) []
othelloTreeMobility game@(Game (Just pl) _) = Node (-10000, [], game) (map (gameTree nextGameStateList) firstGameState)
    where
        firstGameState = valueNextMobility game
        nextGameStateList :: GameState -> [GameState]
        nextGameStateList (_, moveList, g) = case g of
            Game (Just _) _ ->  zip3 scores (newMoveList moves moveList) games
            Game Nothing _ -> []
            where
                moves = legalMoves g
                games = nextGame g
                scores = zipWith (+) (map (valuePositions pl) games)
                                     (map ((*15).valueMobility pl) games)

-- | A helper function which can add new moves to the original moves list individually.
newMoveList :: [Move] -> [Move] -> [[Move]]
newMoveList newMoves oldMoves = case newMoves of
    []      -> []
    x:xs    -> (x:oldMoves) : newMoveList xs oldMoves










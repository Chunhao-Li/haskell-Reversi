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

-- | A list of known AIs and their names.
ais :: [(String, AI)]
ais = [("default", makeBestMove), ("helloWorld", makeFirstLegalMove),
      ("greedyBot", greedyBot), ("alphaBetaBot", alphaBetaBot),
      ("minimaxBot", minimaxBot), ("minimaxV1Bot", minimaxV1Bot)]

-- | The default AI. It just repeatedly applies `makeAMove` to
-- increasingly higher depths.
makeBestMove :: AI
makeBestMove _timeout game = map (makeAMove game) [1..]

-- | Given a `Game` and a lookahead, return the best move to play
makeAMove :: Game -> Int -> (Int, Int)
makeAMove  = undefined

minimaxBot :: AI
minimaxBot _timeout game = map (minimax game) [1..]

minimax :: Game -> Int -> (Int, Int)
minimax game d = last $ (snd' . maximise . prune d .othelloGameState) game

alphaBetaMove :: Game -> Int -> (Int, Int)
alphaBetaMove game d = last $ snd' (maximise1 (prune d (othelloGameState game)))

alphaBetaBot :: AI
alphaBetaBot _timeout game = map (alphaBetaMove game) [1..]

data Tree a = Node a [Tree a]

type Score = Int
type Move = (Int, Int)
type GameState = (Score, [Move], Game)


minimaxV1Bot :: AI
minimaxV1Bot _timeout game = map (minimaxV1 game) [1..]

minimaxV1 :: Game -> Int -> (Int, Int)
minimaxV1 game@(Game _ board) d
  | piecesLength (concat board) <= 15 = last $ (snd' . maximise1 . prune d .othelloTreeMobility) game
  | otherwise = last $ (snd' . maximise1 . prune d .othelloGameState) game
  where
    piecesLength :: [Maybe Player] -> Int
    piecesLength b = case b of
      []    -> 0
      (x:xs)
        | x == Just Dark || x == Just Light -> 1 + piecesLength xs
        | otherwise -> piecesLength xs

valueMobility :: Game -> Score
valueMobility (Game Nothing _) = undefined
valueMobility game@(Game (Just p) b) =
      length (legalMoves game) - length (legalMoves opponentGame)
      where
        opponentGame = Game (Just (opponent p)) b


othelloTreeMobility :: Game -> Tree GameState
othelloTreeMobility (Game Nothing board) = Node (-10000, [], Game Nothing board) []
othelloTreeMobility game@(Game (Just _) _) = Node (-10000, [], game) (map (gameTree makeGameStateList) firstGameState)
    where
        firstGameState = valueNextMove game
        makeGameStateList :: GameState -> [GameState]
        makeGameStateList (_, moveList, g) = case g of
            Game (Just _) _ ->  zip3 scores (newMoveList moves moveList) games
            Game Nothing _ -> []
            where
                moves = legalMoves g
                games = nextGame g
                scores = map valueMobility games

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

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Node a lst) = case lst of
  []    -> Node (f a) []
  cs    -> Node (f a) (map (treeMap f) cs)

maximise1 :: (Ord a) => Tree a -> a
maximise1 = maximum . maximise'

maximise' :: (Ord a) => Tree a -> [a]
maximise' (Node gs []) = [gs]
maximise' (Node _ subtrees) = mapmin (map minimise' subtrees)
  where
    -- mapmin :: (Ord a) => [[a]] -> [a]
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

minimise1 :: (Ord a) => Tree a -> a
minimise1 = minimum . minimise'

minimise' :: (Ord a) => Tree a -> [a]
minimise' (Node gs []) = [gs]
minimise' (Node _ subtrees) = mapmax (map maximise' subtrees)
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

othelloGameState :: Game -> Tree GameState
othelloGameState (Game Nothing board) = Node (-10000, [], Game Nothing board) []
othelloGameState game@(Game (Just pl) _) = Node (-10000, [], game) (map (gameTree makeGameStateList) firstGameState)
    where
        firstGameState = valueNextMove game
        makeGameStateList :: GameState -> [GameState]
        makeGameStateList (_, moveList, g) = case g of
            Game (Just _) _ ->  zip3 scores (newMoveList moves moveList) games
            Game Nothing _ -> []
            where
                moves = legalMoves g
                games = nextGame g
                scores = map (valueGame pl) games

fst' :: (a, b, c) -> a
fst' (s, _, _) = s

snd' :: (a, b, c) -> b
snd' (_, m, _) = m
valueNextMove :: Game -> [GameState]
valueNextMove game  =  case game of
  (Game Nothing b)    -> [(-10000, [], Game Nothing b)]
  (Game (Just pl) board)    -> map (valueMove pl board) moves
  where
    moves = legalMoves game
    valueMove :: Player -> Board -> Move -> GameState
    valueMove p b (x,y) = (score, [(x,y)], newGame)
      where
        newBoard = playMove x y p b
        score = valueBoard p newBoard
        newGame
         | legalMoves (Game (Just (opponent p)) newBoard) /= [] = Game (Just (opponent p)) newBoard
         | otherwise = Game Nothing newBoard


newMoveList :: [Move] -> [Move] -> [[Move]]
newMoveList m ml = case m of
    []      -> []
    x:xs    -> (x:ml) : newMoveList xs ml


valueBoard :: Player -> Board -> Score
valueBoard p board =  countValues (concat boardWithWeights)
  where
    countValues lst = case lst of
      []    -> 0
      ((n, pl):xs)
       | pl == Just p -> n + countValues xs
       | pl == Just (opponent p) -> (-n) + countValues xs
       | otherwise -> 0
    boardWithWeights = [zip x y | (x,y) <- zip weights board]

weights :: [[Int]]
weights = [[120, -25, 10, 5, 5, 10, -25, 120],
        [-25, -45, 1, 1, 1, 1, -45, -25],
        [10, 1, 3, 2, 2, 3, 1, 10],
        [5, 1, 2, 1, 1, 2, 1, 5],
        [5, 1, 2, 1, 1, 2, 1, 5],
        [10, 1, 3, 2, 2, 3, 1, 10],
        [-25, -45, 1, 1, 1, 1, -45, -25],
        [120, -25, 10, 5, 5, 10, -25, 120] ]


valueGame :: Player -> Game -> Score
valueGame pl (Game _ board) = valueBoard pl board


playAllMoves :: [(Int, Int)] -> Player -> Board -> [Board]
playAllMoves lst p board = case lst of
    []      ->  [board]
    ((a,b):xs) -> playMove a b p board:playAllMoves xs p board


nextGame :: Game -> [Game]
nextGame (Game Nothing _) = []
nextGame game@(Game (Just p) board)
    | null moves    = []
    | otherwise     = map (Game (Just (opponent p)))
                          (playAllMoves moves p board)
  where
    moves = legalMoves game


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
greedyBot _ game@(Game (Just _) _) = case legalMoves game of
        []   -> error "greedyBot: No Legal moves"
        _ -> getMoves maxScore valueMoves

        where
          valueMoves = valueNextMove game
          maxScore = maximum $ map fst' valueMoves
          getMoves :: Int -> [GameState] -> [Move]
          getMoves maxS lst = case lst of
            []        -> []
            x:xs
              | fst' x == maxS    -> [head (snd' x)]
              | otherwise         -> getMoves maxS xs




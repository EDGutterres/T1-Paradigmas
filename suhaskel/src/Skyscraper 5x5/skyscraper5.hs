module Main where

import Control.Applicative ((<|>))
import qualified Control.Monad
import qualified Data.Char
import qualified Data.Function
import qualified Data.List.Split
import qualified Data.List

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
type Row  = [Cell]
type Grid = [Row]

replace2D :: Int -> a -> [[a]] -> [[a]]
replace2D i v =
    let (x, y) = (i `quot` 5, i `mod` 5) 
    in replace x (replace y (const v))

replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]

first_prune_left :: [Int] -> Grid -> Grid
first_prune_left [] g = g
first_prune_left (a:b) g =
  first_prune_left b (replace2D (a * 5 + 0) (Fixed 5) g)

first_prune_right :: [Int] -> Grid -> Grid
first_prune_right [] g = g
first_prune_right (a:b) g =
  first_prune_right b (replace2D (a * 5 + 4) (Fixed 5) g)

first_prune_up :: [Int] -> Grid -> Grid
first_prune_up [] g = g
first_prune_up (a:b) g =
  first_prune_up b (replace2D (0 + a) (Fixed 5) g)

first_prune_down :: [Int] -> Grid -> Grid
first_prune_down [] g = g
first_prune_down (a:b) g =
  first_prune_down b (replace2D (20 +  a) (Fixed 5) g)


validate_row :: Int -> [Int] -> Int
validate_row _ [] = 0
validate_row pivot (head:tail) =
  if (head > pivot) then
    1 + validate_row head tail
  else
    validate_row pivot tail

reverse_validate_rows :: [Int] -> Grid -> Bool
reverse_validate_rows [] [] = True
reverse_validate_rows (a:b) (head:tail) =
  if a == 0 then
    reverse_validate_rows b tail
  else
    if a == validate_row 0 (reverse [x | Fixed x <- head]) then
      reverse_validate_rows b tail
    else
      False

validate_rows :: [Int] -> Grid -> Bool
validate_rows [] [] = True
validate_rows (a:b) (head:tail) =
  if a == 0 then
    validate_rows b tail
  else
    if a == validate_row 0 [x | Fixed x <- head] then
      validate_rows b tail
    else
      False

validate_grid :: [Int] -> [Int] -> [Int] -> [Int] -> Grid -> Bool
validate_grid left right up down grid =
     validate_rows left grid &&
     reverse_validate_rows right grid &&
     validate_rows up (Data.List.transpose grid) &&
     reverse_validate_rows down (Data.List.transpose grid)
     
readGrid :: String -> Maybe Grid
readGrid s
  | length s == 25 = traverse (traverse readCell) . Data.List.Split.chunksOf 5 $ s
  | otherwise      = Nothing
  where
    readCell '.' = Just $ Possible [1..5]
    readCell c
      | Data.Char.isDigit c && c > '0' = Just . Fixed . Data.Char.digitToInt $ c
      | otherwise = Nothing

showGridWithPossibilities :: Grid -> String
showGridWithPossibilities = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x)     = show x ++ "      "
    showCell (Possible xs) =
      (++ "]")
      . Data.List.foldl' (\acc x -> acc ++ if x `elem` xs then show x else " ") "["
      $ [1..5]

pruneCells :: [Cell] -> Maybe [Cell]
pruneCells cells = traverse pruneCell cells
  where
    fixeds = [x | Fixed x <- cells]

    pruneCell (Possible xs) = case xs Data.List.\\ fixeds of
      []  -> Nothing
      [y] -> Just $ Fixed y
      ys  -> Just $ Possible ys
    pruneCell x = Just x

pruneGrid' :: Grid -> Maybe Grid
pruneGrid' grid =
  traverse pruneCells grid
  >>= fmap Data.List.transpose . traverse pruneCells . Data.List.transpose

pruneGrid :: Grid -> Maybe Grid
pruneGrid = fixM pruneGrid'
  where
    fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'

isGridFilled :: Grid -> Bool
isGridFilled grid = null [ () | Possible _ <- concat grid ]

isGridInvalid :: Grid -> Bool
isGridInvalid grid =
  any isInvalidRow grid
  || any isInvalidRow (Data.List.transpose grid)

  where
    isInvalidRow row =
      let fixeds         = [x | Fixed x <- row]
          emptyPossibles = [x | Possible x <- row, null x]
      in hasDups fixeds || not (null emptyPossibles)

    hasDups l = hasDups' l []

    hasDups' [] _ = False
    hasDups' (y:ys) xs
      | y `elem` xs = True
      | otherwise   = hasDups' ys (y:xs)

nextGrids :: Grid -> (Grid, Grid)
nextGrids grid =
  let (i, first@(Fixed _), rest) =
        fixCell
        . Data.List.minimumBy (compare `Data.Function.on` (possibilityCount . snd))
        . filter (isPossible . snd)
        . zip [0..]
        . concat
        $ grid
  in (replace2D i first grid, replace2D i rest grid)
  where
    isPossible (Possible _) = True
    isPossible _            = False

    possibilityCount (Possible xs) = length xs
    possibilityCount (Fixed _)     = 1

    fixCell (i, Possible [x, y]) = (i, Fixed x, Fixed y)
    fixCell (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
    fixCell _                    = error "Impossible case"

solve :: [Int] -> [Int] -> [Int] -> [Int] -> Grid -> Maybe Grid
solve l r u d grid = pruneGrid grid >>= solve' l r u d
  where
    solve' l r u d g
      | isGridInvalid g || (isGridFilled g && not(validate_grid l r u d g)) = Nothing
      | (isGridFilled g && validate_grid l r u d g) = Just g
      | otherwise       =
          let (grid1, grid2) = nextGrids g
          in solve l r u d grid1 <|> solve l r u d grid2

solve_all :: [Int] -> [Int] -> [Int] -> [Int] -> Grid -> Maybe Grid
solve_all l r u d =
    solve l r u d
    . first_prune_left (Data.List.elemIndices 1 l)
    . first_prune_right (Data.List.elemIndices 1 r)
    . first_prune_up (Data.List.elemIndices 1 u)
    . first_prune_down (Data.List.elemIndices 1 d)
main = do
  -- problema 32
  let left   = [2, 2, 0, 0, 0]
  let right  = [3, 0, 0, 0, 4]
  let up     = [0, 0, 2, 2, 0]
  let down   = [1, 0, 2, 4, 3]

  inputs <- lines <$> getContents
  Control.Monad.forM_ inputs $ \input ->
    case readGrid "........................." of
      Nothing   -> putStrLn "Invalid input"
      Just grid -> case solve_all left right up down grid of
        Nothing    -> putStrLn "No solution found"
        Just grid' -> putStrLn $ showGridWithPossibilities grid'
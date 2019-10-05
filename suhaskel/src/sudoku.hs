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
validate_grid left right up bottom grid =
     validate_rows left grid &&
     reverse_validate_rows right grid &&
     validate_rows up (Data.List.transpose grid) &&
     reverse_validate_rows bottom (Data.List.transpose grid)
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

    replace2D :: Int -> a -> [[a]] -> [[a]]
    replace2D i v = let (x, y) = (i `quot` 5, i `mod` 5) in replace x (replace y (const v))
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]

solve :: [Int] -> [Int] -> [Int] -> [Int] -> Grid -> Maybe Grid
solve l r u b grid = pruneGrid grid >>= solve' l r u b
  where
    solve' l r u b g
      | isGridInvalid g || (isGridFilled g && not(validate_grid l r u b g)) = Nothing
      | validate_grid l r u b g = Just g
      | otherwise       =
          let (grid1, grid2) = nextGrids g
          in solve l r u b grid1 <|> solve l r u b grid2

main = do
  let left   = [1, 4, 3, 2, 0]
  let right  = [3, 2, 3, 0, 1]
  let up     = [0, 3, 3, 0, 0]
  let bottom = [0, 2, 2, 2, 1]
  -- .........................

  -- SOLUÇÃO "5314212354345214521321435"

  -- INPUT SÓ COM CINCOS 5.......................5

  inputs <- lines <$> getContents
  Control.Monad.forM_ inputs $ \input ->
    case readGrid input of
      Nothing   -> putStrLn "Invalid input"
      Just grid -> case solve left right up bottom grid of
        Nothing    -> putStrLn "No solution found"
        Just grid' -> putStrLn $ showGridWithPossibilities grid'
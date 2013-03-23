import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type Area = String
type Color = Int
type Solution = Map Area Color
type Borders = Map Area (Set Area)

solve :: [String] -> [String]
solve xs = map (\x -> fst x ++ " " ++ show (snd x))
           $ Map.toAscList $ head 
           $ mapMaybe (colorize $ bordersFromLines xs) [1 ..]

-- Границы регионов для полученных строк.
bordersFromLines :: [String] -> Borders
bordersFromLines xs = Map.fromList $ map parseLine xs

-- Разбор строки.
parseLine :: String -> (Area, Set Area)
parseLine xs = let splitted = groupBy (\x y -> y /= ':') xs
               in (head splitted, Set.fromList . words . drop 1 $ splitted !! 1)

colorize :: Borders -> Color -> Maybe Solution
colorize borders maxColor =
        colorizeArea borders maxColor (Map.keysSet borders) Map.empty

-- Передаем границы, максимальный цвет, оставшиеся регионы, текущую часть решения.
colorizeArea :: Borders -> Color -> Set Area -> Solution -> Maybe Solution
colorizeArea borders maxColor areas solution =
    if Set.null areas then Just solution
    else tryColor borders maxColor areas solution nextArea 1
         where neigbours = find (not . Set.null)
                           $ map (\x -> borders Map.! x `Set.intersection` areas)
                           $ Map.keys solution
               nextArea = head . Set.elems $ fromMaybe areas neigbours

tryColor :: Borders -> Color -> Set Area -> Solution -> Area -> Color -> Maybe Solution
tryColor borders maxColor areas solution area color
    | goodColor borders solution area color =
        case colorizeArea borders maxColor areas' solution' of
          Nothing | color < maxColor -> tryNext
          s -> s
    | color < maxColor = tryNext
    | otherwise = Nothing
    where tryNext = tryColor borders maxColor areas solution area (color + 1)
          solution' = Map.insert area color solution
          areas' = Set.delete area areas

goodColor :: Borders -> Solution -> Area -> Color -> Bool
goodColor borders solution area color =
    all (\a -> Map.findWithDefault 0 a solution /= color)
        (Set.toList $ borders Map.! area)

main = interact $ unlines . solve . lines

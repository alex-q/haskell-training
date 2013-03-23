-- Головоломка с планками.

import Data.List

data Plank = Plank Int [Section] deriving (Show, Eq)
data Section = Up | Down | Hole deriving (Show, Eq)

rotations :: Plank -> [Plank]
rotations p = map ($ p) [id, rotX, rotZ, rotX . rotZ]

rotX :: Plank -> Plank
rotX (Plank n xs) = Plank n $ map rotSecX xs

rotZ :: Plank -> Plank
rotZ (Plank n xs) = Plank n $ reverse xs

rotSecX :: Section -> Section
rotSecX Up = Down
rotSecX Down = Up
rotSecX Hole = Hole

splitPlanks :: [Plank] -> [[Plank]]
splitPlanks ps = let ps' = splitAt (length ps `div` 2) ps in [fst ps'] ++ [snd ps']

sections :: Plank -> [Section]
sections (Plank _ xs) = xs

rot90 :: [[a]] -> [[a]]
rot90 = reverse . transpose

-- Верхний и нижний слои. Нижний поворачиваем на 90°.
layers :: [[Plank]] -> [[[Section]]]
layers (us:ds:[]) = [map sections us, rot90 $ map sections ds]

-- Подходят ли слои.
layerMatch :: [[[Section]]] -> Bool
layerMatch (uss:dss:[]) = and $ zipWith rowMatch uss dss

rowMatch :: [Section] -> [Section] -> Bool
rowMatch us ds = and $ zipWith secMatch us ds

secMatch :: Section -> Section -> Bool
secMatch Hole _ = True
secMatch _ Hole = True
secMatch Up Down = True
secMatch _ _ = False

allStates :: [Plank] -> [[[Plank]]]
allStates ps = map splitPlanks $ concat $ map permutations $ sequence $ map rotations ps

validStates :: [Plank] -> [[[Plank]]]
validStates ps = filter (layerMatch . layers) $ allStates ps

-- Эквивалентные преобразования и их композиции.
equalTransforms :: [[[Plank]] -> [[Plank]]]
equalTransforms = map (foldr (.) id) $ 
                  sequence $ map (\x -> [id, x]) [id, rotAllX, rotAllZ, mirror, reverse]

rotAllX :: [[Plank]] -> [[Plank]]
rotAllX (uss:dss:[]) = [reverse $ map rotX dss, reverse $ map rotX uss]

rotAllZ :: [[Plank]] -> [[Plank]]
rotAllZ pss = map (reverse . map rotZ) pss

mirror :: [[Plank]] -> [[Plank]]
mirror (uss:dss:[]) = [map rotZ uss, reverse dss]

equalStates :: [[Plank]] -> [[Plank]] -> Bool
equalStates xss yss = layers xss `elem` map (layers . ($ yss)) equalTransforms

solution :: [Plank] -> [[[Plank]]]
solution ps = nubBy equalStates $ validStates ps

planks = [Plank 1 [Up, Down, Hole],
          Plank 2 [Up, Down, Hole],
          Plank 3 [Up, Hole, Down],
          Plank 4 [Up, Hole, Hole],
          Plank 5 [Up, Down, Down],
          Plank 6 [Up, Down, Up  ]]

main = mapM putStrLn $ map show $ solution planks

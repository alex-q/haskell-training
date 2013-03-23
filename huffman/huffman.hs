-- Huffman compression.

import Data.List
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map

-- A huffman code is represented by a binary tree.

data CodeTree = Fork {left :: CodeTree, right :: CodeTree, chars :: [Char], weight :: Integer} |
                Leaf {char :: Char, weight :: Integer} deriving Show

-- Computes for each unique character in the list the number of times it occurs.

times :: [Char] -> Map Char Integer
times = Map.fromListWith (+) . map (\x -> (x, 1))

ch :: CodeTree -> [Char]
ch Leaf {char = c} = [c]
ch x = chars x

(+++) :: CodeTree -> CodeTree -> CodeTree
(+++) x y = Fork {left = x, right = y,
                  weight = weight x + weight y, 
                  chars = ch x ++ ch y}

-- Takes the first two elements of the list and combines
-- them into a single `Fork` node. This node is then added back into the
-- remaining elements of `trees` at a position such that the ordering by weights
-- is preserved.

combine :: [CodeTree] -> [CodeTree]
combine (x:y:xs) = insertBy (comparing weight) (x +++ y) xs
combine xs = xs

-- Returns a list of `Leaf` nodes for a given frequency table.
-- The returned list should be ordered by ascending weights (i.e. the
-- head of the list should have the smallest weight), where the weight
-- of a leaf is the frequency of the character.

orderedLeafList :: Map Char Integer -> [CodeTree]
orderedLeafList = sortBy (comparing weight) . map makeLeaf . Map.toList

makeLeaf :: (Char, Integer) -> CodeTree
makeLeaf (c, w) = Leaf {char = c, weight = w}

isSingleton :: [CodeTree] -> Bool
isSingleton x = length x == 1

-- Creates a code tree which is optimal to encode the text.

createCodeTree :: [Char] -> CodeTree
createCodeTree xs = let trees = orderedLeafList . times $ xs
                        in last $ until isSingleton combine trees

type Bit = Int

-- Encodes text using the code tree into a sequence of bits.

encode :: CodeTree -> [Char] -> [Bit]
encode tree xs = encode' tree tree xs []

encode' :: CodeTree -> CodeTree -> [Char] -> [Bit] -> [Bit]
encode' _ _ [] acc = acc
encode' tree Leaf {char = c} (x:xs) acc = encode' tree tree xs acc
encode' tree Fork {left = l, right = r} xs acc =
    if head xs `elem` ch r then encode' tree r xs (acc ++ [1])
                           else encode' tree l xs (acc ++ [0])

-- Decodes the bit sequence using the code tree.

decode :: CodeTree -> [Bit] -> [Char]
decode tree bits = decode' tree tree bits []

decode' :: CodeTree -> CodeTree -> [Bit] -> [Char] -> [Char]
decode' _ Leaf {char = c} [] acc = acc ++ [c]
decode' tree Leaf {char = c} bits acc = decode' tree tree bits (acc ++ [c])
decode' _ Fork {left = _, right = _} [] acc = acc
decode' tree Fork {left = l, right = r} (b:bs) acc =
    let node = if b == 0 then l else r
    in decode' tree node bs acc

-- Examples.

text = "this is an example of a huffman tree"
tree = createCodeTree text
encoded = encode tree text
decoded = decode tree encoded
main = do
    putStrLn $ "Encoded: " ++ show encoded
    putStrLn $ "Decoded: " ++ decoded

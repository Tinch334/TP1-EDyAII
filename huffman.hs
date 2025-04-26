import Data.Map
import Heap

data HTree = Leaf Char Int | Node HTree HTree Int deriving Show
type FreqMap = Map Char Int

data Bit = Zero | One deriving Show
type Code = [Bit]
type CodeMap = Map Char Code

getHTreeNum :: HTree -> Int
getHTreeNum (Leaf _ n) = n
getHTreeNum (Node _ _ n) = n

-- Eq instance: trees are equal if they have the same shape and equal stored values
instance Eq HTree where
    t1 == t2 = (getHTreeNum t1) == (getHTreeNum t2)

instance Ord HTree where
    compare t1 t2 = compare (getHTreeNum t1) (getHTreeNum t2)


buildFreqMap :: String -> FreqMap
buildFreqMap str = buildFreqMapInner str (fromList []) where
    buildFreqMapInner "" dict = dict
    buildFreqMapInner (x:xs) dict = buildFreqMapInner xs (insertWith (+) x 1 dict)

makeLeafTreesInner :: [(Char, Int)] -> Heap HTree -> Heap HTree
makeLeafTreesInner [] heap = heap
makeLeafTreesInner ((char, num):xs) heap = makeLeafTreesInner xs (Heap.insert (Leaf char num) heap)

getHtreeFromHeap :: Heap HTree -> HTree
getHtreeFromHeap heap = let (minTree, newHeap) = (Heap.findMin heap, Heap.deleteMin heap) in
    if (Heap.isEmpty newHeap) then
        minTree
    else
        let minTree' = (Heap.findMin newHeap) in
            getHtreeFromHeap (Heap.insert (Node minTree minTree' ((getHTreeNum minTree) + (getHTreeNum minTree'))) (Heap.deleteMin newHeap))


buildHTree :: FreqMap -> HTree
buildHTree fm = getHtreeFromHeap (makeLeafTreesInner (assocs fm) E)


buildCodeMap :: HTree -> CodeMap
buildCodeMap tree = buildCodeMapInner tree (fromList []) [] where
    buildCodeMapInner :: HTree -> CodeMap -> Code -> CodeMap
    buildCodeMapInner (Leaf char _) dict path = (Data.Map.insert char path dict)
    buildCodeMapInner (Node l r _) dict path = Data.Map.union (buildCodeMapInner l dict (path++[Zero])) (buildCodeMapInner r dict (path++[One]))

encode :: CodeMap -> String -> Code
encode cm string = encodeInner cm string [] where
    encodeInner :: CodeMap -> String -> Code -> Code
    encodeInner _ [] encodedString = encodedString
    encodeInner cm' (x:xs) encodedString = case (Data.Map.lookup x cm') of
        Just code -> encodeInner cm' xs (encodedString++code)
        Nothing -> error "Invalid character"


decode :: HTree -> Code -> String
decode tree code = decodeInner tree tree code "" where
    decodeInner :: HTree -> HTree -> Code -> String -> String
    decodeInner _ _ [] decoded = decoded
    decodeInner ot (Leaf char _) code' decoded = decodeInner ot ot code' (decoded++[char])
    decodeInner ot (Node l r _) (x:xs) decoded = case x of
        Zero -> decodeInner ot l xs decoded
        One -> decodeInner ot r xs decoded

treeVar = (buildHTree (buildFreqMap "abcdefghijklmnopqrstuvwxtz "))
codedVar = encode (buildCodeMap treeVar) "hola amigo"
decodedVar = decode treeVar codedVar
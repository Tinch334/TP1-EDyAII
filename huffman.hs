import Data.Map
import Heap

data HTree = Leaf Char Int | Node HTree HTree Int deriving Show
type FreqMap = Map Char Int

-- TODO: quitar la instancia Eq (solo puesta para testear)
data Bit = Zero | One deriving (Show,Eq)
type Code = [Bit]
type CodeMap = Map Char Code

getHTreeNum :: HTree -> Int
getHTreeNum (Leaf _ n) = n
getHTreeNum (Node _ _ n) = n

-- 1
instance Eq HTree where
    t1 == t2 = (getHTreeNum t1) == (getHTreeNum t2)

instance Ord HTree where
    compare t1 t2 = compare (getHTreeNum t1) (getHTreeNum t2)

-- 2
-- characters: keys, node weights: values
buildFreqMap :: String -> FreqMap
buildFreqMap str = buildFreqMapInner str Data.Map.empty where
    buildFreqMapInner "" dict = dict
    buildFreqMapInner (x:xs) dict = buildFreqMapInner xs (insertWith (+) x 1 dict)

-- 3
buildHTree :: FreqMap -> HTree
buildHTree fm = buildCodingTree (initHtreeHeap (assocs fm) E)
    where
        -- Given a list [(k, v)] insert into heap all single noded trees, with root (k, v)
        initHtreeHeap :: [(Char, Int)] -> Heap HTree -> Heap HTree
        initHtreeHeap [] heap = heap
        initHtreeHeap ((char, num) : xs) heap = initHtreeHeap xs (Heap.insert (Leaf char num) heap)

        -- Given an initialized heap, construct coding tree
        buildCodingTree :: Heap HTree -> HTree
        buildCodingTree heap =  let 
                                    findMinAndDel heap = (Heap.findMin heap, Heap.deleteMin heap)
                                    (minTreeA, updatedHeapA) = (findMinAndDel heap) 
                                in
                                    if (Heap.isEmpty updatedHeapA) then
                                        minTreeA -- when a single tree is left in heap => coding tree is done
                                    else
                                        let 
                                            (minTreeB, updatedHeapB) = (findMinAndDel updatedHeapA)
                                            childsTree = (Node minTreeA minTreeB ((getHTreeNum minTreeA) + (getHTreeNum minTreeB)))
                                        in  
                                            buildCodingTree (Heap.insert childsTree updatedHeapB)
-- 4
-- TODO: ver eficiencia
buildCodeMap :: HTree -> CodeMap
buildCodeMap tree = (buildCodeMapInner tree Data.Map.empty []) 
    where
        buildCodeMapInner :: HTree -> CodeMap -> Code -> CodeMap
        buildCodeMapInner (Leaf char _) dict path = (Data.Map.insert char (reverse path) dict) -- ':' inserts at top of the list, but insertion must be made at the bottom
        buildCodeMapInner (Node l r _) dict path = (buildCodeMapInner l (buildCodeMapInner r dict (One : path)) (Zero : path))

-- 5
-- ++ no se vuelve tan costosa cuando la longitud de la primer lista es corta (los code a priori son cortos).
encode :: CodeMap -> String -> Code
encode cm [] = []
encode cm (x : xs) = case (Data.Map.lookup x cm) of
                        (Just code) -> code ++ (encode cm xs)
                        Nothing -> error "Key error: character not found."

-- 6
-- aca si nos sirve agregar los caracteres "por delante" en O(1)
decode :: HTree -> Code -> String
decode tree codedMsg = decodeInner tree tree codedMsg "" 
    where
        decodeInner :: HTree -> HTree -> Code -> String -> String
        decodeInner _ (Leaf char _) [] decoded = [char]
        decodeInner treeRef (Leaf char _) code decoded = char : (decodeInner treeRef treeRef code decoded) -- agregar caracter y volver a buscar
        decodeInner treeRef (Node l r _) (x:xs) decoded = 
            case x of
                Zero -> decodeInner treeRef l xs decoded
                One -> decodeInner treeRef r xs decoded


-- testing
treeVar = (buildHTree (buildFreqMap "abcdefghijklmnopqrstuvwxtz "))
codedVar = encode (buildCodeMap treeVar) "hola amigo"
decodedVar = decode treeVar codedVar

freqmap = buildFreqMap "apa la papaleta"
arb = Node (Leaf 'a' 6) (Node (Node (Node (Leaf 't' 1) (Leaf 'e' 1) 2) (Leaf ' ' 2) 4) (Node (Leaf 'l' 2) (Leaf 'p' 3) 5) 9) 15
assert1 = (arb == (buildHTree freqmap))
cm = buildCodeMap arb
str = "taleeeetlp"
encodedStr = encode cm str
assert2 = ([One,Zero,Zero,Zero,Zero,One,One,Zero,One,Zero,Zero,One,One,Zero,Zero,One,One,Zero,Zero,One,One,Zero,Zero,One,One,Zero,Zero,Zero,One,One,Zero,One,One,One] == encodedStr)
decodedStr = decode arb encodedStr

assert3 = (decodedStr == str)

-- TODO: sacar esto!!!!
-- poner main en ghci para correr
main = putStrLn ("" ++ show assert1 ++ "\n" ++ show assert2 ++ "\n" ++ show assert3)

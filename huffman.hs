module Huffman where

import Data.Map as DM
import Data.Char as DC
import Heap

{-
Integrantes grupo:
Bertoni Juan Ignacio
Goñi Martín
-}

-- Bits y códigos

data Bit = Zero | One deriving (Eq, Show)

type Code = [Bit]

-- Árbol de codificación

data HTree = Leaf Char Int
           | Node HTree HTree Int
           deriving Show

weight :: HTree -> Int
weight (Leaf _ w)   = w
weight (Node _ _ w) = w

instance Eq HTree where
    t1 == t2 = (weight t1) == (weight t2)

instance Ord HTree where
    compare t1 t2 = compare (weight t1) (weight t2)

-- Diccionarios de frecuencias y códigos

type FreqMap = Map Char Int

type CodeMap = Map Char Code


-- Ejercicio 1


-- Ejercicio 2

buildFreqMap :: String -> FreqMap
buildFreqMap str = buildFreqMapInner str DM.empty where
    buildFreqMapInner "" dict = dict
    buildFreqMapInner (x:xs) dict = buildFreqMapInner xs (insertWith (+) x 1 dict)

-- Ejercicio 3

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
                                            childsTree = (Node minTreeA minTreeB ((weight minTreeA) + (weight minTreeB)))
                                        in  
                                            buildCodingTree (Heap.insert childsTree updatedHeapB)

-- Ejercicio 4

buildCodeMap :: HTree -> CodeMap
buildCodeMap tree = (buildCodeMapInner tree DM.empty []) 
    where
        buildCodeMapInner :: HTree -> CodeMap -> Code -> CodeMap
        buildCodeMapInner (Leaf char _) dict path = (DM.insert char (reverse path) dict) -- ':' inserts at top of the list, but insertion must be made at the bottom
        buildCodeMapInner (Node l r _) dict path = (buildCodeMapInner l (buildCodeMapInner r dict (One : path)) (Zero : path))

-- Ejercicio 5

encode :: CodeMap -> String -> Code
encode cm [] = []
encode cm (x : xs) = case (DM.lookup x cm) of
                        (Just code) -> code ++ (encode cm xs)
                        Nothing -> error "Key error: character not found."

-- Ejercicio 6

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

-- Ejercicio 7

engFM :: FreqMap
engFM = fromList [
    ('a', 691),
    ('b', 126),
    ('c', 235),
    ('d', 360),
    ('e', 1074),
    ('f', 188),
    ('g', 170),
    ('h', 515),
    ('i', 589),
    ('j', 13),
    ('k', 65),
    ('l', 340),
    ('m', 203),
    ('n', 571),
    ('o', 635),
    ('p', 163),
    ('q', 8),
    ('r', 506),
    ('s', 535),
    ('t', 766),
    ('u', 233),
    ('v', 83),
    ('w', 200),
    ('x', 13),
    ('y', 167),
    ('z', 6),
    (' ', 1370),
    (',', 84),
    ('.', 89)
    ]

-- Arbol de codificacion para engFM
engTree :: HTree
engTree = buildHTree engFM

cleanString :: String -> String
cleanString str = let lowercaseStr = Prelude.map toLower str in Prelude.filter (\x -> notElem x [';', '-', '?', ':', '/', '(', ')']) lowercaseStr

testStrings :: [String]
testStrings = [
    "Virtual machines offer an interesting solution to a problem that has long plagued users, especially users of open source software: how to install new appli- cation programs. The problem is that many applications are dependent on numerous other applications and libraries, which are themselves dependent on a host of other software packages, and so on. Furthermore, there may be dependencies on particular versions of the compilers, scripting languages, and the operating system.",

    "They pre-emptively evacuated some regions in the South East so that civilian evacuations would not foul up British logistics and manoeuvre. The population was told that evacuations would take a lower priority and, if not already evacuated by the time of an invasion, they should stay where they were unless ordered to retreat.",

    "A sample of the signal at the collector of TR is peak rectified by D, DB and C to turn off TR. If for any reason (i.e. no input signal) the trigger stage is not producing pulses then TR conducts, causing the timebase to free run. Immediately trigger pulses are produced, TR is turned off and the timebase reverts to the triggered state.",

    "British officials found Koops analysis highly impressive. Arthur Kellas, a British diplomat, had acquired a copy of the study and in his forwarding letter observed that it was a model of what these things should be. Treweeks, with the Defence Intelligence Staff, later commended the Canadian intelligence study, declaring that we agreed with what is said and with the conclusions. Apparently the report had not been shared with U.S. intelligence",

    "The meetings of the General Conference of Representatives of the Members shall be held from time to time as occasion may require, and at least once in every year. It shall be composed of four Representatives of each of the Members, of whom two shall be Government Delegates and the two others shall be Delegates representing respectively the employers and the workers of each of the Members",

    "The period of office of the members of the Governing Body will be three years. The method of filling vacancies and other similar questions may be determined by the Governing Body subject to the approval of the Conference.",

    "It is in Latin administrative documents of the ninth century that written GalicianPortuguese words and phrases are first recorded. This phase is known as Proto-Portuguese, which lasted from the ninth century until the twelfth-century independence of the County of Portugal from the Kingdom of Leon, which had by then assumed reign over Galicia.",
    
    "In reconstructing the history of the Indo-European languages and the form of the Proto-Indo-European language, some languages have been of particular importance. These generally include the ancient Indo-European languages that are both well-attested and documented at an early date, although some languages from later periods are important if they are particularly linguistically conservative (most notably, Lithuanian). Early poetry is of special significance because of the rigid poetic meter normally employed, which makes it possible to reconstruct a number of features (e.g. vowel length) that were either unwritten or corrupted in the process of transmission down to the earliest extant written manuscripts.",

    "After another revolt in June, the constitution was suspended and power passed from the National Convention to the Committee of Public Safety. About six thousand people were executed in a Reign of Terror, which ended in July. Weakened by external threats and internal opposition, the Republic was replaced by the Directory. Four years later, the Consulate seized power in a coup led by Napoleon.",

    "It was the twelfth day of August, in the ninth consulship of Diocletian and the eighth of Maximian, when the governor Calvisianus said to Euplius under torture: What now do you repeat with regard to the things you admitted in your confession?",

    "abcdefghijklmnopqrstuvwxtz ,."
    ]

cleanStrings :: [String]
cleanStrings = Prelude.map cleanString testStrings
codedStrings :: [Code]
codedStrings = Prelude.map (encode (buildCodeMap engTree)) cleanStrings
codedStringLength :: [Int]
codedStringLength = Prelude.map length codedStrings
notCodedStringLength :: [Int]
notCodedStringLength = Prelude.map (\x -> (length x) * 5) cleanStrings

{-
Si bien el usar la codificación de Huffman ahorra espacio en la mayoría de los casos cuantos más caracteres distintos tiene la string a
Codificar menos espacio se ahorra; En el peor de los casos(Están todos los caracteres en la string) la versión codificada ocupa más espacio.
Esto hace que el algoritmo no sea ideal para codificar textos largos o con mucha varianza.
-}
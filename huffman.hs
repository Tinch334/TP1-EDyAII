module Huffman where

import Data.Map as DM
import Data.Char as DC -- usada para testeo
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

-- Diccionarios de frecuencias y códigos

type FreqMap = Map Char Int

type CodeMap = Map Char Code

-- Ejercicio 1
instance Eq HTree where
    t1 == t2 = (weight t1) == (weight t2)

instance Ord HTree where
    compare t1 t2 = compare (weight t1) (weight t2)

-- Ejercicio 2
-- Usamos la funcion insertWith
-- Si la key k no existia en el dict, se agrega (k, 1) al dict
-- Si la key k ya existia, entonces se actualiza la entrada (k, v) por (k, v + 1) en dict
-- Arrancamos de un dict vacio, y vamos aplicando de derecha a izquierda la funcion lambda que inserta el par key value o lo actualiza
buildFreqMap :: String -> FreqMap
buildFreqMap = Prelude.foldl (\dict -> \ch -> DM.insertWith (+) ch 1 dict) DM.empty

-- Ejercicio 3
-- Usamos assocs, para convertir el diccionario en una lista que podamos recorrer.

buildHTree :: FreqMap -> HTree
buildHTree fm = buildCodingTree (initHtreeHeap (assocs fm))
    where
        -- Dada una lista [(k, v)], insertar en el heap arboles de un unico elemento, de raiz (k, v)
        -- La funcion queda parcialmente instanciada, esperando la lista [(k,v)]
        initHtreeHeap :: [(Char, Int)] -> Heap HTree
        initHtreeHeap = Prelude.foldl (\h -> \(char, num) -> Heap.insert (Leaf char num) h) Heap.empty

        -- Dado un heap inicializado, construir el arbol de codificacion
        buildCodingTree :: Heap HTree -> HTree
        buildCodingTree heap =  let 
                                    findMinAndDel heap = (Heap.findMin heap, Heap.deleteMin heap)
                                    (minTreeA, updatedHeapA) = (findMinAndDel heap) 
                                in
                                    if (Heap.isEmpty updatedHeapA) then
                                        minTreeA -- queda un unico arbol en el heap => el arbol de codificacion esta listo
                                    else
                                        let 
                                            (minTreeB, updatedHeapB) = (findMinAndDel updatedHeapA)
                                            childsTree = (Node minTreeA minTreeB ((weight minTreeA) + (weight minTreeB)))
                                        in  
                                            buildCodingTree (Heap.insert childsTree updatedHeapB)

-- Ejercicio 4
-- Vamos llevando la traza actual en la funcion buildCodeMapInner
-- Cuando buscamos por la izquierda -> añadimos un 0, cuando buscamos por derecha -> añadimos un 1
-- Cuando llegamos a una hoja, asociamos el valor del caracter en la hoja a la traza actual
-- Si usamos (:) los elementos son añadidos al principio de la lista, pero realmente la traza la queremos construir insertando por atras
-- Optamos por revertir el path al llegar a una hoja, en lugar de usar (++) cada vez que añadimos un elemento a la traza
buildCodeMap :: HTree -> CodeMap
buildCodeMap tree = (buildCodeMapInner tree DM.empty []) 
    where
        buildCodeMapInner :: HTree -> CodeMap -> Code -> CodeMap
        buildCodeMapInner (Leaf char _) dict path = (DM.insert char (reverse path) dict)
        buildCodeMapInner (Node l r _) dict path =  let 
                                                        dictR = buildCodeMapInner r dict (One : path) 
                                                    in
                                                        buildCodeMapInner l dictR (Zero : path)

-- Ejercicio 5
encode :: CodeMap -> String -> Code
encode cm str = concat (Prelude.map getChar str)
    where
        getChar :: Char -> Code
        getChar c = case (DM.lookup c cm) of
                        (Just charcode) -> charcode
                        Nothing -> error "Key error: character not found."

-- Ejercicio 6
-- Recorremos el arbol siguiendo la traza hasta que nos encontremos con una hoja.
-- Al encontrarnos una hoja, añadimos el caracter de ella a la solucion y reseteamos el arbol (arrancamos de nuevo 
-- desde la raiz) para seguir procesando la traza.
decode :: HTree -> Code -> String
decode tree codedMsg = decodeInner tree codedMsg 
    where
        decodeInner :: HTree -> Code -> String
        decodeInner (Leaf c _) [] = [c]
        decodeInner (Leaf c _) str = c : (decodeInner tree str)
        decodeInner (Node l _ _) (Zero:str) = decodeInner l str
        decodeInner (Node _ r _) (One:str) = decodeInner r str
        decodeInner _ [] = []

-- Ejercicio 7
engFM :: FreqMap
engFM = DM.fromList [
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

-- Funcion auxiliar para pasar las letras a minuscula y quitar caracteres indeseados de los strings usados para testear
cleanString :: String -> String
cleanString str =   let 
                        lowercaseStr = Prelude.map toLower str
                        list = [';', '-', '?', ':', '/', '(', ')']
                    in 
                        Prelude.filter (\x -> not (elem x list)) lowercaseStr

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
notCodedStringLength = Prelude.map (\testStr -> (length testStr) * 5) cleanStrings

{-
Si bien el usar la codificación de Huffman ahorra espacio en la mayoría de los casos, cuantos más caracteres distintos tiene la string a
codificar menos espacio se ahorra; En el peor de los casos (Están todos los caracteres del alfabeto en la string) la versión codificada ocupa
más espacio. Esto ocurre porque en Huffman al ser los códigos de longitud variable, el árbol de codificacion resultante puede no estar perfectamente
balanceado, llevando a que algunos caracteres posean un codigo de longitud mayor a 5 bits.
Esto hace que el algoritmo no sea ideal para codificar textos largos o con mucha varianza.
-}

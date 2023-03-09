module Modular where

import Data.List
import Data.Function (on)
import qualified Data.Set as S
import AlgebraicGraph

type Graph a = AlgebraicGraph a

-- Graful descris în diagrama din enunțul temei
diagram :: AlgebraicGraph Int
diagram = ((1*2) * (3+4)) * 5

{-
    O partiție este o mulțime de submulțimi ale unei alte mulțimi, disjuncte
    (fără elemente comune) și care împreună conțin toate elementele originale.
    
    De exemplu, pentru mulțimea [1,2,3], o posibilă partiție este [[1], [2,3]].
-}
type Partition a = S.Set (S.Set a)

{-
    *** TODO ***

    Aplică o funcție pe fiecare element al unei liste, însă doar pe unul singur
    la un moment dat, păstrându-le pe celalte nemodificate. Prin urmare, pentru
    fiecare element din lista inițială rezultă câte o listă în lista finală,
    aferentă modificării doar a acelui element.

    Exemplu:

    > mapSingle (+ 10) [1,2,3]
    [[11,2,3],[1,12,3],[1,2,13]]
-}
replace_index :: [a] -> Int -> (a -> a) -> [a]
replace_index list n f = if (n == 1)
                        then [f (head list)]
                        else (head list) : (replace_index (tail list) (n - 1) f)
                        
mapSingle :: (a -> a) -> [a] -> [[a]]
mapSingle f xs = map (\elem -> let first = (replace_index xs elem f)
                                   second = (drop elem xs) in first ++ second) [x | x <- [1 .. (length xs)]]


{-
    *** TODO ***

    Determină lista tuturor partițiilor unei liste. Deși mai sus tipul
    Partition a este definit utilizând mulțimi, aici utilizăm liste,
    pentru simplitate.

    Dacă vi se pare greu de urmărit tipul întors de funcție, cu 3 niveluri
    ale constructorului de tip listă, gândiți-vă așa:
    - avem nevoie de un nivel pentru o submulțime
    - încă un nivel pentru o partiție, care este o mulțime de submulțimi
    - încă un nivel pentru mulțimea tuturor partițiilor.

    Hint: Folosiți list comprehensions pentru a răspunde la întrebarea:
    dacă am obținut o partiție a restului listei, cum obținem o partiție
    a întregii liste, care include capul? (folosiți și mapSingle)

    Exemple:

    > partitions [2,3]
    [[[2],[3]],[[2,3]]]

    > partitions [1,2,3]
    [[[1],[2],[3]],[[1,2],[3]],[[2],[1,3]],[[1],[2,3]],[[1,2,3]]]
-}
create_partitions :: a -> [[[a]]] -> [[[a]]]
create_partitions elem [[[]]] = [[[elem]]]
create_partitions elem parts = concat (map (\part -> (mapSingle (elem:) part ++ [[elem] : part])) parts)

partitions_rec :: [a] -> [[[a]]] -> [[[a]]]
partitions_rec xs part = if (null xs)
                           then part
                           else partitions_rec (tail xs) (create_partitions (head xs) part)

partitions :: [a] -> [[[a]]]
partitions xs = partitions_rec (reverse xs) [[[]]]

{-
    *** TODO ***

    Verifică dacă o mulțime este un modul, i.e. dacă toate nodurile din mulțime
    au aceeași mulțime de vecini out și aceeași mulțime de vecini in,
    în exteriorul mulțimii de plecare. Cu alte cuvinte, excludem din verificare
    vecinii din interiorul mulțimii de plecare.

    Hint: S.map poate reduce dimensiunea unei mulțimi dacă elemente diferite
    inițial sunt asociate cu același element final, întrucât nu pot exista
    duplicate.

    Exemple:

    > isModule (S.fromList [1,2,3,4]) diagram
    True

    > isModule (S.fromList [5]) diagram
    True
    
    > isModule (S.fromList [1,2]) diagram
    True

    > isModule (S.fromList [3,4]) diagram
    True

    > isModule (S.fromList [1,3]) diagram
    False
-}
create_neighbours_set :: Ord a
         => S.Set a
         -> Graph a
         -> S.Set (S.Set a, S.Set a)
create_neighbours_set set graph = (S.map
                            (\node ->
                                ((S.difference (inNeighbors node graph) set), (S.difference (outNeighbors node graph) set)))
                                set)

isModule :: Ord a
         => S.Set a
         -> Graph a
         -> Bool
isModule set graph = let set_sizes = create_neighbours_set set graph in if (S.size set_sizes /= 1) then False else True

{-
    *** TODO ***

    Verifică dacă o partiție a mulțimii de noduri constituie o descompunere
    modulară. Partiția este reprezentată ca o mulțime de mulțimi.

    Hint: la fel ca la isModule.

    Exemple:

    > isModularPartition
        (S.fromList [S.fromList [1], S.fromList [2],
                     S.fromList [3], S.fromList [4], S.fromList [5]])
        diagram
    True

    > isModularPartition (S.fromList [S.fromList [1,2,3,4,5]]) diagram
    True

    > isModularPartition (S.fromList [S.fromList [1,2,3,4], S.fromList [5]])
                         diagram
    True

    > isModularPartition
        (S.fromList [S.fromList [1,2], S.fromList [3,4],
                     S.fromList [5]])
        diagram
    True

    > isModularPartition (S.fromList [S.fromList [1,3], S.fromList [2,4,5]])
                         diagram
    False
-}
isModularPartition :: Ord a
                   => Partition a
                   -> Graph a
                   -> Bool
isModularPartition partition graph = let set_bool = S.map (\p -> (isModule p graph)) partition in
                                        if (S.size set_bool /= 1)
                                            then False
                                            else if (S.member True set_bool)
                                                then True
                                                else False

{-
    *** TODO ***

    Determină partiția maximală dintr-o listă de partiții. Partiția maximală
    conține cele mai acoperitoare submulțimi ale mulțimii de noduri. Cu alte
    cuvinte, partiția maximală conține cel mai mic număr de submulțimi mai mare
    strict decât 1, pentru a exlcude partiția care conține doar întreaga mulțime
    de noduri.

    Hint: minimumBy din Data.List. Funcția este folosită pentru a stabili
    un criteriu ad hoc de ordonare, conform valorii întoarse de o funcție f
    când este aplicată pe elementele listei, printr-o construcție de forma:

    minimumBy (compare `on` f) lista.

    Exemple:

    > maximalModularPartition <lista partițiilor> diagram
    fromList [fromList [1,2,3,4],fromList [5]]

    > maximalModularPartition <lista partițiilor> $ removeNode 5 diagram
    fromList [fromList [1,2],fromList [3,4]]
-}
filter_partitions :: Ord a
                        => [Partition a]
                        -> Graph a
                        -> [Partition a]
filter_partitions partitions graph = filter (\part -> (isModularPartition part graph)) partitions
                   
maximalModularPartition :: Ord a
                        => [Partition a]
                        -> Graph a
                        -> Partition a
maximalModularPartition partitions graph = if (length partitions <= 1)
                                                then S.empty
                                                else minimumBy (compare `on` S.size) (filter (\part -> (S.fromList [(nodes graph)]) /= part) (filter_partitions partitions graph))

{-
    Obține descompunerea modulară a unui graf. O puteți utiliza pentru
    a experimenta manual cu maximalModularPartition.
    
    Exemple:

    > modularlyDecompose diagram                        
    fromList [fromList [1,2,3,4],fromList [5]]

    > modularlyDecompose $ removeNode 5 diagram
    fromList [fromList [1,2],fromList [3,4]]
-}
modularlyDecompose :: Ord a
                   => Graph a
                   -> Partition a
modularlyDecompose graph = maximalModularPartition partList graph
  where
    parts = partitions $ S.toList $ nodes graph
    partList = map (S.fromList . map S.fromList) parts
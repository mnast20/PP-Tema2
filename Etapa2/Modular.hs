module Modular where

import Data.List
import Data.Function (on)
import qualified Data.Set as S
import StandardGraph

type Graph a = StandardGraph a

{-
    O partiție este o mulțime de submulțimi ale unei alte mulțimi, disjuncte
    (fără elemente comune) și care împreună conțin toate elementele originale.
    
    De exemplu, pentru mulțimea [1,2,3], o posibilă partiție este [[1], [2,3]].

    Va fi folosită în etapa 3.
-}
type Partition a = S.Set (S.Set a)

{-
    *** TODO ***

    Aplică o funcție pe fiecare element al unei liste, însă doar pe unul singur
    la un moment dat, păstrându-le pe celalte nemodificate. Prin urmare, pentru
    fiecare element din lista inițială rezultă câte o listă în lista finală,
    aferentă modificării doar a acelui element.

    Exemplu:

    > mapSingle (+10) [1,2,3]
    [[11,2,3],[1,12,3],[1,2,13]]
-}

replace_index :: [a] -> Int -> (a -> a) -> [a]
replace_index list n f = if (n == 1)
                        then [f (head list)] -- Apply function
                        else (head list) : (replace_index (tail list) (n - 1) f) -- Keep list as it is
                        
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
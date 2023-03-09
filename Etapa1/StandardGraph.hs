{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns, S.fromList es)

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = (\graph -> fst graph)

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = (\graph -> snd graph)

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outEdges :: Ord a => a -> StandardGraph a -> S.Set (a, a)
outEdges node graph = (S.filter (\pair -> (fst pair) == node)  (edges graph))

outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = S.map (\pair -> snd pair) (outEdges node graph)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inEdges :: Ord a => a -> StandardGraph a -> S.Set (a, a)
inEdges node graph = (S.filter (\pair -> (snd pair) == node)  (edges graph))

inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = S.map (\pair -> fst pair) (inEdges node graph)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
nonEdges :: Ord a => a -> StandardGraph a -> [(a, a)]
nonEdges node graph = filter (\graphEdge -> (fst graphEdge) /= node && (snd graphEdge) /= node) (S.toList (edges graph))

removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph = fromComponents (filter (\graphNode -> graphNode /= node) (S.toList (nodes graph))) (nonEdges node graph)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
replaceEdges :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> [(a, a)]
replaceEdges old news graph = (++) (concat (map (\pair -> (map (\x -> (fst pair, x)) news)) (S.toList (inEdges old graph))))
                                   (concat (map (\pair -> (map (\x -> (x, snd pair)) news)) (S.toList (outEdges old graph))))

splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news graph = fromComponents ((filter (\graphNode -> graphNode /= old) (S.toList (nodes graph))) ++ news)
                                          ((replaceEdges old news graph) ++ (nonEdges old graph))

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
removeNodesProp :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> [a]
removeNodesProp prop node graph = (filter (\graphNode -> not (prop graphNode)) (S.toList (nodes graph)))

mergeEdges :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> [(a, a)]
mergeEdges prop node graph = (map (\pair -> if (prop (fst pair)) then
                                                if (prop (snd pair))
                                                    then (node, node)
                                                    else (node, (snd pair))
                                                else
                                                    ((fst pair), node))
                                (filter (\graphEdge -> (prop (fst graphEdge)) || (prop (snd graphEdge))) (S.toList (edges graph))))

mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph = fromComponents (let new_nodes = (removeNodesProp prop node graph)
                                                in (if ((length new_nodes) == (length (S.toList (nodes graph))))
                                                    then new_nodes
                                                    else (node : new_nodes)))
                                                ((mergeEdges prop node graph) ++
                                                (filter (\graphEdge -> (not (prop (fst graphEdge))) && (not (prop (snd graphEdge)))) (S.toList (edges graph))))
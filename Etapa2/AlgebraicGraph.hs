module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes (Empty) = S.empty
nodes (Node node) = S.singleton node
nodes (Overlay a b) = S.union (nodes a) (nodes b)
nodes (Connect a b) = S.union (nodes a) (nodes b)
-- nodes graph = S.union

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges (Empty) = S.empty
edges (Node node) = S.empty
edges (Overlay a b) = S.union (edges a) (edges b)
edges (Connect a b) = S.union (S.cartesianProduct (nodes a) (nodes b)) (S.union (edges a) (edges b))

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node (Empty) = S.empty
outNeighbors node (Node a) = S.empty
outNeighbors node (Overlay a b) = S.union (outNeighbors node a) (outNeighbors node b)
outNeighbors node (Connect a b) = let fromNodes = nodes a
                                      toNodes = nodes b in
                             if ((S.lookupIndex node fromNodes) /= Nothing)
                                then S.union toNodes (S.union (outNeighbors node a) (outNeighbors node b)) -- node is part of FromNodes, so add to OUTNeighbours
                                else S.union (outNeighbors node a) (outNeighbors node b)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node (Empty) = S.empty
inNeighbors node (Node a) = S.empty
inNeighbors node (Overlay a b) = S.union (inNeighbors node a) (inNeighbors node b)
inNeighbors node (Connect a b) = let fromNodes = nodes a
                                     toNodes = nodes b in
                             if ((S.lookupIndex node toNodes) /= Nothing) 
                                then S.union fromNodes (S.union (inNeighbors node a) (inNeighbors node b)) -- node is part of ToNodes, so add to INNeighbours
                                else S.union (inNeighbors node a) (inNeighbors node b)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node (Empty) = Empty
removeNode node (Node a) = if (a == node)
                                then Empty
                                else Node a
removeNode node (Overlay a b) = let new_a = removeNode node a
                                    new_b = removeNode node b in
                                if new_a == Empty
                                    then  new_b -- Only The New_B Node Remains
                                    else if new_b == Empty
                                        then new_a -- Only Only The New_A Node Remains
                                        else Overlay new_a new_b
removeNode node (Connect a b) = let new_a = removeNode node a
                                    new_b = removeNode node b in
                                if new_a == Empty -- Only Only The New_B Node Remains
                                    then  new_b
                                    else if new_b == Empty
                                        then new_a -- Only Only The New_A Node Remains
                                        else Connect new_a new_b

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
createOverlay :: [a]-> AlgebraicGraph a
createOverlay nodes = if (length nodes == 0)
                        then Empty
                        else if ((length nodes) == 1)
                            then Node (head nodes) -- Only 1 node
                            else Overlay (Node (head nodes)) (createOverlay (tail nodes))

splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
          

splitNode old news (Empty) = Empty
splitNode old news (Node a) = if (a == old)
                                then createOverlay news
                                else Node a -- Node remains unchanged
splitNode old news (Overlay a b) = Overlay (splitNode old news a) (splitNode old news b)
splitNode old news (Connect a b) = Connect (splitNode old news a) (splitNode old news b)

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node (Empty) = Empty
mergeNodes prop node (Node a) = if (prop a)
                                then Node node
                                else Node a -- Node remains unchanged
mergeNodes prop node (Overlay a b) = let new_a = (mergeNodes prop node a)
                                         new_b = (mergeNodes prop node b) in
                                         Overlay new_a new_b
mergeNodes prop node (Connect a b) = let new_a = (mergeNodes prop node a)
                                         new_b = (mergeNodes prop node b) in
                                     Connect new_a new_b

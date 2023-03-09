module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    


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
                                then S.union toNodes (S.union (outNeighbors node a) (outNeighbors node b))
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
                                then S.union fromNodes (S.union (inNeighbors node a) (inNeighbors node b))
                                else S.union (inNeighbors node a) (inNeighbors node b)


{-
    *** TODO ***

    Instanțiați clasa Num cu tipul (AlgebraicGraph a), astfel încât:
    - un literal întreg să fie interpretat ca un singur nod cu eticheta egală
      cu acel literal
    - operația de adunare să fie intepretată ca Overlay
    - operația de înmulțire să fie interpretată drept Connect.

    Celelalte funcții din clasă nu sunt relevante. Veți obține warning-uri
    pentru neimplementarea lor, dar puteți să le ignorați.

    După instanțiere, veți putea evalua în consolă expresii ca:

    > 1 :: AlgebraicGraph Int
    Node 1
    
    > 1*(2+3) :: AlgebraicGraph Int
    Connect (Node 1) (Overlay (Node 2) (Node 3))
-}
instance Num a => Num (AlgebraicGraph a) where
    fromInteger 0 = Empty
    fromInteger int = Node (Prelude.fromInteger int)
    Empty + graph = graph
    Node a + graph = Overlay (Node a) graph
    (Overlay a b) + graph = Overlay (Overlay a b) graph
    (Connect a b) + graph = Overlay (Connect a b) graph
    Empty * graph = graph
    Node a * graph = Connect (Node a) graph
    (Overlay a b) * graph = Connect (Overlay a b) graph
    (Connect a b) * graph = Connect (Connect a b) graph
    
    

{-
    *** TODO ***

    Instanțiați clasa Show cu tipul (AlgebraicGraph a), astfel încât
    reprezentarea sub formă de șir de caractere a unui graf să reflecte
    expresiile aritmetice definite mai sus. Puteți pune un nou rând de paranteze
    la fiecare subexpresie compusă.

    Exemple:

    > Node 1
    1

    > Connect (Node 1) (Overlay (Node 2) (Node 3))
    (1*(2+3))
-}
createString :: [String] -> String
createString list = if (null list)
                        then ""
                        else (head list) ++ createString (tail list)

instance Show a => Show (AlgebraicGraph a) where
    show Empty = show 0
    show (Node a) = show a
    show (Overlay a b) = "(" ++ (show a) ++ "+" ++ (show b) ++ ")"
    show (Connect a b) = "(" ++ (show a) ++ "*" ++ (show b) ++ ")"

{-
    *** TODO ***

    Observați că instanța predefinită de Eq pentru tipul (AlgebraicGraph a)
    nu surprinde corect egalitatea a două grafuri, deoarece același graf
    conceptual poate avea două descrieri simbolice diferite.
    
    Prin urmare, instanțiați clasa Eq cu tipul (AlgebraicGraph a), astfel încât
    să comparați propriu-zis mulțimile de noduri și de arce.

    Exemple:

    > Node 1 == 1
    True

    > Node 1 == 2
    False
1*
    > angle == 1*2 + 1*3
    True

    > triangle == (1*2)*3
    True
-}
instance Ord a => Eq (AlgebraicGraph a) where
    graph1 == graph2 = (nodes graph1 == nodes graph2) && (edges graph1 == edges graph2)

{-
    *** TODO ***

    Extinde un graf existent, atașând noi subgrafuri arbitrare în locul nodurilor
    individuale. Funcția primită ca prim parametru determină această
    corespondență între noduri și subgrafuri. Observați că tipul etichetelor
    noi (b) poate diferi de al etichetelor vechi (a).

    Exemplu:

    > extend (\n -> if n == 1 then 4+5 else Node n) $ 1*(2+3)
    ((4+5)*(2+3))
-}
extend :: (a -> AlgebraicGraph b) -> AlgebraicGraph a -> AlgebraicGraph b
extend f (Node a) = f a
extend f (Overlay a b) = Overlay (extend f a) (extend f b)
extend f (Connect a b) = Connect (extend f a) (extend f b)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Implementați splitNode folosind extend!
-}
createOverlay :: [a]-> AlgebraicGraph a
createOverlay nodes = if (length nodes == 0)
                        then Empty
                        else if ((length nodes) == 1)
                            then Node (head nodes)
                            else Overlay (Node (head nodes)) (createOverlay (tail nodes))

splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode node targets = (\graph -> let new_overlay = createOverlay targets in
                                        extend (\elem -> if elem == node then new_overlay else Node elem) graph)

{-
    *** TODO ***

    Instanțiați clasa Functor cu constructorul de tip AlgebraicGraph, astfel
    încât să puteți aplica o funcție pe toate etichetele unui graf.
    fmap reprezintă generalizarea lui map pentru orice fel de structură.

    Implementați fmap folosind extend!

    Exemplu:

    > fmap (+ 10) $ 1*(2+3) :: AlgebraicGraph Int
    (11*(12+13))
-}

instance Functor AlgebraicGraph where
    -- fmap :: (a -> b) -> AlgebraicGraph a -> AlgebraicGraph b
    fmap f graph = extend (\elem -> Node (f elem)) graph

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Implementați mergeNodes folosind fmap!
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node = (\graph -> fmap (\elem -> if (prop elem)
                                                    then node
                                                    else elem) graph)

{-
    *** TODO ***

    Filtrează un graf, păstrând doar nodurile care satisfac proprietatea dată.

    Implementați filterGraph folosind extend!
    
    Exemplu:

    > nodes $ filterGraph odd $ 1*(2+3)
    fromList [1,3]

    > edges $ filterGraph odd $ 1*(2+3)
    fromList [(1,3)]
-}

filterGraph :: (a -> Bool) -> AlgebraicGraph a -> AlgebraicGraph a
filterGraph prop graph = extend (\elem -> if (prop elem) then Node elem else Empty)graph

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Implementați removeNode folosind filterGraph!
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = filterGraph (\elem -> if (elem == node) then False else True) graph
module Algorithms where

import qualified Data.Set as S
import StandardGraph

{-
    În etapa 1, prin graf înțelegem un graf cu reprezentare standard.
    În etapele următoare, vom experimenta și cu altă reprezentare.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Graph a = StandardGraph a

{-
    *** TODO ***

    Funcție generală care abstractizează BFS și DFS pornind dintr-un anumit nod,
    prin funcția de îmbinare a listelor care constituie primul parametru.
    
    Cele două liste primite ca parametru de funcția de îmbinare sunt lista
    elementelor deja aflate în structură (coadă/stivă), respectiv lista
    vecinilor nodului curent, proaspăt expandați.

    Căutarea ar trebui să țină cont de eventualele cicluri.

    Hint: Scrieți o funcție auxiliară care primește ca parametru suplimentar
    o mulțime (set) care reține nodurile vizitate până în momentul curent.
-}

front :: [a] -> a
front list = head list

remove_front :: [a] -> [a]
remove_front list = drop 1 list

enqueue :: [a] -> [a] -> [a]
enqueue list queue = queue ++ list

push :: [a] -> [a] -> [a]
push list stack = list ++ stack

remove_visited :: Ord a
              => S.Set a
              -> S.Set a
              -> [a]
remove_visited neighbours visited = S.toList (S.difference neighbours visited)

search_rec :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- structura ca lista (stiva/coada)
       -> S.Set a              -- set de noduri deja vizitate
       -> [a]                  -- lista obținută în urma parcurgerii
search_rec f node graph structure visited = if (not (null structure)) then
                                                (let current_node = (front structure)
                                                     new_structure = (remove_front structure)
                                                     removed_visited = (remove_visited (outNeighbors current_node graph) visited)
                                                 in (if (S.member current_node visited) then
                                                        (search_rec f current_node graph (f removed_visited new_structure) visited)
                                                        else current_node :
                                                            (search_rec f current_node graph (f removed_visited new_structure) (S.insert current_node visited))))
                                                else []
                                    

search :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f node graph = search_rec f node graph (node : []) S.empty

{-
    *** TODO ***

    Strategia BFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}
bfs :: Ord a => a -> Graph a -> [a]
bfs = (\node -> (\graph -> (search enqueue node graph)))

{-
    *** TODO ***

    Strategia DFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > dfs 1 graph4 
    [1,2,4,3]
    
    > dfs 4 graph4
    [4,1,2,3]
-}
dfs :: Ord a => a -> Graph a -> [a]
dfs = (\node -> (\graph -> (search push node graph)))

{-
    *** TODO ***

    Funcția numără câte noduri intermediare expandează strategiile BFS,
    respectiv DFS, în încercarea de găsire a unei căi între un nod sursă
    și unul destinație, ținând cont de posibilitatea absenței acesteia din graf.
    Numărul exclude nodurile sursă și destinație.

    Modalitatea uzuală în Haskell de a preciza că o funcție poate să nu fie
    definită pentru anumite valori ale parametrului este constructorul de tip
    Maybe. Astfel, dacă o cale există, funcția întoarce
    Just (numărBFS, numărDFS), iar altfel, Nothing.

    Hint: funcția span.

    Exemple:

    > countIntermediate 1 3 graph4
    Just (1,2)

    Aici, bfs din nodul 1 întoarce [1,2,3,4], deci există un singur nod
    intermediar (2) între 1 și 3. dfs întoarce [1,2,4,3], deci sunt două noduri
    intermediare (2, 4) între 1 și 3.

    > countIntermediate 3 1 graph4
    Nothing

    Aici nu există cale între 3 și 1.
    
-}
countIntermediate :: Ord a
                  => a                 -- nodul sursă
                  -> a                 -- nodul destinație
                  -> StandardGraph a   -- graful
                  -> Maybe (Int, Int)  -- numărul de noduri expandate de BFS/DFS
countIntermediate from to graph = let span_bfs = (span (/= to) (bfs from graph))
                                      span_dfs = (span (/= to) (dfs from graph))
                                  in (if ((length (snd span_bfs)) == 0 && (length (snd span_dfs)) == 0)
                                        then Nothing -- didn't reach node "to"
                                        else
                                            (let length_bfs_result = ((length (fst span_bfs)) - 1)
                                                 length_dfs_result = ((length (fst span_dfs)) - 1)
                                             in (if (length_bfs_result == 0 && length_dfs_result == 0)
                                                    then Nothing -- same nr of intermediate nodes
                                                    else Just (length_bfs_result, length_dfs_result))))

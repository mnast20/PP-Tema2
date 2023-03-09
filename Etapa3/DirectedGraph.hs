module DirectedGraph where

import qualified Data.Set as S

class DirectedGraph a where
    nodes :: a -> S.Set b

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)

instance DirectedGraph (AlgebraicGraph a) where
    nodes (Empty) = S.empty
    nodes (Node node) = S.singleton node
    nodes (Overlay a b) = S.union (nodes a) (nodes b)
    nodes (Connect a b) = S.union (nodes a) (nodes b)
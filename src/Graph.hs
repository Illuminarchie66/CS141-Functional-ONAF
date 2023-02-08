module Graph where
import Data.Maybe

-- This data structure defines a graph with vertices and edges. This is used for the paths of the animatronics.
-- Made with a polymorphic type so that it feels more generic (if ever necesary).
-- Each edge consists of two vertices, or it doesn't exist. 
-- The graph is the list of edges.
data Graph a = Graph {vertices :: [a], edges :: [Edge a]}
data Edge a = Edge a a | Null
    deriving (Eq)

-- This takes an edge and a vertex, and returns the vertex on the other side of the edge. 
-- Returns it as a maybe, if it doesn't exist for either as it becomes a nothing type. 
-- If the edge is null, then it gives a Nothing. 
isolateVertex :: Eq a => a -> Edge a -> Maybe a
isolateVertex v (Edge x y)
    | v == x = Just y
    | v == y = Just x
    | otherwise = Nothing
isolateVertex v Null = Nothing

-- This gets a list of Maybe vertices, by mapping the previous isolateVertex function. 
getMaybeVertex :: Eq a => a -> Graph a -> [Maybe a]
getMaybeVertex v graph = map (isolateVertex v) (edges graph)

-- This gets a list of pure vertices, removing all Nothings, and the Maybe type with fromJust.
-- This gives all vertices attached to a given node within a graph. 
getVertex :: Eq a => a -> Graph a -> [a]
getVertex v graph = map fromJust (filter isJust (getMaybeVertex v graph))

-- This is similar to isolateVertex, but is strictly linear. There is no going backwards with this one.
isolateNext :: Eq a => a -> Edge a -> Maybe a
isolateNext v (Edge x y)
    | v == x = Just y
    | otherwise = Nothing
isolateNext v Null = Nothing

-- This is similar to the getVertex function, except it finds the node that follows the vertex given.
-- These last two functions are used for an almost queue like structure, designed for Freddy Fazbear.
getNextVertex :: Eq a => a -> Graph a -> a
getNextVertex v graph = fromJust $ head (map (isolateNext v) (edges graph))
module Tree exposing (..)


flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b


type Node a
  = Node a (List (Node a))


foldl : (a -> b -> b) -> b -> Node a -> b
foldl f acc node =
  case node of
    Node val [] ->
      f val acc

    Node val rest ->
      let
        restAcc = List.foldl (flip (foldl f)) acc rest
      in
      f val restAcc


map : (a -> b) -> Node a -> Node b
map f (Node item children) =
  Node (f item) (List.map (map f) children)


mapAccumulate : (s -> a -> (s, b)) -> s -> Node a -> (s, Node b)
mapAccumulate f acc0 (Node item children) =
  let
    ( acc1, res ) = f acc0 item
    childrenReducer : Node a -> (s, List (Node b)) -> (s, List (Node b))
    childrenReducer child (acc2, resNodes) =
      let
        ( acc3, newChild ) = mapAccumulate f acc2 child
      in
      ( acc3, resNodes ++ [newChild] )
    ( acc4, newChildren ) = List.foldl childrenReducer ( acc1, [] ) children
  in
  ( acc4, Node res newChildren )


indexedMapWithStartAndEnd : (Int -> a -> b) -> Int -> Node a -> (Int, Node b)
indexedMapWithStartAndEnd f =
  let
    wrappedF idx node = ( idx + 1, f idx node )
  in
  mapAccumulate wrappedF


indexedMap : (Int -> a -> b) -> Node a -> Node b
indexedMap f node = Tuple.second <| indexedMapWithStartAndEnd f 0 node


enumerate : Node a -> Node (Int, a)
enumerate = indexedMap Tuple.pair


unenumerate : Node (Int, a) -> Node a
unenumerate = map Tuple.second


modifyEnumeratedNodeWithIdx : Int -> (Node a -> Node a) -> Node (Int, a) -> Node a
modifyEnumeratedNodeWithIdx idx1 f (Node (idx2, item) children as node) = 
  if idx1 == idx2 then
    f (unenumerate node)
  else
    Node item (List.map (modifyEnumeratedNodeWithIdx idx1 f) children)


modifyNodeWithIdx : Int -> (Node a -> Node a) -> Node a -> Node a
modifyNodeWithIdx idx f node =
  modifyEnumeratedNodeWithIdx idx f (enumerate node)


addChildAtIdx : Int -> Node a -> Node a -> Node a
addChildAtIdx idx newNode =
  modifyNodeWithIdx idx
  <| \(Node item children) -> Node item (children ++ [newNode])


removeEnumeratedNodeAtIdx : Int -> Node (Int, a) -> Maybe (Node (Int, a))
removeEnumeratedNodeAtIdx idx1 (Node (idx2, item) children as node) =
  if idx1 == idx2 then
    Nothing
  else
    Just <| Node (idx2, item)
      (List.filterMap (removeEnumeratedNodeAtIdx idx1) children)


removeNodeAtIdx : Int -> Node a -> Maybe (Node a)
removeNodeAtIdx idx node =
  removeEnumeratedNodeAtIdx idx (enumerate node)
  |> Maybe.map unenumerate


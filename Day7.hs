data Node = Node { name :: [Char]
                 , weight :: Int
                 -- , parent :: Maybe Node
                 , children :: [ Node ] 
                 } deriving (Eq, Show)

bar = Node {name="bar", weight = 9, children = [foo]}
foo = Node {name="foo", weight = 10, children = []}
baz = Node {name="baz", weight = 12, children = [bar]}

getName :: Node -> [Char]
getName Node {name=n, weight = w, children = c} = n

getWeight :: Node -> Int
getWeight Node {name=n, weight = w, children = c} = w

getChildren :: Node -> [Node]
getChildren Node {name=n, weight = w, children = c} = c

addChild :: Node -> Node -> Node
addChild node child = Node name weight newChildren
    where 
        name = getName node
        weight = getWeight node
        children  = getChildren node
        newChildren = children ++ [child]


hasChildren :: Node -> Bool
hasChildren n
    | getChildren n == [] = False
    | otherwise  = True

--isChild :: Node -> [Node] -> Bool
--isChild n ns 
--    where
--        childNodes = map getChildren ns

-- getRootTree :: [Nodes] -> Node
getRootTree ns = root
    where
        branches = filter (hasChildren) ns
        childNodes = concatMap getChildren branches
        root = head $ filter (\n -> not $ elem n childNodes)  ns
        

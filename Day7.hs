data Node = Node { name :: [Char]
                 , weight :: Int
                 -- , parent :: Maybe Node
                 , children :: [ Node ] 
                 } deriving (Eq, Show)

bar = Node {name="bar", weight = 9, children = [foo]}
foo = Node {name="foo", weight = 10, children = []}
baz = Node {name="baz", weight = 19, children = []}

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



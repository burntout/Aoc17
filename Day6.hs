-- run one memory redistribution cycle
--
reDist :: [Int] -> [Int]
reDist memList = zipWith (+) memToAdd zeroMemListLocation
    where
        numMemBanks = length memList
        memToReDist = maximum memList
        -- find the index of the memory bank we are taking from
        reDistLocation = listIndex memToReDist memList
        -- zero that location
        zeroMemListLocation = zeroIndex (reDistLocation) memList
        -- start building a list of memory to add back in 
        allMemLocationsGet  = replicate numMemBanks $ div memToReDist numMemBanks
        remainingMem = rem memToReDist numMemBanks
        someMemLocationsGet = replicate remainingMem 1 
        padding = replicate (numMemBanks - remainingMem) 0
        -- rotate the list until it aligns with the memory banks
        memToAdd = rotate ((numMemBanks -1 ) - reDistLocation)$ zipWith (+) allMemLocationsGet (someMemLocationsGet ++ padding)

rotate :: Int -> [a] -> [a]
rotate n l  = take (length l) $ drop n $ cycle l

zeroIndex :: Int -> [Int] -> [Int]
zeroIndex 0 (x:xs) = (0:xs)
zeroIndex i (x:xs) = (x:zeroIndex (i-1) xs)

listIndex :: (Eq a) => a -> [a] -> Int
listIndex value list  = listIndex' value list 0
listIndex' val (l:ls) cnt
    | val == l  = cnt
    | otherwise = listIndex' val ls (cnt+1)
    
-- redistribute memory until a known state occurs
memDistCycles memList knownStates cnt
    | memList `elem` knownStates = (cnt, cnt - (listIndex memList knownStates))
    | otherwise                  = memDistCycles memList' knownStates' (cnt+1)
    where 
        memList' = reDist memList
        knownStates' = knownStates ++ [memList]

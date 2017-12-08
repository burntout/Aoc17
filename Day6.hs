reDist :: [Int] -> [Int]
reDist memList = zipWith (+) memToAdd zeroMemListLocation
    where
        numMemBanks = length memList
        memToReDist = maximum memList
        reDistLocation = listIndex memToReDist memList
        zeroMemListLocation = zeroIndex (reDistLocation) memList
        allMemLocationsGet  = replicate numMemBanks $ div memToReDist numMemBanks
        remainingMem = rem memToReDist numMemBanks
        someMemLocationsGet = replicate remainingMem 1 
        padding = replicate (numMemBanks - remainingMem) 0
        memToAdd = rotate ((numMemBanks -1 ) - reDistLocation)$ zipWith (+) allMemLocationsGet (someMemLocationsGet ++ padding)

rotate :: Int -> [a] -> [a]
rotate n l  = take (length l) $ drop n $ cycle l

zeroIndex 0 (x:xs) = (0:xs)
zeroIndex i (x:xs) = (x:zeroIndex (i-1) xs)

listIndex value list  = listIndex' value list 0
listIndex' val (l:ls) cnt
    | val == l  = cnt
    | otherwise = listIndex' val ls (cnt+1)

memDistCycles memList knownStates cnt
    | memList `elem` knownStates = (cnt, cnt - (listIndex memList knownStates))
    | otherwise                  = memDistCycles memList' knownStates' (cnt+1)
    where 
        memList' = reDist memList
        knownStates' = knownStates ++ [memList]

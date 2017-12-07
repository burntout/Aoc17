reDist :: [Int] -> [Int]
reDist memList = zipWith (+) toAdd zeroMemList
    where
        numMemBanks = length memList
        toReDist = maximum memList
        location = memIndex toReDist memList
        zeroMemList = zeroIndex (location ) memList
        allGet  = replicate numMemBanks $ div toReDist numMemBanks
        remain = rem toReDist numMemBanks
        someGet = replicate remain 1 
        remainder = replicate (numMemBanks - remain) 0
        toAdd = rotate ((numMemBanks -1 ) - location)$ zipWith (+) allGet (someGet ++ remainder)

rotate :: Int -> [a] -> [a]
rotate n l  = take (length l) $ drop n $ cycle l

zeroIndex 0 (x:xs) = (0:xs)
zeroIndex i (x:xs) = (x:zeroIndex (i-1) xs)

memIndex v l  = memIndex' v l 0
memIndex' v (l:ls) c
    | v == l  = c
    | otherwise = memIndex' v ls (c+1)

memCycles memList knownStates cnt
    | memList `elem` knownStates = (cnt, cnt - (memIndex memList knownStates))
    | otherwise                  = memCycles memList' knownStates' (cnt+1)
    where 
        memList' = reDist memList
        knownStates' = knownStates ++ [memList]

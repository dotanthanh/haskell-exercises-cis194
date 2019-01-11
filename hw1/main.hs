toDigits :: Integer -> [Integer]
toDigits n
    | n < 0 = []
    | n < 10 = [n]
    | otherwise = (toDigits (n `quot` 10)) ++ [n `mod` 10]

doubleEveryTwo :: [Integer] -> [Integer]
doubleEveryTwo [] = []
doubleEveryTwo [x] = [x] 
doubleEveryTwo (x:y:xs) = x : y * 2 : doubleEveryTwo xs

listToDigits :: [Integer] -> [Integer]
listToDigits [] = []
listToDigits (x:xs)
    | x < 10 = x : listToDigits xs
    | otherwise = toDigits x ++ listToDigits xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits lst =
    let converted_list = listToDigits lst
    in foldl1 (+) converted_list    

validate :: Integer -> Bool
validate n =
    let checksum = sumDigits $ doubleEveryTwo $ toDigits n
    in checksum `mod` 10 == 0
    
type Peg = String
type Move = (Peg, Peg)

-- move n disks from a to b using c as temporary storage
hanoiThreePegs :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoiThreePegs 0 _ _ _ = []
hanoiThreePegs n a b c = hanoiThreePegs (n - 1) a c b ++ [(a,b)] ++ hanoiThreePegs (n-1) c b a

hanoiFourPegs :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFourPegs 0 _ _ _ _ = []
hanoiFourPegs 1 a b _ _ = [(a,b)]
hanoiFourPegs n a b c d = hanoiFourPegs k a c b d ++ hanoiThreePegs (n-k) a b d ++ hanoiFourPegs k c b a d
    where k = n `quot` 2

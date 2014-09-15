import Test.QuickCheck
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []                     = True
isPermutation (a:as) b 
        | (length (a:as)) == length b   = isPermutation as (check a b)
        | otherwise                     = False
isPermutation _ _                       = False

check :: Eq a => a -> [a] -> [a]
check _ [] = []
check a (b:bs)
        | a == b                = bs
        | otherwise             = b : (check a bs)

isPermutation2 :: Ord a => [a] -> [a] -> Bool
isPermutation2 a b = (isort a) == (isort b)


sorted :: Ord a => [a] -> Bool
sorted []         = True
sorted [a]        = True
sorted (a:b:as)   = a <= b && (sorted (b:as))

insert' :: Ord a => a -> [a] -> [a]
insert' a []                = [a]
insert' a (b:bs)
        | a<=b              = a:b:bs
        | otherwise         = b:(insert' a bs)

prop_insert :: Integer -> [Integer] -> Property
prop_insert x xs = sorted xs ==> sorted (insert' x xs)

isort :: Ord a => [a] -> [a]
isort []         = []
isort (a:as)     = insert' a (isort as)

prop_isort :: [Integer] -> Bool
prop_isort xs = isPermutation xs s && sorted s
    where s = isort xs

duplicates :: Eq a => [a] -> Bool
duplicates []     = False
duplicates [a]    = False
duplicates (a:as) = (duplicates' a as) || (duplicates as)

duplicates' :: Eq a => a -> [a] -> Bool
duplicates' _ [] = False
duplicates' a (b:bs) = a == b || duplicates' a bs

--prop_duplicates :: [Integer] -> Bool
--prop_duplicates xs = duplicates xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (a:as) = a : (removeDuplicates (removeDuplicates' a as))

removeDuplicates' :: Eq a => a -> [a] -> [a]
removeDuplicates' _ []  = []
removeDuplicates' a (b:bs)
        | a == b                = removeDuplicates' a bs
        | otherwise             = b : (removeDuplicates' a bs)

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))

pascal :: Int -> [Integer]
pascal 1 = [1]
pascal 2 = [1,1]
pascal n = 1 : pascal' (pascal (n-1))  ++ [1]

pascal' :: [Integer] -> [Integer]
pascal' (_:[])          = []
pascal' (a:b:bs)        = (a + b) : (pascal' (b:bs))

crossOut :: Int -> [Int] -> [Int]
crossOut m ns = [x | x <- ns, ((x `mod` m) /= 0)]

sieve :: [Int] -> [Int]
sieve [a] = [a]
sieve (a:as) = a : sieve (crossOut a as)

isSum n = (isPrime,(length primeFactors) /= 0 , primeFactors )
        where 
                primeList       = sieve [2..n]
                primeFactors    = [(a,b) | a <- primeList, b <- primeList, a<=b, a+b==n]
                isPrime         = or [True| x <- primeList, x==n]


-- Spotify-time code
-- occursIn a xs, which returns True if x is an element of xs.
occursIn x xs = or [y==x | y <- xs]

-- allOccurIn xs ys, which returns True if all of the elements of xs are also elements of ys.
allOccursIn xs ys = and [occursIn x ys | x <- xs]

-- sameElements xs ys, which returns True if xs and ys have exactly the same elements
-- doesn't work just yet
--sameElements xs ys =  [ x | x <- xs, y <- ys, x == y]

-- numOccurrences x xs, which returns the number of times x occurs in xs
numOccurences x xs = length [ y | y <- xs, y==x] 

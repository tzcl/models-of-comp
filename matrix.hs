-- This is my first Haskell file!
-- Implementation of some basic matrix functions

mytranspose :: [[a]] -> [[a]]
mytranspose ([] : _) = []
mytranspose rows     = map head rows : mytranspose (map tail rows)

mmult :: [[Int]] -> [[Int]] -> [[Int]]
mmult []       _ = []
mmult (x : xs) y = map (dot x) (mytranspose y) : mmult xs y

dot :: [Int] -> [Int] -> Int
dot []       []       = 0
dot (x : xs) (y : ys) = x * y + dot xs ys
dot _        _        = error "Length mismatch"

-- Can use a lambda to avoid writing an extra function
-- (sum . zipWith (*) x) is the same as my dot function
mmult' :: Num a => [[a]] -> [[a]] -> [[a]]
mmult' mA mB = map (\row -> map (sum . zipWith (*) row) (mytranspose mB)) mA

-- Building a permutation function
ncombs :: Int -> [Int] -> [[Int]]
ncombs 0 []       = [[]]
ncombs n []       = []
ncombs n (x : xs) = [ [x] ++ cs | cs <- (ncombs (n - 1) xs) ] ++ (ncombs n xs)

combs :: [Int] -> [[Int]]
combs xs = concat (map ((flip ncombs) xs) [0 .. (length xs)])

combs' :: [Int] -> [[Int]]
combs' []       = [[]]
combs' (x : xs) = (combs' xs) ++ map ((++) [x]) (combs' xs)

insert_everywhere :: Int -> [Int] -> [[Int]]
insert_everywhere x [] = [[x]]
insert_everywhere x (y : ys) =
  [x : y : ys] ++ map ((++) [y]) (insert_everywhere x ys)

shuffle :: [Int] -> [[Int]]
shuffle []       = [[]]
shuffle (x : xs) = concat (map (insert_everywhere x) (shuffle xs))

perms :: [Int] -> [[Int]]
perms xs = concat (map shuffle (combs xs))

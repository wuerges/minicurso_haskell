
-- | Find the last element of a list:
question1 :: [a] -> a
question1 [x]    = x
question1 (x:xs) = question1 xs

-- | Find the last but one element of a list:
question2 :: [a] -> a
question2 [x,_] = x
question2 (x:xs) = question2 xs

-- | Find the K'th elemet of a list. The first element in the list is number 1.
question3 :: [a] -> Int -> a
question3 (x:xs) n | n == 1     = x
                   | otherwise = question3 xs (n-1)

-- | Find the number of elements of a list.
question4 :: [a] -> Int
question4 [] = 0
question4 (x:xs) = 1 + question4 xs

-- | Reverse a list.
question5 :: [a] -> [a]
question5 xs = r xs []
  where r []     l = l
        r (x:xs) l = r xs (x:l)

-- | Find out whether a list is a palindrome.
-- | A palindrome can be read forward or backward; e.g. (x a m a x).
question6 :: Eq a => [a] -> Bool
question6 p = p == question5 p

-- | Flatten a nested list structure.
question7 :: [[a]] -> [a]
question7 []     = []
question7 [x]    = x
question7 (x:xs) = x ++ question7 xs

-- | Eliminate consecutive duplicates of list elements
question8 :: Eq a => [a] -> [a]
question8 [] = []
question8 [x] = [x]
question8 (x1:x2:xs) | x1 == x2   = question8 (x2:xs)
                     | otherwise = x1:(question8 (x2:xs))

-- | Pack consecutive duplicates of list elements into sublists.
-- | If a list contains repeated elements they
-- | should be placed in separate sublists.
question9 :: Eq a => [a] -> [[a]]
question9 [] = []
question9 (x:xs) = pack x [x] xs
  where pack p ps [] = [ps]
        pack p ps (x:xs) | x == p     = pack p (x:ps) xs
                         | otherwise = ps:(question9 (x:xs))

-- | Run-length encoding of a list.
-- | Use the result of problem P09 to implement the
-- | so-called run-length encoding data compression method.
-- | Consecutive duplicates of elements are encoded as
-- | lists (N E) where N is the number of duplicates of
-- | the element E.

question10 :: Eq a => [a] -> [(Int, a)]
question10 l = map (\xs@(x:_) -> (length xs, x)) (question9 l)

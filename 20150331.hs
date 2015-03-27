-- Given a string, "say" what's in the string.
say :: [Char] -> [Char]
say [] = []
say str@(x:xs) = show (length (first)) ++ [x] ++ say second
 where
 	(first, second) = (span (== x) str)

-- Given a number (expressed as a string),
-- returns an infinite list in its look-and-say
-- sequence.
lookAndSayList :: [Char] -> [[Char]]
lookAndSayList n = [said] ++ lookAndSayList said
 where
 	said = say n

-- Retrieves the nth element in the look-and-say sequence
-- specified in the question.
lookAndSay :: Int -> [Char]
lookAndSay 1 = "1"
lookAndSay n = (lookAndSayList "1")!!(n - 2)
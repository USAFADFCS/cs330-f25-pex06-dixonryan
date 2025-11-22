-- pex6main.hs 
-- unKnot Haskell

-- name: Dixon Ryan

{- DOCUMENTATION:
   - Had to use google to figure out how to get the second guard of the unknot function to compare to another 
      bool in order to satisfy the left side of the equals sign
-}

{- TODOS:
+Create unknot logic to check for missing tuple in the typeII knots
+Make code recurse/loop until the entire knot it solved
-}

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | getLength tripCode <= 1 = "unknot"
   | typeIknot tripCode /= tripCode = unKnot (typeIknot tripCode)
   -- typeIIfindPair
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)


getLength [] = 0
getLength (x:xs) = 1 + getLength xs

typeIknot :: [(Char, Char)] -> [(Char, Char)]
typeIknot [] = []
typeIknot [x] = [x]
typeIknot (x:y:xs)
   --remove type 1 knot
   | fst(x) == fst(y) = typeIknot xs
   --head doesn't match second; bypass it and check rest of trip code
   | otherwise = x: typeIknot (y:xs)


--no point in looking if a typeII not possible
typeIIfindPair [] = []
typeIIfindPair [x] = [x]
typeIIfindPair [x,y] = [x,y]
typeIIfindPair [x,y,z] = [x,y,z]
--returns the adjacent pair that has to exist for there to be a typeII knot
typeIIfindPair (x:y:xs)
--if the types are equal and letters unequal -> type II start possible
   | fst(x) /= fst(y) && snd(x) == snd(y) = [(x),(y)] ++ typeIIfindSecond xs [x,y]
--else look in the rest of the list
   | otherwise = typeIIfindPair(y:xs)

typeIIfindThird [] _ = []
--finds the 4th tuple (third thing to find)
typeIIfindThird (x:xs) [p3]
   --returns the 4th thing if found
   | fst(x) == fst(p3) && snd(x) /= snd(x) = [x]
   --otherwise keeps looking
   | otherwise = typeIIfindThird (xs) [p3]



typeIIfindSecond [] _ = []
--finds first occurence of either of the two found in the pair
typeIIfindSecond (x:xs) [p1, p2]
   --if it finds the first pair with different ending it will look for the 4th tuple & append it
   | fst(x) == fst(p1) && snd(x) /= snd(p1) = p1: typeIIfindThird xs [p2]
   --otherwise it keeps looking
   | otherwise = typeIIfindSecond (xs) [p1, p2]



main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   let t02 = [('a','o'), ('a','u'), ('b','u'), ('b','o')]
   let t03 = [('a','o'),('b','o'),('c','o'),('c','u'),('b','u'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   --print("   result:" ++ unKnot t01)

   --print(typeIknot(t01))
   --print(typeIknot(t03))
   print(unKnot t03)
   --print(typeIIfindPair(t03))

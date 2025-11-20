-- pex6.hs 
-- unKnot Haskell

-- name: Dixon Ryan

{- DOCUMENTATION:
   - Had to use google to figure out how to get the second guard of the unknot function to compare to another 
      bool in order to satisfy the left side of the equals sign
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | getLength tripCode == 1 = "unknot"
   | typeIknot tripCode /= tripCode = unKnot tripCode
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)


getLength [] = 0
getLength (x:xs) = 1 + getLength xs

typeIknot :: [(Char, Char)] -> [(Char, Char)]
typeIknot tripCode
   -- if 1 or 0 elements then no knot to be found
   | null tripCode = tripCode
   | getLength(tripCode) == 1 = tripCode
   --remove type 1 knot
   | fst(head(tripCode)) == fst(head(tail(tripCode))) = typeIknot (tail(tail(tripCode)))
   --head doesn't match second; bypass it and check rest of trip code
   | otherwise = head tripCode: typeIknot (tail tripCode)

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

   print(unKnot(t01))

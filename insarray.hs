
import Debug.Trace

data LvValue = LvINT Int
             | LvArr [LvValue]
   deriving Show

dims (LvArr (v:vs)) = 1 + dims v
dims _ = 0

zero (LvINT _) = (LvINT 0)
zero (LvArr l@(x : xs)) = LvArr (replicate (length l) (zero x))

pad s l@(x:xs) = take s (l ++ repeat (zero x))

--matchDim (LvArr a@(LvINT x:xs)) (LvArr b@(LvINT y:ys)) =
--   let
--      s = max (length a) (length b)
--   in
--      (LvArr (pad s a), LvArr (pad s b))
--
--matchDim a@(LvArr (LvArr x:xs)) b@(LvArr (LvArr y:ys)) =
--   let
--      dims (LvArr a@(LvArr x:xs)) = length a : dims (LvArr x)
--      dims (LvArr a@(LvINT x:xs)) = [length a]
--      
--      zipMax (x:xs) (y:ys) = max x y : zipMax xs ys
--      zipMax [] y = y
--      zipMax x [] = x
--      
--      maxDims = zipMax (dims a) (dims b)
--      
--      zeroArray (d:[]) = LvArr (replicate d (LvINT 0))
--      zeroArray (d:ds) = LvArr (replicate d (zeroArray ds))
--      
--      padTo (d:ds) (LvArr vs) = LvArr (take d $ map (padTo ds) vs ++ repeat (zeroArray ds))
--   in
--      (padTo maxDims a, padTo maxDims b)

insertIntoArray a@(LvArr aa) v@(LvINT vv) [Just i] | dims a == 1 =
   LvArr ((take i aa) ++ [v] ++ (drop i aa))

insertIntoArray a@(LvArr aa@(LvArr aa1:aas)) v@(LvArr vv) [Just i] | dims a == dims v + 1 =
   let
      len_v = length vv
      len_aa' = length aa1
      aa2 = map (\(LvArr x) -> LvArr $ pad (max len_v len_aa') x) aa
      vv2 = pad (max len_v len_aa') vv
   in
      LvArr ((take i aa2) ++ vv2 ++ (drop i aa2))

insertIntoArray a@(LvArr aa) v@(LvArr vv) (Nothing : idxs) | dims a == dims v + 1 =
   let
      len_v = length vv
      len_a = length aa
      aa2 = pad (max len_v len_a) aa
      vv2 = pad (max len_v len_a) vv
   in
      LvArr (map insertEach (zip aa2 vv2))
      where
         insertEach (a, v) = insertIntoArray a v idxs

insertIntoArray a@(LvArr aa@(LvINT aa1:aas)) v@(LvArr vv@(LvINT vv1:vvs)) [Just i] =
   LvArr ((take i aa) ++ vv ++ (drop i aa))

insertIntoArray a@(LvArr aa@(LvArr aa1:aas)) v@(LvArr vv@(LvArr vv1:vvs)) [Just i] | dims a == dims v =
   let
      len_v = length vv
      len_a = length aa
      aa2 = pad (max len_v len_a) aa
      vv2 = pad (max len_v len_a) vv
   in
      LvArr ((take i aa2) ++ vv2 ++ (drop i aa2))

arr1 a = LvArr (map LvINT a)
arr2 a = LvArr a

main =
   do
      print $ insertIntoArray (arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]]) (arr1 [10, 20, 30, 40, 50]) [Just 0]
      print $ insertIntoArray (arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]]) (arr1 [10, 20, 30, 40, 50]) [Nothing, Just 0]
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]]) (arr2 [arr1 [10, 20, 30], arr1 [40, 50, 60]]) [Just 0]
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]]) (arr2 [arr1 [10, 20, 30], arr1 [40, 50, 60]]) [Nothing, Just 0]
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]]) (arr2 [arr1 [10, 20, 30], arr1 [40, 50, 60]]) [Nothing, Nothing, Just 3]
      print $ insertIntoArray (arr1 [1, 2, 3]) (arr1 [10, 20, 30]) [Just 2]
--      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
--                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]])
--                              (arr2 [arr2 [arr1 [101, 102, 103, 200], arr1 [104, 105, 106, 202]],
--                                     arr2 [arr1 [107, 108, 109, 201], arr1 [110, 111, 112, 203]],
--                                     arr2 [arr1 [204, 205, 206, 207], arr1 [208, 209, 210, 211]]])
--                              [Just 0]

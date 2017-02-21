
import Debug.Trace

data LvValue = LvINT Int
             | LvArr [LvValue]
   deriving Show

ndims (LvArr (v:vs)) = 1 + ndims v
ndims _ = 0

zero (LvArr l@(x : xs)) = LvArr (replicate (length l) (zero x))
zero (LvINT _)          = (LvINT 0)

resizeDim :: (LvValue -> LvValue) -> [LvValue] -> [LvValue] -> [LvValue]
resizeDim conv xs@(x:_) ys = map conv $ take (length xs) $ ys ++ (repeat . zero) x

resize :: [LvValue] -> [LvValue] -> [LvValue]
resize xs@(LvArr x:_) ys = resizeDim (\(LvArr a) -> LvArr (resize x a)) xs ys
resize xs@(LvINT x:_) ys = take (length xs) $ ys ++ repeat (LvINT 0)

resizeLower xs@(LvArr x:_) ys = map (\(LvArr a) -> LvArr (resize x a)) ys
resizeLower xs@(LvINT x:_) ys = ys

insertIntoArray vx@(LvArr lx@(LvINT x:_)) vy@(LvINT ly) [Just i] | ndims vx == ndims vy + 1 =  LvArr ((take i lx) ++ [vy] ++ (drop i lx))
insertIntoArray vx@(LvArr lx@(LvArr x:_)) vy@(LvArr ly) [Just i] | ndims vx == ndims vy + 1 =  LvArr ((take i lx) ++ [LvArr (resize x ly)] ++ (drop i lx))
insertIntoArray vx@(LvArr lx@(LvArr x:_)) vy@(LvArr ly) (Nothing: idxs) | ndims vx == ndims vy + 1 = LvArr (zipWith (\a b -> insertIntoArray a b idxs) lx (resize x ly))
insertIntoArray vx@(LvArr lx@(LvINT x:_)) vy@(LvArr ly) [Just i] | ndims vx == ndims vy = LvArr ((take i lx) ++ ly ++ (drop i lx))
insertIntoArray vx@(LvArr lx@(LvArr x:_)) vy@(LvArr ly) [Just i] | ndims vx == ndims vy = LvArr ((take i lx) ++ (resizeLower lx ly) ++ (drop i lx))
insertIntoArray vx@(LvArr lx@(LvArr x:_)) vy@(LvArr ly) (Nothing: idxs) | ndims vx == ndims vy = LvArr (zipWith (\a b -> insertIntoArray a b idxs) lx (resizeDim id lx ly))

arr1 a = LvArr (map LvINT a)
arr2 a = LvArr a

main =
   do
      print $ insertIntoArray (arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]]) (arr1 [10, 20, 30, 40, 50]) [Just 0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]]) (arr1 [10, 20, 30, 40, 50]) [Nothing, Just 0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [Just 0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [Nothing, Just 0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [Nothing, Nothing, Just 3]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3, 1, 1, 1, 1], arr1 [1, 1, 1, 1, 4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9, 1, 1, 1, 1], arr1 [1, 1, 1, 1, 10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [Just 0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 1, 1, 1, 1, 2, 3], arr1 [1, 1, 1, 1, 4, 5, 6]],
                                     arr2 [arr1 [1, 1, 1, 1, 7, 8, 9], arr1 [1, 1, 1, 1, 10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [Nothing, Just 0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 1, 1, 1, 1, 2, 3], arr1 [1, 1, 1, 1, 4, 5, 6]],
                                     arr2 [arr1 [1, 1, 1, 1, 7, 8, 9], arr1 [1, 1, 1, 1, 10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [Nothing, Nothing, Just 3]
      putStr "\n"
      print $ insertIntoArray (arr1 [1, 2, 3]) (arr1 [10, 20, 30, 40, 50]) [Just 2]
      putStr "THIRD DIMENSION\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]])
                              (arr2 [arr2 [arr1 [111, 112, 113, 114], arr1 [121, 122, 123, 124], arr1 [131, 132, 133, 134]],
                                     arr2 [arr1 [211, 212, 213, 214], arr1 [221, 222, 223, 224], arr1 [231, 232, 233, 234]],
                                     arr2 [arr1 [311, 312, 313, 314], arr1 [321, 322, 323, 324], arr1 [331, 332, 333, 334]]])
                              [Nothing, Nothing, Just 1]
      putStr "SECOND DIMENSION\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]])
                              (arr2 [arr2 [arr1 [111, 112, 113, 114], arr1 [121, 122, 123, 124], arr1 [131, 132, 133, 134]],
                                     arr2 [arr1 [211, 212, 213, 214], arr1 [221, 222, 223, 224], arr1 [231, 232, 233, 234]],
                                     arr2 [arr1 [311, 312, 313, 314], arr1 [321, 322, 323, 324], arr1 [331, 332, 333, 334]]])
                              [Nothing, Just 1]
      putStr "FIRST DIMENSION\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]])
                              (arr2 [arr2 [arr1 [111, 112, 113, 114], arr1 [121, 122, 123, 124], arr1 [131, 132, 133, 134]],
                                     arr2 [arr1 [211, 212, 213, 214], arr1 [221, 222, 223, 224], arr1 [231, 232, 233, 234]],
                                     arr2 [arr1 [311, 312, 313, 314], arr1 [321, 322, 323, 324], arr1 [331, 332, 333, 334]]])
                              [Just 1]

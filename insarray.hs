
import Debug.Trace

data LvValue = LvINT Int
             | LvArr [LvValue]
   deriving Show

ndims :: LvValue -> Int
ndims (LvArr (v:vs)) = 1 + ndims v
ndims _              = 0

zero :: LvValue -> LvValue
zero (LvArr l@(x : xs)) = LvArr (replicate (length l) (zero x))
zero (LvINT _)          = (LvINT 0)

resizeCurr :: (LvValue -> LvValue) -> [LvValue] -> [LvValue] -> [LvValue]
resizeCurr childOp xs@(x:_) ys = map childOp $ take (length xs) $ ys ++ (repeat (zero x))

childResizer :: LvValue -> (LvValue -> LvValue)
childResizer (LvArr x) = \(LvArr a) -> LvArr (resizeAll x a)
childResizer (LvINT _) = id

resizeAll :: [LvValue] -> [LvValue] -> [LvValue]
resizeAll xs@(x:_) ys = resizeCurr (childResizer x) xs ys

resizeLower :: [LvValue] -> [LvValue] -> [LvValue]
resizeLower (x:_) ys = map (childResizer x) ys

insertAt  i  lx ly = LvArr $ (take i lx) ++ ly ++ (drop i lx)

recurseTo is lx ly = LvArr $ zipWith (\a b -> insertIntoArray a b is) lx ly

eqDim a b = ndims a == ndims b
neDim a b = ndims a == ndims b + 1

---- vx and vy have equal dimensions
--insertIntoArray vx@(LvArr lx@(LvINT x:_)) vy@(LvArr ly) (i  : _)  | vx `eqDim` vy = insertAt  i  lx ly                         -- base dimension: insert an array of integers as-is
--insertIntoArray vx@(LvArr lx@(LvArr x:_)) vy@(LvArr ly) (i  : _)  | vx `eqDim` vy = insertAt  i  lx (resizeLower lx ly)        -- inserting at the current dimension, resize lower dimensions of input
--insertIntoArray vx@(LvArr lx@(LvArr x:_)) vy@(LvArr ly) (-1 : is) | vx `eqDim` vy = recurseTo is lx (resizeCurr id lx ly)      -- inserting at a lower dimension, adjust current dimension of input
--
---- vx is one dimension bigger than vy
--insertIntoArray vx@(LvArr lx@(LvINT x:_)) vy@(LvINT ly) (i  : _)  | vx `neDim` vy = insertAt  i  lx [vy]                       -- base dimension: inserting an integer
--insertIntoArray vx@(LvArr lx@(LvArr x:_)) vy@(LvArr ly) (i  : _)  | vx `neDim` vy = insertAt  i  lx [LvArr (resizeAll x ly)]   -- inserting at the current dimension, adjust size and insert
--insertIntoArray vx@(LvArr lx@(LvArr x:_)) vy@(LvArr ly) (-1 : is) | vx `neDim` vy = recurseTo is lx (resizeCurr id x ly)       -- inserting at a lower dimension, adjust size and insert

--insertIntoArray vx vy idxs
-- | ndims vx == ndims vy =
--      case (vx, vy, idxs) of
--      (LvArr lx@(LvArr x:_), LvArr ly, (-1 : is)) -> recurseTo  is  lx (resizeCurr id lx ly)      -- inserting at a lower dimension, adjust current dimension of input
--      (LvArr lx@(LvArr x:_), LvArr ly, (i  : _ )) -> insertAt   i   lx (resizeLower lx ly)        -- inserting at the current dimension, resize lower dimensions of input
--      (LvArr lx            , LvArr ly, (i  : _ )) -> insertAt   i   lx ly                         -- base dimension: insert an array of integers as-is
-- | ndims vx == ndims vy + 1 =
--      case (vx, vy, idxs) of
--      (LvArr lx@(LvArr x:_), LvArr ly, (-1 : is)) -> recurseTo  is  lx (resizeCurr id x ly)       -- inserting at a lower dimension, adjust current dimension of input
--      (LvArr lx@(LvArr x:_), LvArr ly, (i  : _ )) -> insertAt   i   lx [LvArr (resizeAll x ly)]   -- inserting at the current dimension, resize lower dimensions of input
--      (LvArr lx            , _       , (i  : _ )) -> insertAt   i   lx [vy]                       -- base dimension: insert an array of integers as-is

insertIntoArray vx vy idxs =
   case (vx, vy, idxs) of
   (LvArr lx@(LvArr x:_),  LvArr ly,  (-1 : is))  -> recurseTo  is  lx (next x lx ly)
   (LvArr lx@(LvArr x:_),  LvArr ly,  (i  : _ ))  -> insertAt   i   lx (curr x lx ly)
   (LvArr lx            ,  _       ,  (i  : _ ))  -> insertAt   i   lx (base vy)
   where
      (next, curr, base) =
         if ndims vx == ndims vy
         then (\x lx ly     -> resizeCurr id lx ly,
               \x lx ly     -> resizeLower lx ly,
               \(LvArr ly)  -> ly)
         else (\x lx ly     -> resizeCurr id x ly,
               \x lx ly     -> [LvArr (resizeAll x ly)],
               \vy          -> [vy])




arr1 a = LvArr (map LvINT a)
arr2 a = LvArr a

main =
   do
      print $ insertIntoArray (arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]]) (arr1 [10, 20, 30, 40, 50]) [0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]]) (arr1 [10, 20, 30, 40, 50]) [-1, 0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [-1, 0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [-1, -1, 3]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3, 1, 1, 1, 1], arr1 [1, 1, 1, 1, 4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9, 1, 1, 1, 1], arr1 [1, 1, 1, 1, 10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 1, 1, 1, 1, 2, 3], arr1 [1, 1, 1, 1, 4, 5, 6]],
                                     arr2 [arr1 [1, 1, 1, 1, 7, 8, 9], arr1 [1, 1, 1, 1, 10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [-1, 0]
      putStr "\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 1, 1, 1, 1, 2, 3], arr1 [1, 1, 1, 1, 4, 5, 6]],
                                     arr2 [arr1 [1, 1, 1, 1, 7, 8, 9], arr1 [1, 1, 1, 1, 10,11,12]]]) (arr2 [arr1 [100, 200, 300], arr1 [400, 500, 600]]) [-1, -1, 3]
      putStr "\n"
      print $ insertIntoArray (arr1 [1, 2, 3]) (arr1 [10, 20, 30, 40, 50]) [2]
      putStr "THIRD DIMENSION\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]])
                              (arr2 [arr2 [arr1 [111, 112, 113, 114], arr1 [121, 122, 123, 124], arr1 [131, 132, 133, 134]],
                                     arr2 [arr1 [211, 212, 213, 214], arr1 [221, 222, 223, 224], arr1 [231, 232, 233, 234]],
                                     arr2 [arr1 [311, 312, 313, 314], arr1 [321, 322, 323, 324], arr1 [331, 332, 333, 334]]])
                              [-1, -1, 1]
      putStr "SECOND DIMENSION\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]])
                              (arr2 [arr2 [arr1 [111, 112, 113, 114], arr1 [121, 122, 123, 124], arr1 [131, 132, 133, 134]],
                                     arr2 [arr1 [211, 212, 213, 214], arr1 [221, 222, 223, 224], arr1 [231, 232, 233, 234]],
                                     arr2 [arr1 [311, 312, 313, 314], arr1 [321, 322, 323, 324], arr1 [331, 332, 333, 334]]])
                              [-1, 1]
      putStr "FIRST DIMENSION\n"
      print $ insertIntoArray (arr2 [arr2 [arr1 [1, 2, 3], arr1 [4, 5, 6]],
                                     arr2 [arr1 [7, 8, 9], arr1 [10,11,12]]])
                              (arr2 [arr2 [arr1 [111, 112, 113, 114], arr1 [121, 122, 123, 124], arr1 [131, 132, 133, 134]],
                                     arr2 [arr1 [211, 212, 213, 214], arr1 [221, 222, 223, 224], arr1 [231, 232, 233, 234]],
                                     arr2 [arr1 [311, 312, 313, 314], arr1 [321, 322, 323, 324], arr1 [331, 332, 333, 334]]])
                              [1]

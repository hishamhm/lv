
data LvStringWire = LvStringWire String String
   deriving Show

wire :: String -> String -> LvStringWire
wire a b = LvStringWire a b

makeVI ::  [(String, LvControl)] -> [(String, LvIndicator)]
           -> [(String, LvNode)] -> [LvStringWire] -> LvVI
makeVI ctrls indics nodes stringWires =
   LvVI {
      vCtrls = ctrls,
      vIndics = indics,
      vNodes = nodes,
      vWires = map convert stringWires
   }
   where
      convert :: LvStringWire -> LvWire      
      convert (LvStringWire src dst) =
         let
            (srcType,  srcElem,  srcPort')  = findElem ctrls   LvC  vIndics  src
            (dstType,  dstElem,  dstPort')  = findElem indics  LvI  vCtrls   dst
         in
            LvWire (LvPortAddr srcType srcElem srcPort') (LvPortAddr dstType dstElem dstPort')

      findIndex :: [(String, a)] -> String -> Maybe Int
      findIndex es name = elemIndex name $ map fst es
      
      must :: (String -> Maybe a) -> String -> a
      must fn name = fromMaybe (error ("No such entry " ++ name)) (fn name)

      findElem ::  [(String, a)] -> LvElemType -> (LvVI -> [(String, b)])
                   -> String -> (LvElemType, Int, Int)
      findElem entries etype elemEntries name
       | isJust $ find (== ':') name =
            let 
               [elemName, portName] = splitOn ":" name
               elem = (must . flip lookup) nodes elemName
               findPort (LvStructure _ subVi)  = must $ findIndex (elemEntries subVi)
               findPort (LvCase subVis)        = must $ findIndex (elemEntries (head subVis))
               findPort (LvFunction _)         = \s -> if null s then 0 else read s
               findPort _                      = \s -> 0
            in
               (LvN, (must . findIndex) nodes elemName, findPort elem portName)
       | otherwise =
          case findIndex entries name of
          Just i -> (etype, i, 0)
          Nothing -> findElem entries etype elemEntries (name ++ ":0")

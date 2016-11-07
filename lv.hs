
import Data.List
import Data.Maybe
import Data.Sequence (mapWithIndex, fromList)
import Data.Foldable (toList)

-- ========================================
-- Debugging
-- ========================================
import Debug.Trace
import ShowConcat ((@@), (@@@))

-- Debugging on:
tsi :: Show a => a -> a
tsi = traceShowId
trc = trace

-- Debugging off:
--tsi = id
--trc m = id

-- ========================================
-- Data types
-- ========================================

data LvValue = LvEXT Double
             | LvDBL Double
             | LvSGL Float
             | LvFXP Double
             | LvI64 Int
             | LvI32 Int
             | LvI16 Int
             | LvI8  Int
             | LvU64 Int
             | LvU32 Int
             | LvU16 Int
             | LvU8  Int
             | LvBoolean Bool
             | LvString String
             | LvCluster [LvValue]
             | LvArray Int LvValue
             -- | LvCXT Complex
             -- | LvCDB Complex
             -- | LvCSG Complex
   deriving (Show, Eq)

data LvControlType = LvCtrl
                   | LvTunCtrl -- tunnel, used in loops
                   | LvSRCtrl -- shift register, used in loops
                   -- TODO: tables?
   deriving Show

data LvControl = LvControl LvControlType LvValue
   deriving Show

data LvTunnelMode = LvAutoIndexing
                  | LvLastValue
                  -- TODO other kinds
   deriving Show

data LvIndicatorType = LvIndic
                     | LvTunIndic LvTunnelMode -- tunnel, used in loops
                     | LvSRIndic -- shift register, used in loops
                     -- TODO: tables?
   deriving Show

data LvIndicator = LvIndicator LvIndicatorType LvValue
   deriving Show

data LvPanel = LvPanel
                  [(String, LvControl)]
                  [(String, LvIndicator)]
   deriving Show

data LvType = LvN
            | LvC
            | LvI
            | LvERR String
   deriving (Show, Eq)

data LvWire = LvWire
                LvType -- source node type
                Int -- node index
                Int -- port index
                LvType -- destination node type
                Int -- node index
                Int -- port index
   deriving Show

data LvStringWire = LvStringWire (String, Int) (String, Int)
   deriving Show

data LvStrucType = LvWhile
                 | LvFor
   deriving Show

data LvNode = LvSubVI LvVI
            | LvFunction String
            | LvConstant LvValue
            | LvStructure LvStrucType LvVI
            | LvFeedbackNode LvValue
   deriving Show

data LvDiagram = LvDiagram 
                    [(String, LvNode)] -- nodes
                    [LvWire] -- wires
   deriving Show

data LvVI = LvVI LvPanel LvDiagram
   deriving Show

data LvState = LvState
                  [LvValue] -- control values
                  [LvValue] -- indicator values
                  [Maybe LvState] -- structures and subVIs
                  [Maybe LvValue] -- wire values
   deriving Show

data FromTo = From | To
   deriving Eq

-- ========================================
-- Program construction
-- ========================================

zwire a b = LvStringWire (a, 0) (b, 0)

nwire a i b j = LvStringWire (a, i) (b, j)

makeVI :: [(String, LvControl)] -> [(String, LvIndicator)] -> [(String, LvNode)] -> [LvStringWire] -> LvVI
makeVI controls indicators nodes stringWires =
   LvVI (LvPanel controls indicators) (LvDiagram nodes wires)
   where
      findNode :: FromTo -> String -> Int -> (LvType, Int, Int)
      findNode side name port =
         let
            pickSide :: a -> a -> a
            pickSide a b = if side == From then a else b

            findPort ctrls indics = elemIndex name $ pickSide (map fst ctrls) (map fst indics)
            
            checkNode :: Int -> (String, LvNode) -> Maybe (Int, Int)
            checkNode nodeIdx (_, LvStructure _ (LvVI (LvPanel nodeCtrls nodeIndics) _)) =
               case findPort nodeIndics nodeCtrls of
                  Just i -> Just (nodeIdx, i)
                  Nothing -> Nothing
            checkNode nodeIdx (nodeName, node) =
               if name == nodeName
               then Just (nodeIdx, port)
               else Nothing
         in
            case findPort controls indicators of
               Just i -> (pickSide LvC LvI, i, port)
               Nothing -> case find isJust $ toList $ mapWithIndex checkNode (fromList nodes) of
                  Just (Just (node, nodePort)) -> (LvN, node, nodePort)
                  Nothing -> (LvERR name, 0, port)

      convert :: LvStringWire -> LvWire      
      convert (LvStringWire (from, fromPort) (to, toPort)) =
         let
            (fromType, fromNode, fromPort') = findNode From from fromPort
            (toType,   toNode,   toPort')   = findNode To   to   toPort
         in
            LvWire fromType fromNode fromPort' toType toNode toPort'
      
      wires :: [LvWire]
      wires = map convert stringWires

-- ========================================
-- Functions
-- ========================================

numberOfInputs :: String -> Int
numberOfInputs "*" = 2
numberOfInputs "WaitUntilNextMs" = 1
numberOfInputs "RandomNumber" = 0
numberOfInputs "Bundle" = 2
numberOfInputs "+" = 2
numberOfInputs "InsertIntoArray" = 2
numberOfInputs "ArrayMax&Min" = 1

applyFunction :: String -> [LvValue] -> [LvValue]

applyFunction "*" [LvDBL a, LvDBL b] = [LvDBL (a * b)]

-- ========================================
-- Dataflow
-- ========================================

initialState :: LvVI -> LvState
initialState (LvVI (LvPanel controls indicators) (LvDiagram nodes wires)) =
   LvState controlValues indicatorValues subStates wireValues
      where
         controlValues = map (\(_, LvControl _ v) -> v) controls
         indicatorValues = map (\(_, LvIndicator _ v) -> v) indicators
         subStates = map expandNode nodes
            where
               expandNode :: (String, LvNode) -> Maybe LvState
               expandNode node = 
                  case node of
                     (_, LvSubVI vi) -> Just $ initialState vi
                     (_, LvStructure _ vi) -> Just $ initialState vi
                     otherwise -> Nothing
         wireValues = map (\_ -> Nothing) wires

-- TODO this is implying the ordering given in the description of LvVI,
-- but LabVIEW does not specify it
run :: LvState -> LvVI -> LvState
run state (LvVI (LvPanel controls indicators) (LvDiagram nodes wires)) =
   let
      (_, state')   = foldl' fireControl   (0, state)   controls
      (_, state'')  = foldl' fireNode      (0, state')  nodes
      (_, state''') = foldl' fireIndicator (0, state'') indicators
   in
      state'''
   where

      propagate :: LvValue -> LvType -> Int -> Int -> [Maybe LvValue] -> [Maybe LvValue]
      propagate val typ nidx pidx wvalues =
         (toList $ mapWithIndex setValue $ (fromList wvalues))
            where
               setValue widx wvalue =
                  case wires !! widx of
                  LvWire t n p _ _ _ | t == typ && n == nidx && p == pidx -> Just val
                  otherwise -> wvalue

      replace :: Int -> a -> [a] -> [a]
      replace idx val list =
         let (a, b) = splitAt idx list
         in a ++ [val] ++ (if null b then [] else tail b)

      incomingValue :: LvType -> Int -> Int -> [Maybe LvValue] -> Maybe LvValue
      incomingValue typ nidx pidx ws =
         -- look for dst = (typ, nidx, pidx) in wires, get its widx, then return ws!!widx
         -- FIXME what if more than one incoming wires have values; we are discarding. CHECK: should trigger 2x?
         fromMaybe Nothing $ find isJust $ map matchDst $ zip wires ws
         where
            matchDst (LvWire _ _ _ t n p, val) = if typ == t && nidx == n && pidx == p then val else Nothing

      fireControl (cidx, (LvState cs is ss ws)) (cname, control) =
         trc ("Firing control " ++ cname) $ tsi $
         (cidx + 1, state')
         where
         state' = LvState cs is ss (propagate (cs !! cidx) LvC cidx 0 ws)

      fireNode (nidx, state@(LvState cs is ss ws)) (nname, node) =
         trc ("Firing node " ++ nname) $ tsi $
         (nidx + 1, state')
         where
         state' =
            case node of
            LvSubVI vi -> state -- TODO
            LvFunction name -> 
               let
                  ninputs = numberOfInputs name
                  inVals = map (\n -> incomingValue LvN nidx n ws) [0..(ninputs - 1)]
               in
                  -- if not all inVals have values, don't fire: return unaltered state
                  if elem Nothing inVals 
                  then state
                  else 
                     let
                        outVals = applyFunction name (catMaybes inVals)
                        propagateOutVal curWs pidx = propagate (outVals !! pidx) LvN nidx pidx curWs
                        newWs = foldl' propagateOutVal ws [0..((length outVals) - 1)]
                     in
                        LvState cs is ss newWs
            LvConstant val -> LvState cs is ss (propagate val LvN nidx 0 ws)
            LvStructure typ vi -> state -- TODO
            LvFeedbackNode initVal ->
               let
                  inVal = incomingValue LvN nidx 0 ws
                  newVal =
                     case inVal of
                     Nothing -> initVal
                     Just v -> v 
               in
                  LvState cs is ss (propagate newVal LvN nidx 0 ws)
      
      fireIndicator (iidx, state@(LvState cs is ss ws)) (iname, indicator) =
         trc ("Firing indicator " ++ iname) $ tsi $
         (iidx + 1, state')
         where
         inVal = incomingValue LvI iidx 0 ws
         state' =
            case inVal of
            Nothing -> state
            Just val -> LvState cs (replace iidx val is) ss ws

-- ========================================
-- Example programs
-- ========================================

-- TestingFor.vi
-- On continuous run, running this with 0 in the input produces 0,
-- incrementing to 1 it remains at 0, and
-- incrementing to 2 it increments the output forever.
-- This is due to the shift register.
-- Uncommenting the node and the wire that are marked ***
-- stops the endless incrementation. FIXME but does it busy-wait?
testingFor =
   makeVI
         [ -- controls
            ("control", LvControl LvCtrl (LvDBL 0.0))
         ]
         [ -- indicators
            ("indicator", LvIndicator LvIndic (LvDBL 0.0))
         ]
         [ -- nodes
            --("0", LvConstant (LvDBL 0.00)), -- ***
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("N", LvControl LvTunCtrl (LvI32 0)),
                  ("i", LvControl LvCtrl (LvI32 0)),
                  ("shift reg out", LvControl LvSRCtrl (LvI32 0))
               ]
               [ -- indicators
                  ("shift reg in", LvIndicator LvSRIndic (LvI32 0)),
                  ("out", LvIndicator (LvTunIndic LvLastValue) (LvI32 0))
               ]
               [ -- nodes
                  ("+", LvFunction "+")
               ]
               [ -- wires
                  nwire "shift reg out" 0 "+" 0,
                  nwire "i" 0             "+" 1,
                  zwire "+" "shift reg in",
                  zwire "+" "out",
                  zwire "shift reg in" "shift reg out" -- this is not a wire!
               ]
            ))
         ]
         [ -- wires
            --zwire "0" "shift reg out", -- ***
            zwire "control" "N",
            zwire "out" "indicator"
         ]            

example1 =
   makeVI
         [ -- controls
            ("Number of measurements", LvControl LvCtrl (LvI32 10)),
            ("Delay (sec)", LvControl LvCtrl (LvDBL 0.1))
         ]
         [ -- indicators
            ("XY graph", LvIndicator LvIndic (LvCluster [LvArray 1 (LvDBL 0.0), LvArray 1 (LvDBL 0.0)]))
         ]
         [ -- nodes
            ("1000", LvConstant (LvDBL 1000.00)),
            ("Delay * 1000", LvFunction "*"),
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("N", LvControl LvTunCtrl (LvI32 0)),
                  ("i", LvControl LvCtrl (LvI32 0)),
                  ("delay tunnel", LvControl LvTunCtrl (LvDBL 0.0)),
                  ("* tunnel", LvControl LvTunCtrl (LvDBL 0.0))
               ]
               [ -- indicators
                  ("keys array", LvIndicator (LvTunIndic LvAutoIndexing) (LvDBL 0.0)),
                  ("values array", LvIndicator (LvTunIndic LvAutoIndexing) (LvDBL 0.0))
               ]
               [ -- nodes
                  ("WaitUntilNextMs", LvFunction "WaitUntilNextMs"),
                  ("i * delay", LvFunction "*"),
                  ("100", LvConstant (LvI32 100)),
                  ("100 * RandomNumber", LvFunction "*"),
                  ("RandomNumber", LvFunction "RandomNumber")
               ]
               [ -- wires
                  nwire "i" 0            "i * delay" 0,
                  nwire "delay tunnel" 0 "i * delay" 1,
                  zwire "i * delay" "keys array",
                  zwire "* tunnel" "WaitUntilNextMs",
                  nwire "100" 0          "100 * RandomNumber" 0,
                  nwire "RandomNumber" 0 "100 * RandomNumber" 1,
                  zwire "100 * RandomNumber" "values array"
               ]
            )),
            ("Bundle", LvFunction "Bundle")
         ]
         [ -- wires
            zwire "Number of measurements" "N",
            zwire "Delay (sec)" "delay tunnel",
            nwire "Delay (sec)" 0 "Delay * 1000" 0,
            nwire "1000" 0        "Delay * 1000" 1,
            zwire "Delay * 1000" "* tunnel",
            nwire "keys array" 0 "Bundle" 0,
            nwire "values array" 0 "Bundle" 1,
            zwire "Bundle" "XY graph"
         ]

randomXY =
   makeVI
         [ -- controls
            ("Number of measurements", LvControl LvCtrl (LvI32 10)),
            ("Delay (sec)", LvControl LvCtrl (LvDBL 2.0))
         ]
         [ -- indicators
            ("XY graph", LvIndicator LvIndic (LvCluster [LvArray 1 (LvDBL 0.0), LvArray 1 (LvDBL 0.0)]))
         ]
         [ -- nodes
            ("1000", LvConstant (LvDBL 1000.00)),
            ("Delay * 1000", LvFunction "*"),
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("N", LvControl LvTunCtrl (LvI32 0)),
                  ("i", LvControl LvCtrl (LvI32 0)),
                  ("delay tunnel", LvControl LvTunCtrl (LvDBL 0.0)),
                  ("* tunnel", LvControl LvTunCtrl (LvDBL 0.0)),
                  ("feedback loop tunnel", LvControl LvTunCtrl (LvDBL 0.0))
               ]
               [ -- indicators
                  ("keys array", LvIndicator (LvTunIndic LvAutoIndexing)  (LvDBL 0.0)),
                  ("values array", LvIndicator (LvTunIndic LvAutoIndexing)  (LvDBL 0.0))
               ]
               [ -- nodes
                  ("WaitUntilNextMs", LvFunction "WaitUntilNextMs"),
                  ("1", LvConstant (LvI32 1)),
                  ("i + 1", LvFunction "+"),
                  ("i+1 * delay", LvFunction "*"),
                  ("feedback + *", LvFunction "+"),
                  ("100", LvConstant (LvI32 100)),
                  ("100 * RandomNumber", LvFunction "*"),
                  ("RandomNumber", LvFunction "RandomNumber")
               ]
               [ -- wires
                  zwire "* tunnel" "WaitUntilNextMs",
                  nwire "i" 0 "i + 1" 0,
                  nwire "1" 0 "i + 1" 1,
                  nwire "i + 1" 0        "i+1 * delay" 0,
                  nwire "delay tunnel" 0 "i+1 * delay" 1,
                  nwire "feedback loop tunnel" 0 "feedback + *" 0,
                  nwire "i+1 * delay" 0          "feedback + *" 1,
                  zwire "feedback + *" "keys array",
                  nwire "100" 0          "100 * RandomNumber" 0,
                  nwire "RandomNumber" 0 "100 * RandomNumber" 1,
                  zwire "100 * RandomNumber" "values array"
               ]
            )),
            ("insert keys", (LvFunction "InsertIntoArray")),
            ("insert values", (LvFunction "InsertIntoArray")),
            ("bundle", (LvFunction "Bundle")),
            ("max key", (LvFunction "ArrayMax&Min")),
            ("feedback to keys array", (LvFeedbackNode (LvDBL 0.0))),
            ("feedback to values array", (LvFeedbackNode (LvDBL 0.0))),
            ("feedback to loop", (LvFeedbackNode (LvDBL 0.0)))
         ]
         [ -- wires
            zwire "Number of measurements" "N",
            nwire "Delay (sec)" 0 "Delay * 1000" 0,
            nwire "1000" 0        "Delay * 1000" 1,
            zwire "Delay * 1000" "* tunnel",
            zwire "Delay (sec)" "delay tunnel",

            nwire "feedback to keys array" 0 "insert keys" 0,
            nwire "keys array" 0             "insert keys" 1,
            zwire "insert keys" "feedback to keys array",
            zwire "insert keys" "max key",
            zwire "max key" "feedback to loop",
            zwire "feedback to loop" "feedback loop tunnel",

            nwire "feedback to values array" 0 "insert values" 0,
            nwire "values array" 0             "insert values" 1,
            zwire "insert values" "feedback to values array",
            
            nwire "insert keys"   0 "bundle" 0,
            nwire "insert values" 0 "bundle" 1,

            zwire "bundle" "XY graph"
         ]

main = 
   let
      program = randomXY
      state = run (initialState program) program
   in do
      print state


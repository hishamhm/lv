import Data.Sequence (Seq, fromList, index, update, mapWithIndex, fromList)
import qualified Data.Sequence (length)
import Data.List
import Data.Maybe
import Data.Sequence ()
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
                (LvType, Int, Int) -- source type, node, port
                (LvType, Int, Int) -- destination type, node port
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

data LvNodeState = LvNodeState
                     String -- name (for debugging)
                     (Seq (Maybe LvValue)) -- inlet values
                     (Maybe LvState) -- recurse for structures
   deriving Show

data LvState = LvState
                  Int -- timestamp
                  (Seq LvNodeState) -- node states
                  (Seq (Maybe LvValue)) -- indicator values
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
            LvWire (fromType, fromNode, fromPort') (toType, toNode, toPort')
      
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
numberOfInputs fn = trc ("Failing function " ++ fn) $ undefined

applyFunction :: String -> [LvValue] -> [LvValue]

applyFunction "*" [LvDBL a, LvDBL b] = [LvDBL (a * b)]

-- ========================================
-- Dataflow
-- ========================================

-- Produce a sequence of n empty inlets
emptyInlets :: Int -> Seq (Maybe LvValue)
emptyInlets n = fromList (replicate n Nothing)

numberOfControls :: LvVI -> Int
numberOfControls (LvVI (LvPanel controls indicators) diagram) = length controls

numberOfIndicators :: LvVI -> Int
numberOfIndicators (LvVI (LvPanel controls indicators) diagram) = length indicators

initialState :: LvVI -> LvState
initialState (LvVI (LvPanel controls indicators) (LvDiagram nodes wires)) =
   LvState 0 nodeStates indicatorValues
      where
         nodeStates = fromList $ map expandNode nodes
            where
               expandNode :: (String, LvNode) -> LvNodeState
               expandNode (name, node) = 
                  case node of
                     LvSubVI vi       -> LvNodeState name (emptyInlets (numberOfControls vi)) (Just $ initialState vi)
                     LvFunction name  -> LvNodeState name (emptyInlets (numberOfInputs name)) Nothing
                     LvConstant v     -> LvNodeState name (emptyInlets 0) Nothing
                     LvStructure _ vi -> LvNodeState name (emptyInlets (numberOfControls vi)) (Just $ initialState vi)
                     LvFeedbackNode v -> LvNodeState name (emptyInlets 1) Nothing
         indicatorValues = fromList $ map (\(_, LvIndicator _ v) -> Just v) indicators

-- TODO this is implying the ordering given in the description of LvVI,
-- but LabVIEW does not specify it
run :: LvState -> LvVI -> LvState
run state (LvVI (LvPanel controls indicators) (LvDiagram nodes wires)) =
   let
      (_, state')   = trc "controls" $ foldl' fireControl   (0, state)   controls
      (_, state'')  = trc "nodes" $ foldl' fireNode      (0, state')  nodes
   in
      state''
   where

      fire :: LvValue -> (LvType, Int, Int) -> LvState -> LvState      
      fire value addr state =
         foldl' checkWire state wires
            where
            checkWire state (LvWire src dst) =
               if addr == src
               then propagate dst state
               else state
            updatePort port value set = update port (Just value) set
            updateNode node port value nstates = update node nstate nstates
               where
               (LvNodeState name set sub) = index nstates node
               nstate = LvNodeState name (updatePort port value set) sub
            propagate dst@(dtype, dnode, dport) (LvState ts nstates ivs) =
               trc ("propagating wire " @@ dst) $
               if dtype == LvI
               then LvState (ts + 1) nstates (updatePort dnode value ivs)
               else LvState (ts + 1) (updateNode dnode dport value nstates) ivs

      getValue :: [(String, LvControl)] -> Int -> LvValue
      getValue controls idx = (\(name, LvControl typ val) -> val) (controls !! idx)

      takeInputValues :: Int -> LvState -> (LvState, [Maybe LvValue])
      takeInputValues nidx (LvState ts nstates subvi) = (state', toList set)
         where
         nstate@(LvNodeState name set sub) = index nstates nidx
         clearState = LvNodeState name (emptyInlets (Data.Sequence.length set)) sub
         nstates' = update nidx clearState nstates
         state' = LvState (ts + 1) nstates' subvi

      fireControl (cidx, (LvState ts nstates ivs)) (cname, control) =
         trc ("Firing control " ++ cname) $ tsi $
         (cidx + 1, trc ("Fired") $ state')
         where
         state' = fire (getValue controls cidx) (LvC, cidx, 0) state

      fireNode (nidx, state@(LvState ts nstates ivs)) (nname, node) =
         trc ("Firing node " ++ nname) $ tsi $
         (nidx + 1, state')
         where
         state' =
            case node of
            LvSubVI vi -> state -- TODO
            LvFunction name -> 
               let
                  ninputs = trc "noi" $ numberOfInputs name
                  (state1, inlets) = takeInputValues nidx state
               in
                  trc ("Entering LvFunction "++name) $
                  -- if not all inVals have values, don't fire: return unaltered state
                  if elem Nothing inlets
                  then state
                  else 
                     let
                        outVals = applyFunction name (catMaybes inlets)
                        firePort state pidx = fire (outVals !! pidx) (LvN, nidx, pidx) state1
                     in
                        foldl' firePort state [0..((length outVals) - 1)]
            LvConstant value ->
               trc ("firing constant " @@ value) $
               fire value (LvN, nidx, 0) state
            LvStructure typ vi@(LvVI (LvPanel ctrls indics) diagram) ->
               let
                  (state1, inlets) = takeInputValues nidx state
               in
                  case typ of
                  LvFor -> 
                     if isJust (inlets !! 0)
                     then state -- TODO run structure
                     else state
                  LvWhile -> state -- TODO run structure, boolean must be wired
            LvFeedbackNode initVal ->
               let
                  (state1, inlets) = takeInputValues nidx state
                  inVal = inlets !! 0
                  newVal =
                     case inVal of
                     Nothing -> initVal
                     Just v -> v 
               in
                  fire newVal (LvN, nidx, 0) state

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

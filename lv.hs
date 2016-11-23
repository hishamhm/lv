import Data.Sequence (Seq, fromList, index, update, mapWithIndex, fromList, elemIndexL)
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

data LvPortAddr = LvPortAddr LvType Int Int
   deriving Eq

data LvNodeAddr = LvNodeAddr LvType Int
   deriving Eq

data LvWire = LvWire {
                 wSrc :: LvPortAddr,
                 wDst :: LvPortAddr
              }
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

data LvDiagram = LvDiagram {
                    dNodes :: [(String, LvNode)],
                    dWires :: [LvWire]
                 }
   deriving Show

data LvVI = LvVI LvPanel LvDiagram
   deriving Show

data LvVisibleState = LvVisibleState {
                         vsTs :: Int
                      }
   
data LvCont = LvCont {
                 kFn :: (LvVisibleState -> [LvValue] -> [LvValue]),
                 kArgs :: [LvValue]
              }

data LvNodeState = LvNodeState {
                      nsName :: String,
                      nsCont :: (Maybe LvCont),
                      nsInlets :: (Seq (Maybe LvValue)),
                      nsStruct :: (Maybe LvState)
                   }
   deriving Show

data LvState = LvState {
                  sTs :: Int,
                  sSched :: [LvNodeAddr],
                  sNodeStates :: (Seq LvNodeState),
                  sIndicatorValues :: (Seq (Maybe LvValue))
               }
   deriving Show

data FromTo = From | To
   deriving Eq

-- ========================================
-- Utilities
-- ========================================

instance Show LvPortAddr where
   show (LvPortAddr typ nidx pidx) = "{" @@ typ @@@ nidx @@ ", " @@ pidx @@ "}"

instance Show LvNodeAddr where
   show (LvNodeAddr typ nidx) = "{" @@ typ @@@ nidx @@ "}"

instance Show LvCont where
   show (LvCont fn args) = "K(" @@ args @@ ")"

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
            LvWire (LvPortAddr fromType fromNode fromPort') (LvPortAddr toType toNode toPort')
      
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
numberOfInputs fn = trc ("Failing numberOfInputs " ++ fn) $ undefined

applyFunction :: LvVisibleState -> String -> [LvValue] -> (Maybe LvCont, [LvValue])

applyFunction vst "*" [LvDBL a, LvDBL b] = (Nothing, [LvDBL (a * b)])

applyFunction vst fn args = trc ("Failing applyFunction " ++ fn @@@ args) $ undefined

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
   LvState 0 initialSchedule nodeStates indicatorValues
      where
         -- FIXME schedule while loop
         initialSchedule = (map (LvNodeAddr LvC) [0..(length controls) - 1])
                        ++ (map (LvNodeAddr LvN) $ filter (\i -> isConstant (nodes !! i)) [0..(length nodes) - 1])
            where
               isConstant (_, (LvConstant _)) = True
               isConstant _ = False
         nodeStates = fromList $ map expandNode nodes
            where
               expandNode :: (String, LvNode) -> LvNodeState
               expandNode (name, node) = 
                  case node of -- FIXME continuations
                     LvSubVI vi       -> LvNodeState name Nothing (emptyInlets (numberOfControls vi)) (Just $ initialState vi)
                     LvFunction name  -> LvNodeState name Nothing (emptyInlets (numberOfInputs name)) Nothing
                     LvConstant v     -> LvNodeState name Nothing (emptyInlets 0) Nothing
                     LvStructure _ vi -> LvNodeState name Nothing (emptyInlets (numberOfControls vi)) (Just $ initialState vi)
                     LvFeedbackNode v -> LvNodeState name Nothing (emptyInlets 1) Nothing

         indicatorValues = fromList $ map (\(_, LvIndicator _ v) -> Just v) indicators

visible :: LvState -> LvVisibleState
visible state@(LvState ts sched nstate ivs) =
   LvVisibleState ts

-- TODO this is implying the ordering given in the description of LvVI,
-- but LabVIEW does not specify it
run :: LvState -> LvVI -> (LvState, Bool)

run state@(LvState _ [] _ _) vi = (state, False)

run (LvState ts ((LvNodeAddr typ idx):ns) nstates ivs) (LvVI (LvPanel controls indicators) (LvDiagram nodes wires)) =
   (state', True)
   where 
   state0 = LvState ts ns nstates ivs
   state' = case typ of
      LvC ->
         let
            (name, LvControl _ value) = controls !! idx
         in
            fire value (LvPortAddr LvC idx 0) state0
      LvN ->
         let
            updateNode :: LvState -> LvNodeState -> LvState
            updateNode st@(LvState ts ns nstates ivs) newNstate = LvState (ts + 1) ns (update idx newNstate nstates) ivs
         
            nstate@(LvNodeState _ k values sub) = index nstates idx -- FIXME continuation
            state1 =
               if isNothing k
               then updateNode state0 (LvNodeState name k (emptyInlets (Data.Sequence.length values)) sub)
               else state0
            inlets =
               case k of
               Nothing -> toList values
               Just (LvCont kfn kargs) -> map Just kargs
            (name, node) = nodes !! idx
         in
            case node of
            LvSubVI vi -> state1 -- TODO
            LvFunction name -> 
               let
                  (k', outVals) = applyFunction (visible state1) name (catMaybes $ tsi $ inlets)
                  firePort st pidx = fire (outVals !! pidx) (LvPortAddr LvN idx pidx) st
               in
                  if isNothing k'
                  then foldl' firePort state1 [0..((length outVals) - 1)]
                  else updateNode state1 (LvNodeState name k' values sub)
            LvConstant value ->
               trc ("firing constant " @@ value) $
               fire value (LvPortAddr LvN idx 0) state1
            LvStructure typ vi@(LvVI (LvPanel ctrls indics) diagram) ->
               case typ of
               LvFor -> 
                  state1 -- TODO run structure
               LvWhile ->
                  state1 -- TODO run structure, boolean must be wired
            LvFeedbackNode initVal ->
               let
                  inputVal = inlets !! 0
               in
                  fire (fromMaybe initVal inputVal) (LvPortAddr LvN idx 0) state1

   shouldSchedule :: Int -> Seq (Maybe LvValue) -> Bool
   shouldSchedule nidx inlets =
      let
         (name, node) = nodes !! nidx
      in
         case node of
            LvStructure typ vi ->
               case typ of
                  LvFor -> isJust (index inlets 0)
                  LvWhile -> True -- FIXME what if a wire connects in?
            otherwise ->
               elemIndexL Nothing inlets == Nothing -- all inlets have values

   fire :: LvValue -> LvPortAddr -> LvState -> LvState
   fire value addr state =
      trc ("firing" @@@ addr) $
      foldl' checkWire state wires
         where
         checkWire state (LvWire src dst) =
            if addr == src
            then propagate dst state
            else state
         updatePort port value set = update port (Just value) set
         propagate dst@(LvPortAddr dtype dnode dport) (LvState ts sched nstates ivs) =
            trc ("propagating wire " @@ dst) $
            if dtype == LvI
            then LvState (ts + 1) sched nstates (updatePort dnode value ivs)
            else
               let
                  nstate@(LvNodeState name k inlets sub) = index nstates dnode
                  inlets' = updatePort dport value inlets
                  nstate' = LvNodeState name k inlets' sub -- FIXME continuation
                  nstates' = update dnode nstate' nstates
                  sched' = 
                     let
                        entry = LvNodeAddr dtype dnode
                     in
                        if trc ("should schedule " @@ dnode @@ "?") $ tsi $ (shouldSchedule dnode inlets' && not (elem entry sched))
                        then sched ++ [entry]
                        else sched
               in
                  LvState (ts + 1) sched' nstates' ivs

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

loop (state, go) program =
   do
      print state
      if go
      then loop (run state program) program 
      else return ()

main = 
   let
      program = randomXY
   in 
      loop (initialState program, True) program

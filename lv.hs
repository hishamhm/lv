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

data LvControl = LvControl LvValue
               | LvAutoControl
               | LvTunControl
               | LvTunSRControl
               | LvSRControl LvValue
   deriving Show

data LvTunnelMode = LvAutoIndexing
                  | LvLastValue
                  -- TODO other kinds
   deriving Show

data LvIndicator = LvIndicator LvValue
                 | LvTunIndicator LvTunnelMode
                 | LvSRIndicator Int
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
   
data LvCont = LvKFunction {
                 kFn :: LvVisibleState -> [LvValue] -> LvReturn,
                 kArgs :: [LvValue]
              }
            | LvKState LvState

data LvReturn = LvReturn [LvValue]
              | LvContinue LvCont

data LvNodeState = LvNodeState {
                      nsName :: String,
                      nsCont :: Maybe LvCont,
                      nsInlets :: Seq (Maybe LvValue)
                   }
   deriving Show

data LvState = LvState {
                  sTs :: Int,
                  sSched :: [LvNodeAddr],
                  sNodeStates :: Seq LvNodeState,
                  sControlValues :: Seq (Maybe LvValue),
                  sIndicatorValues :: Seq (Maybe LvValue)
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
   show (LvKFunction fn args) = "KFunction(" @@ args @@ ")"
   show (LvKState state) = "KState[" @@ state @@ "]"

indices l = [0 .. ((length l) - 1)]

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

applyFunction :: LvVisibleState -> String -> [LvValue] -> LvReturn

applyFunction vst "*" [LvDBL a, LvDBL b] = LvReturn [LvDBL (a * b)]
applyFunction vst "*" [LvI32 a, LvDBL b] = LvReturn [LvDBL ((fromIntegral a) * b)]
applyFunction vst "*" [LvDBL a, LvI32 b] = LvReturn [LvDBL (a * (fromIntegral b))]
applyFunction vst "*" [LvI32 a, LvI32 b] = LvReturn [LvI32 (a * b)]

applyFunction vst "+" [LvDBL a, LvDBL b] = LvReturn [LvDBL (a + b)]
applyFunction vst "+" [LvI32 a, LvDBL b] = LvReturn [LvDBL ((fromIntegral a) + b)]
applyFunction vst "+" [LvDBL a, LvI32 b] = LvReturn [LvDBL (a + (fromIntegral b))]
applyFunction vst "+" [LvI32 a, LvI32 b] = LvReturn [LvI32 (a + b)]

applyFunction vst "RandomNumber" [] = LvReturn [LvDBL 0.5] -- not very random :)

applyFunction vst@(LvVisibleState ts) "WaitUntilNextMs" [LvDBL msd] =
   let
      ms = floor msd
      nextMs = ts - (ts `mod` ms) + ms
   in
      LvContinue $ LvKFunction waitUntil [LvDBL (fromIntegral nextMs)]

applyFunction vst fn args = trc ("Failing applyFunction " ++ fn @@@ args) $ undefined

waitUntil vst@(LvVisibleState ts) arg@[LvDBL nextMs] =
   if ts >= floor nextMs
   then LvReturn []
   else LvContinue $ LvKFunction waitUntil arg

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

-- FIXME schedule while loop
initialSchedule (LvVI (LvPanel controls indicators) (LvDiagram nodes wires)) =
   (map (LvNodeAddr LvC) (indices controls))
   ++ (map (LvNodeAddr LvN) $ filter (\i -> isBootNode (nodes !! i)) (indices nodes))
   where
      isBootNode (_, (LvConstant _)) = True
      isBootNode (_, (LvFeedbackNode _)) = True
      isBootNode (_, (LvFunction name)) | numberOfInputs name == 0 = True
      isBootNode _ = False

initialState :: Int -> LvVI -> LvState
initialState ts vi@(LvVI (LvPanel controls indicators) (LvDiagram nodes wires)) =
   LvState ts (initialSchedule vi) nodeStates controlValues indicatorValues
      where
         nodeStates = fromList $ map expandNode nodes
            where
               expandNode :: (String, LvNode) -> LvNodeState
               expandNode (name, node) = 
                  case node of
                     LvSubVI vi       -> LvNodeState name Nothing (emptyInlets (numberOfControls vi))
                     LvFunction name  -> LvNodeState name Nothing (emptyInlets (numberOfInputs name))
                     LvConstant v     -> LvNodeState name Nothing (emptyInlets 0)
                     LvStructure _ vi -> LvNodeState name Nothing (emptyInlets (numberOfControls vi))
                     LvFeedbackNode v -> LvNodeState name Nothing (emptyInlets 1)
         controlValues = fromList $ map makeControlValue controls
            where
               makeControlValue (_, LvControl v) = Just v
               makeControlValue (_, LvSRControl v) = Just v
               makeControlValue _ = Nothing
         indicatorValues = fromList $ map makeIndicatorValue indicators
            where
               makeIndicatorValue (_, LvIndicator v) = Just v
               makeIndicatorValue _ = Nothing

feedInletsToVi :: [Maybe LvValue] -> LvState -> LvState

feedInletsToVi inlets state@(LvState ts sched nstates cvs ivs) =
   let
      combine inlets cvs = fromList $ map (\(a,b) -> if isNothing a then b else a) $ zip inlets $ toList cvs
   in
      LvState (ts + 1) sched nstates (combine inlets cvs) ivs

-- Counter is always at index 1
initCounter :: LvState -> LvState
initCounter state@(LvState ts sched nstates cvs ivs) =
   LvState (ts + 1) sched nstates (update 1 (Just $ LvI32 0) cvs) ivs

nextStep :: LvVI -> LvState -> Int -> LvState
nextStep vi@(LvVI (LvPanel controls indicators) (LvDiagram nodes wires)) state@(LvState ts sched nstates cvs ivs) i' =
   let
      cvs' = update 1 (Just $ LvI32 i') cvs
      cvs'' :: Seq (Maybe LvValue)
      cvs'' = foldl' shiftRegister cvs' (zip indicators (toList ivs))
         where
            shiftRegister :: Seq (Maybe LvValue) -> ((String, LvIndicator), Maybe LvValue) -> Seq (Maybe LvValue)
            shiftRegister cvs ((name, (LvSRIndicator cidx)), ival) = 
               trc ("shifting" @@@ ival @@@ "to" @@@ cidx) $
               update cidx (thisOrThat ival (index cvs cidx)) cvs
                  where thisOrThat a b = if isNothing a then b else a
            shiftRegister cvs _ = cvs
   in
      LvState (ts + 1) (initialSchedule vi) nstates cvs'' ivs

visible :: LvState -> LvVisibleState
visible state@(LvState ts sched nstate cvs ivs) =
   LvVisibleState ts

coerceToInt :: LvValue -> LvValue
coerceToInt v@(LvI32 _) = v
coerceToInt v@(LvDBL d) = LvI32 (floor d)

-- TODO this is implying the ordering given in the description of LvVI,
-- but LabVIEW does not specify it
run :: LvState -> LvVI -> (LvState, Bool)

run state@(LvState _ [] _ _ _) vi = (state, False)

run (LvState ts (q@(LvNodeAddr typ idx):qs) nstates cvs ivs) (LvVI (LvPanel controls indicators) (LvDiagram nodes wires)) =
   (state', True)
   where 
   state0 = LvState ts qs nstates cvs ivs
   state' = case typ of
   
      LvC ->
         fire (fromMaybe (trc ("maybe???" @@@ cvs @@@ idx) $ undefined) $ index cvs idx) (LvPortAddr LvC idx 0) state0
         
      LvN ->
         let
            updateNode :: LvState -> LvNodeState -> [LvNodeAddr] -> LvState
            updateNode st@(LvState ts qs nstates cvs ivs) newNstate newSched = LvState (ts + 1) (qs ++ newSched) (update idx newNstate nstates) cvs ivs
         
            nstate@(LvNodeState _ k values) = index nstates idx
            state1 =
               if isNothing k
               then updateNode state0 (LvNodeState name k (emptyInlets (Data.Sequence.length values))) []
               else state0
            inlets =
               case k of
               Nothing -> toList values
               Just (LvKFunction kfn kargs) -> map Just kargs
               Just (LvKState st) -> trc "reading inlets from KState!?" $ undefined
            (name, node) = nodes !! idx
         in
            case node of
            
            LvSubVI vi -> state1 -- TODO initialize state, run subvi
            
            LvFunction name ->
               let
                  ret =
                     case k of
                     Nothing -> applyFunction (visible state1) name (catMaybes $ tsi $ inlets)
                     Just kf -> (kFn kf)      (visible state1)      (catMaybes $ tsi $ inlets)
               in
                  trc ("firing function " @@ name) $
                  case ret of
                  LvReturn outVals ->
                     let
                        firePort st pidx = fire (outVals !! pidx) (LvPortAddr LvN idx pidx) st
                        state2 = updateNode state1 (LvNodeState name Nothing values) []
                     in
                        foldl' firePort state2 (indices outVals)
                  LvContinue k' ->
                     updateNode state1 (LvNodeState name (Just k') values) [q]

            LvConstant value ->
               trc ("firing constant " @@ value) $
               fire value (LvPortAddr LvN idx 0) state1

            LvStructure typ vi@(LvVI (LvPanel ctrls indics) diagram) ->
               trc ("firing structure " @@ typ) $
               case typ of

               LvFor -> 
                  let
                     state2 =
                        case k of
                        Nothing -> initCounter $ feedInletsToVi inlets $ initialState ((sTs state1) + 1) vi
                        Just (LvKState st) -> st
                     (statek, go) = run state2 vi
                     LvI32 n = coerceToInt $ fromMaybe (LvI32 0) $ index cvs 0
                     nextk =
                        if go
                        then Just (LvKState statek)
                        else 
                           let
                              Just (LvI32 i) = (\state@(LvState ts qs ns cvs ivs) -> index cvs 1) statek
                              i' = i + 1
                           in
                              if trc (i' @@ " >= " @@ n) $ i' >= n
                              then Nothing
                              else Just (LvKState (nextStep vi statek i'))
                     nstate' = (LvNodeState name nextk values)
                     thisq = if isJust nextk then [q] else []
                     state3 = LvState ((sTs statek) + 1) (qs ++ thisq) (update idx nstate' nstates) cvs ivs
                     fireIndicator st (pidx, v) = 
                        case v of
                        Nothing -> st
                        Just val -> fire val (LvPortAddr LvN idx pidx) st -- TODO test
                  in
                     if isJust nextk
                     then state3
                     else foldl' fireIndicator state3 (zip (indices indics) (toList $ sIndicatorValues statek))
                     -- TODO fire exits when for finishes

               LvWhile ->
                  state1 -- TODO initialize state, run structure, boolean must be wired

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
            LvStructure typ vi@(LvVI (LvPanel controls indicators) diagram) ->
               case typ of
                  LvFor -> find unfilledTunnel (indices controls) == Nothing
                     where
                        unfilledTunnel cidx = 
                           case controls !! cidx of
                              (name, LvTunControl) -> isNothing (index inlets cidx)
                              (name, LvTunSRControl) -> isNothing (index inlets cidx)
                              otherwise -> False
                  LvWhile -> True -- FIXME what if a wire connects in? does the while-loop wait until there's a value?
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
         propagate dst@(LvPortAddr dtype dnode dport) (LvState ts sched nstates cvs ivs) =
            trc ("propagating wire " @@ dst) $
            if dtype == LvI
            then LvState (ts + 1) sched nstates cvs (updatePort dnode value ivs)
            else
               let
                  nstate@(LvNodeState name k inlets) = index nstates dnode
                  inlets' = updatePort dport value inlets
                  nstate' = LvNodeState name k inlets' -- FIXME continuation
                  nstates' = update dnode nstate' nstates
                  sched' =
                     let
                        entry = LvNodeAddr dtype dnode
                     in
                        if trc ("should schedule " @@ dnode @@ "?") $ tsi $ (shouldSchedule dnode inlets' && not (elem entry sched))
                        then sched ++ [entry]
                        else sched
               in
                  LvState (ts + 1) sched' nstates' cvs ivs

-- ========================================
-- Example programs
-- ========================================

-- TestingFor.vi
-- On continuous run, running this with 0 in the input produces 0,
-- incrementing to 1 it remains at 0, and
-- incrementing to 2 it increments the output forever.
-- This is due to the shift register.
-- Uncommenting the node and the wire that are marked ***
-- stops the endless increment, but it busy-waits.
testingFor =
   makeVI
         [ -- controls
            ("input", LvControl (LvDBL 10.0))
         ]
         [ -- indicators
            ("indicator", LvIndicator (LvDBL (-999.0)))
         ]
         [ -- nodes
            --("0", LvConstant (LvDBL 0.00)), -- ***
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("N", LvTunControl),
                  ("i", LvAutoControl),
                  ("shift reg out", LvSRControl (LvDBL 0.0))
               ]
               [ -- indicators
                  ("shift reg in", LvSRIndicator 2),
                  ("out", LvTunIndicator LvLastValue)
               ]
               [ -- nodes
                  ("+", LvFunction "+")
               ]
               [ -- wires
                  nwire "shift reg out" 0 "+" 0,
                  nwire "i" 0             "+" 1,
                  zwire "+" "shift reg in",
                  zwire "+" "out"
               ]
            ))
         ]
         [ -- wires
            --zwire "0" "shift reg out", -- ***
            zwire "input" "N",
            zwire "out" "indicator"
         ]            

testingFor2 =
   makeVI
         [ -- controls
            ("input", LvControl (LvDBL 10.0))
         ]
         [ -- indicators
            ("indicator", LvIndicator (LvDBL (-999.0)))
         ]
         [ -- nodes
            --("0", LvConstant (LvDBL 0.00)), -- ***
            ("20", LvConstant (LvDBL 20.00)),
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("N", LvTunControl),
                  ("i", LvAutoControl),
                  ("shift reg out", LvTunSRControl)
               ]
               [ -- indicators
                  ("shift reg in", LvSRIndicator 2),
                  ("out", LvTunIndicator LvLastValue)
               ]
               [ -- nodes
                  ("+", LvFunction "+")
               ]
               [ -- wires
                  nwire "shift reg out" 0 "+" 0,
                  nwire "i" 0             "+" 1,
                  zwire "+" "shift reg in",
                  zwire "+" "out"
               ]
            ))
         ]
         [ -- wires
            --zwire "0" "shift reg out", -- ***
            zwire "input" "N",
            zwire "out" "indicator"
         ]            

example1 =
   makeVI
         [ -- controls
            ("Number of measurements", LvControl (LvI32 10)),
            ("Delay (sec)", LvControl (LvDBL 0.1))
         ]
         [ -- indicators
            ("XY graph", LvIndicator (LvCluster [LvArray 1 (LvDBL 0.0), LvArray 1 (LvDBL 0.0)]))
         ]
         [ -- nodes
            ("1000", LvConstant (LvDBL 1000.00)),
            ("Delay * 1000", LvFunction "*"),
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("N", LvTunControl),
                  ("i", LvAutoControl),
                  ("delay tunnel", LvTunControl),
                  ("* tunnel", LvTunControl)
               ]
               [ -- indicators
                  ("keys array", LvTunIndicator LvAutoIndexing),
                  ("values array", LvTunIndicator LvAutoIndexing)
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
            ("Number of measurements", LvControl (LvI32 10)),
            ("Delay (sec)", LvControl (LvDBL 2.0))
         ]
         [ -- indicators
            ("XY graph", LvIndicator (LvCluster [LvArray 1 (LvDBL 0.0), LvArray 1 (LvDBL 0.0)]))
         ]
         [ -- nodes
            ("1000", LvConstant (LvDBL 1000.00)),
            ("Delay * 1000", LvFunction "*"),
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("N", LvTunControl),
                  ("i", LvAutoControl),
                  ("delay tunnel", LvTunControl),
                  ("* tunnel", LvTunControl),
                  ("feedback loop tunnel", LvTunControl)
               ]
               [ -- indicators
                  ("keys array", LvTunIndicator LvAutoIndexing),
                  ("values array", LvTunIndicator LvAutoIndexing)
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
      program = testingFor
   in 
      loop (initialState 0 program, True) program

\documentclass[a4paper]{article}
%\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}
%include polycode.fmt
%include Lv_format.lhs

\begin{document}

\title{An interpreter modelling the semantics of LabVIEW}
\author{Hisham Muhammad}

\maketitle{}

\section{Introduction}

\begin{code}

module LvInterpreter where

import Data.Sequence (Seq, fromList, index, update, mapWithIndex, fromList, elemIndexL)
import qualified Data.Sequence (length)
import Data.List
import Data.Maybe
import Data.Sequence ()
import Data.Foldable (toList)

\end{code}

\section{Debugging}

\begin{code}

import Debug.Trace
import ShowConcat ((@@), (@@@))

-- Debugging on:
tsi :: Show a => a -> a
tsi = traceShowId
trc = trace

-- Debugging off:
--tsi = id
--trc m = id

\end{code}

\section{Representation of programs}

\begin{code}

data LvVI =  LvVI {
               vControls    :: [(String, LvControl)],
               vIndicators  :: [(String, LvIndicator)],
               vNodes       :: [(String, LvNode)],
               vWires       :: [LvWire]
             }
   deriving Show

data LvControl  =  LvControl LvValue
                |  LvAutoControl
                |  LvTunControl
                |  LvTunSRControl
                |  LvSRControl LvValue
   deriving Show

\end{code}

\begin{code}

data LvIndicator  =  LvIndicator LvValue
                  |  LvTunIndicator LvTunnelMode
                  |  LvSRIndicator Int
   deriving Show

\end{code}

\section{Representation of programs}

\begin{code}
data LvTunnelMode  =  LvAutoIndexing -- TODO implement
                   |  LvLastValue
                      -- TODO list other kinds
   deriving Show

data LvNode  =  LvSubVI LvVI
             |  LvFunction String
             |  LvConstant LvValue
             |  LvStructure LvStrucType LvVI
             |  LvFeedbackNode LvValue
   deriving Show

data LvStrucType  =  LvWhile
                  |  LvFor
                     -- TODO LvSequence
   deriving Show

\end{code}

\begin{code}

data LvValue  =  LvEXT Double
              |  LvDBL Double
              |  LvSGL Float
              |  LvFXP Double
              |  LvI64 Int
              |  LvI32 Int
              |  LvI16 Int
              |  LvI8  Int
              |  LvU64 Int
              |  LvU32 Int
              |  LvU16 Int
              |  LvU8  Int
              |  LvBool Bool
              |  LvString String
              |  LvCluster [LvValue]
              |  LvArray Int LvValue
   deriving (Show, Eq)

\end{code}

The following types were not implemented: CSG, CDB, CXT (single-precision,
double-precision and extended-precision complex numbers).

\begin{code}

data LvWire =  LvWire {
                  wSrc :: LvPortAddr,
                  wDst :: LvPortAddr
               }
   deriving Show

data LvPortAddr = LvPortAddr LvNodeType Int Int
   deriving Eq

data LvNodeType  =  LvN
                 |  LvC
                 |  LvI
   deriving (Show, Eq)

\end{code}

\section{Representation of state}

\begin{code}

data LvState =  LvState {
                   sTs :: Int,
                   sSched :: [LvNodeAddr],
                   sNodeStates :: Seq LvNodeState,
                   sControlValues :: Seq (Maybe LvValue),
                   sIndicatorValues :: Seq (Maybe LvValue)
                }
   deriving Show

data LvNodeAddr = LvNodeAddr LvNodeType Int
   deriving Eq

data LvNodeState =  LvNodeState {
                       nsName :: String,
                       nsCont :: Maybe LvCont,
                       nsInlets :: Seq (Maybe LvValue)
                    }
   deriving Show

data LvCont  =  LvKFunction {
                   kFn :: LvVisibleState -> [LvValue] -> LvReturn,
                   kArgs :: [LvValue]
                }
             |  LvKState LvState
 
data LvVisibleState  =  LvVisibleState {
                           vsTs :: Int
                        }

data LvReturn  =  LvReturn [LvValue]
               |  LvContinue LvCont

\end{code}

\section{Utilities}

\begin{code}

instance Show LvPortAddr where
   show (LvPortAddr typ nidx pidx) = "{" ++ show typ ++ " " ++ show nidx ++ ", " ++ show pidx ++ "}"

instance Show LvNodeAddr where
   show (LvNodeAddr typ nidx) = "{" ++ show typ ++ " " ++ show nidx ++ "}"

instance Show LvCont where
   show (LvKFunction fn args) = "KFunction(" ++ show args ++ ")"
   show (LvKState state) = "KState[" ++ show state ++ "]"

indices l = [0 .. ((length l) - 1)]

\end{code}

\section{Program construction}

\begin{code}

data LvStringWire = LvStringWire (String, Int) (String, Int)
   deriving Show

zwire a b = LvStringWire (a, 0) (b, 0)

nwire a i b j = LvStringWire (a, i) (b, j)

makeVI :: [(String, LvControl)] -> [(String, LvIndicator)] -> [(String, LvNode)] -> [LvStringWire] -> LvVI
makeVI controls indicators nodes stringWires =
   LvVI {
      vControls = controls,
      vIndicators = indicators,
      vNodes = nodes,
      vWires = map convert stringWires
   }
   where
      convert :: LvStringWire -> LvWire      
      convert (LvStringWire (src, srcPort) (dst, dstPort)) =
         let
            (srcType,  srcNode,  srcPort')  = findNode controls    LvC  vIndicators  src  srcPort
            (dstType,  dstNode,  dstPort')  = findNode indicators  LvI  vControls    dst  dstPort
         in
            LvWire (LvPortAddr srcType srcNode srcPort') (LvPortAddr dstType dstNode dstPort')

      findNode :: [(String, a)] -> LvNodeType -> (LvVI -> [(String, b)]) -> String -> Int -> (LvNodeType, Int, Int)
      findNode entries etype nodeEntries name port =
         let
            findPort es = elemIndex name $ map fst es
            
            checkNode :: Int -> (String, LvNode) -> Maybe (Int, Int)
            checkNode nodeIdx (_, LvStructure _ nodeVI) =
               case findPort (nodeEntries nodeVI) of
                  Just i -> Just (nodeIdx, i)
                  Nothing -> Nothing
            checkNode nodeIdx (nodeName, node)
               | name == nodeName  = Just (nodeIdx, port)
               | otherwise         = Nothing
         in
            case findPort entries of
               Just i -> (etype, i, port)
               Nothing -> case find isJust $ toList $ mapWithIndex checkNode (fromList nodes) of
                  Just (Just (node, nodePort)) -> (LvN, node, nodePort)
                  Nothing -> error ("No such wire " ++ name ++
                                    " (attempted to connect port " ++ show port ++ ")")

\end{code}

\section{Main loop}

\begin{code}

-- Produce a sequence of n empty inlets
emptyInlets :: Int -> Seq (Maybe LvValue)
emptyInlets n = fromList (replicate n Nothing)

-- FIXME schedule while loop
initialSchedule vi = 
   (map (LvNodeAddr LvC) (indices $ vControls vi))
   ++ (map (LvNodeAddr LvN) $ filter (\i -> isBootNode (vNodes vi !! i)) (indices $ vNodes vi))
   where
      isBootNode (_, (LvConstant _)) = True
      isBootNode (_, (LvFeedbackNode _)) = True
      isBootNode (_, (LvFunction name)) | numberOfInputs name == 0 = True
      isBootNode _ = False

initialState :: Int -> LvVI -> LvState
initialState ts vi = 
   LvState (ts + 1) (initialSchedule vi) nodeStates controlValues indicatorValues
      where
         nodeStates = fromList $ map expandNode (vNodes vi)
            where
               expandNode :: (String, LvNode) -> LvNodeState
               expandNode (name, node) = 
                  case node of
                     LvSubVI vi       -> LvNodeState name Nothing (emptyInlets (length $ vControls vi))
                     LvFunction name  -> LvNodeState name Nothing (emptyInlets (numberOfInputs name))
                     LvConstant v     -> LvNodeState name Nothing (emptyInlets 0)
                     LvStructure _ vi -> LvNodeState name Nothing (emptyInlets (length $ vControls vi))
                     LvFeedbackNode v -> LvNodeState name Nothing (emptyInlets 1)
         controlValues = fromList $ map makeControlValue (vControls vi)
            where
               makeControlValue (_, LvControl v) = Just v
               makeControlValue (_, LvSRControl v) = Just v
               makeControlValue _ = Nothing
         indicatorValues = fromList $ map makeIndicatorValue (vIndicators vi)
            where
               makeIndicatorValue (_, LvIndicator v) = Just v
               makeIndicatorValue _ = Nothing

feedInletsToVi :: [Maybe LvValue] -> LvState -> LvState

feedInletsToVi inlets state@(LvState ts sched nstates cvs ivs) =
   let
      combine inlets cvs = fromList $ map (\(a,b) -> if isNothing a then b else a) $ zip inlets $ toList cvs
   in
      LvState (ts + 1) sched nstates (combine inlets cvs) ivs

iIndex = 0 -- counter control for both 'for' and 'while'
nIndex = 1 -- limit control for 'for'
testIndex = 0 -- test indicator for 'while'

initCounter :: LvState -> LvState
initCounter state@(LvState ts sched nstates cvs ivs) =
   LvState (ts + 1) sched nstates (update iIndex (Just $ LvI32 0) cvs) ivs

nextStep :: LvVI -> LvState -> Int -> LvState
nextStep vi state@(LvState ts sched nstates cvs ivs) i' =
   let
      cvs' = update iIndex (Just $ LvI32 i') cvs
      cvs'' :: Seq (Maybe LvValue)
      cvs'' = foldl' shiftRegister cvs' (zip (vIndicators vi) (toList ivs))
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
run :: LvState -> LvVI -> LvState

run state@(LvState _ [] _ _ _) vi = state

run (LvState ts (q@(LvNodeAddr typ idx):qs) nstates cvs ivs) vi =
   let
      state0 = LvState (ts + 1) qs nstates cvs ivs
   in
      case typ of
   
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
            (name, node) = vNodes vi !! idx
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

            LvStructure typ vi ->
               let
                  getControl   st idx def = fromMaybe def $ (\state@(LvState ts qs ns cvs ivs) -> index cvs idx) st
                  getIndicator st idx def = fromMaybe def $ (\state@(LvState ts qs ns cvs ivs) -> index ivs idx) st
                  
                  runLoop vi shouldStop =
                     let
                        state2 = 
                           case k of
                           Nothing -> initCounter $ feedInletsToVi inlets $ initialState ((sTs state1) + 1) vi
                           Just (LvKState st) -> st
                        statek@(LvState _ qk _ _ _) = run state2 vi
                        nextk =
                           if not $ null qk
                           then Just (LvKState statek)
                           else
                              if shouldStop statek
                              then Nothing
                              else trc ("let's go " @@ (i + 1)) $
                                 trc ("before: " @@ statek) $ Just (LvKState (nextStep vi statek (i + 1)))
                                 where (LvI32 i) = getControl statek iIndex (LvI32 999)
                        nstate' = (LvNodeState name nextk values)
                        thisq = if isJust nextk then [q] else []
                        state3 = LvState ((sTs statek) + 1) (qs ++ thisq) (update idx nstate' nstates) cvs ivs
                        fireIndicator st (pidx, v) = 
                           trc ("indicator " @@ (pidx, v)) $
                           case v of
                           Nothing -> st
                           Just val -> fire val (LvPortAddr LvN idx pidx) st -- TODO test
                     in
                        if isJust nextk
                        then state3
                        else trc "firing indicators" $ foldl' fireIndicator state3 (zip (indices $ vIndicators vi) (toList $ sIndicatorValues statek))
               in
                  trc ("firing structure " @@ typ) $
                  case typ of
   
                  LvFor -> runLoop vi shouldStop
                     where
                        shouldStop st =
                           trc (i' @@ " >= " @@ n) $ (i' >= n)
                           where
                              (LvI32 i) = getControl st iIndex (LvI32 0)
                              i' = i + 1
                              LvI32 n = coerceToInt $ getControl st nIndex (LvI32 0)
                              
                  LvWhile -> runLoop vi shouldStop
                     where
                        shouldStop st =
                           not test
                           where
                              (LvBool test) = getIndicator st testIndex (error "test boolean in 'while' must be set")
      
            LvFeedbackNode initVal ->
               let
                  inputVal = inlets !! 0
               in
                  fire (fromMaybe initVal inputVal) (LvPortAddr LvN idx 0) state1
   where

   fire :: LvValue -> LvPortAddr -> LvState -> LvState
   fire value addr state =
      trc ("firing" @@@ addr) $
      foldl' checkWire state (vWires vi)
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
                        if trc ("should schedule " @@ dnode @@ "?") $ tsi $ (shouldSchedule (vNodes vi !! dnode) inlets' && not (entry `elem` sched))
                        then sched ++ [entry]
                        else sched
               in
                  LvState (ts + 1) sched' nstates' cvs ivs

shouldSchedule :: (String, LvNode) -> Seq (Maybe LvValue) -> Bool
shouldSchedule (name, node) inlets =
   case node of
      LvStructure typ vi ->
         find unfilledTunnel (indices $ vControls vi) == Nothing
            where
               unfilledTunnel cidx = 
                  case vControls vi !! cidx of
                     (name, LvTunControl) -> isNothing (index inlets cidx)
                     (name, LvTunSRControl) -> isNothing (index inlets cidx)
                     otherwise -> False
      otherwise ->
         elemIndexL Nothing inlets == Nothing -- all inlets have values

loop :: LvState -> LvVI -> IO ()
loop state@(LvState _ q _ _ _) program =
   do
      print state
      if not (null q)
      then loop (run state program) program 
      else return ()

\end{code}

\section{Operations}

\begin{code}

numberOfInputs :: String -> Int
numberOfInputs "*" = 2
numberOfInputs "WaitUntilNextMs" = 1
numberOfInputs "RandomNumber" = 0
numberOfInputs "Bundle" = 2
numberOfInputs "+" = 2
numberOfInputs "InsertIntoArray" = 2
numberOfInputs "ArrayMax&Min" = 1
numberOfInputs "<" = 2
numberOfInputs fn = error ("unknown function " ++ fn)

applyFunction :: LvVisibleState -> String -> [LvValue] -> LvReturn

applyFunction vst "+" args = numOp (+) (+) args
applyFunction vst "-" args = numOp (-) (-) args
applyFunction vst "*" args = numOp (*) (*) args
applyFunction vst "/" args = numOp (/) div args
applyFunction vst "<" args = boolOp (<) (<) args
applyFunction vst ">" args = boolOp (>) (>) args

applyFunction vst "RandomNumber" [] = LvReturn [LvDBL 0.5] -- not very random :)

applyFunction vst@(LvVisibleState ts) "WaitUntilNextMs" [LvI32 ms] =
   LvContinue $ LvKFunction waitUntil [LvI32 nextMs]
   where
      nextMs = ts - (ts `mod` ms) + ms
      waitUntil vst@(LvVisibleState ts) arg@[LvI32 nextMs]
         | ts >= nextMs = LvReturn []
         | otherwise    = LvContinue $ LvKFunction waitUntil arg

applyFunction vst "WaitUntilNextMs" [LvDBL msd] = applyFunction vst "WaitUntilNextMs" [LvI32 (floor msd)]

applyFunction vst fn args = error ("No rule to apply " ++ fn ++ " " ++ show args)


binOp opD typD opI typI [LvDBL a, LvDBL b] = LvReturn [typD (opD a b)]
binOp opD typD opI typI [LvI32 a, LvDBL b] = LvReturn [typD (opD (fromIntegral a) b)]
binOp opD typD opI typI [LvDBL a, LvI32 b] = LvReturn [typD (opD a (fromIntegral b))]
binOp opD typD opI typI [LvI32 a, LvI32 b] = LvReturn [typI (opI a b)]
binOp opD typD opI typI _                  = undefined

numOp opD opI args = binOp opD LvDBL opI LvI32 args
boolOp opD opI args = binOp opD LvBool opI LvBool args

\end{code}

\end{document}

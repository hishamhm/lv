\documentclass[a4paper]{article}
%\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}

%BEGIN LYX PREAMBLE

%include polycode.fmt
%include Lv_format.lhs

%END LYX PREAMBLE

\begin{document}

\title{An interpreter modelling the semantics of LabVIEW}
\author{Hisham Muhammad}

\maketitle{}

%BEGIN LYX TEXT

\section{Introduction}

\begin{code}

module LvInterpreter where

import Data.Sequence (Seq, fromList, index, update, mapWithIndex, fromList, elemIndexL)
import qualified Data.Sequence as Seq (length)
import Data.List
import Data.Maybe
import Data.Foldable (toList)
import Data.Generics.Aliases (orElse)

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
   show (LvPortAddr typ nidx pidx) =
      "{" ++ show typ ++ " " ++ show nidx ++ ", " ++ show pidx ++ "}"

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

makeVI ::  [(String, LvControl)] -> [(String, LvIndicator)]
           -> [(String, LvNode)] -> [LvStringWire] -> LvVI
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

      findNode ::  [(String, a)] -> LvNodeType -> (LvVI -> [(String, b)])
                   -> String -> Int -> (LvNodeType, Int, Int)
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

loop :: LvState -> LvVI -> IO ()
loop state@(LvState _ q _ _ _) program =
   do
      print state
      if not (null q)
      then loop (run state program) program 
      else return ()

\end{code}

% FIXME where should this comment go? :
% TODO this is implying the ordering given in the description of LvVI,
% but LabVIEW does not specify it

\begin{code}

run :: LvState -> LvVI -> LvState

run state@(LvState _ [] _ _ _) vi = state

run (LvState ts (q@(LvNodeAddr typ idx):qs) nstates cvs ivs) mainVi =
   let
      state0 = LvState (ts + 1) qs nstates cvs ivs
   in
      runThing q state0 mainVi

\end{code}

\section{Objects and structures}

\subsection{Firing data to objects}

\begin{code}

runThing :: LvNodeAddr -> LvState -> LvVI -> LvState
runThing (LvNodeAddr LvC idx) state0 mainVi =
   fire mainVi (fromMaybe undefined $ index cvs idx) (LvPortAddr LvC idx 0) state0
   where
      cvs = sControlValues state0

runThing q@(LvNodeAddr LvN idx) state0 mainVi =
   let
      nstates = sNodeStates state0
      nstate@(LvNodeState _ k values) = index nstates idx

      state1 =
         if isNothing k
         then updateNode idx state0 (LvNodeState name k (emptyInlets (Seq.length values))) []
         else state0
      inlets =
         case k of
         Nothing -> toList values
         Just (LvKFunction kfn kargs) -> map Just kargs
         Just (LvKState st) -> trc "reading inlets from KState!?" $ undefined
      (name, node) = vNodes mainVi !! idx
      (state2, pvs) = runNode node state0 state1 inlets idx
   in
      foldl' (\s (p, v) -> fire mainVi v (LvPortAddr LvN idx p) s) state2 pvs

-- Produce a sequence of n empty inlets
emptyInlets :: Int -> Seq (Maybe LvValue)
emptyInlets n = fromList (replicate n Nothing)

updateNode :: Int -> LvState -> LvNodeState -> [LvNodeAddr] -> LvState
updateNode idx st newNstate newSched =
   st {
      sTs = (sTs st) + 1,
      sSched = (sSched st) ++ newSched,
      sNodeStates = update idx newNstate (sNodeStates st)
   }

\end{code}

\begin{code}

fire :: LvVI -> LvValue -> LvPortAddr -> LvState -> LvState
fire vi value addr state =
   trc ("firing" @@@ addr @@@ "with value" @@@ value @@@ "\tvi <" @@ (take 30 $ show vi) @@ ">\tstate" @@@ state) $!
   foldl' checkWire state (vWires vi)
      where
      checkWire state (LvWire src dst) =
         if addr == src
         then propagate value vi dst state
         else state

propagate value _ dst@(LvPortAddr LvI dnode dport) state =
   state {
      sTs = ((sTs state) + 1),
      sIndicatorValues = update dnode (Just value) (sIndicatorValues state)
   }

propagate value vi dst@(LvPortAddr dtype dnode dport) state =
   state {
      sTs = ((sTs state) + 1),
      sSched = sched',
      sNodeStates = nstates'
   }
   where
      sched = sSched state
      nstates = sNodeStates state
      nstate@(LvNodeState name k inlets) = index nstates dnode
      inlets' = update dport (Just value) inlets
      nstate' = LvNodeState name k inlets' -- FIXME continuation
      nstates' = update dnode nstate' nstates
      sched' =
         let
            entry = LvNodeAddr dtype dnode
         in
            if shouldSchedule (vNodes vi !! dnode) inlets' && not (entry `elem` sched)
            then sched ++ [entry]
            else sched

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

\end{code}

\subsection{Nodes}

\begin{code}

runNode :: LvNode -> LvState -> LvState -> [Maybe LvValue] -> Int -> (LvState, [(Int, LvValue)])

runNode (LvSubVI subVi) _ state1 _ _ = (state1, []) -- TODO initialize state, run subvi

runNode (LvFunction name) state0 state1 inlets idx =
   let
      nstates = sNodeStates state0 -- REFACTOR was 0
      nstate@(LvNodeState _ k values) = index nstates idx
      q = (LvNodeAddr LvN idx)

      visible :: LvState -> LvVisibleState
      visible state = LvVisibleState (sTs state)

      ret =
         case k of
         Nothing -> applyFunction (visible state1) name (catMaybes $ tsi $ inlets)
         Just kf -> (kFn kf)      (visible state1)      (catMaybes $ tsi $ inlets)
   in
      trc ("firing function " @@ name) $
      case ret of
      LvReturn outVals ->
         (state2, pvs)
         where
            state2 = updateNode idx state1 (LvNodeState name Nothing values) []
            pvs = zip (indices outVals) outVals
      LvContinue k' ->
         (updateNode idx state1 (LvNodeState name (Just k') values) [q], [])

runNode (LvConstant value) _ state1 _ _ =
   trc ("firing constant " @@ value) $
   (state1, [(0, value)])

\end{code}

In our model, an |LvFeedbackNode| always takes an initialization value. In the
LabVIEW UI, this value can be left out, in which case a default value of zero
is implied.

\begin{code}

runNode (LvFeedbackNode initVal) _ state1 inlets _ =
   (state1, [(0, fromMaybe initVal (inlets !! 0) )])

\end{code}

\begin{code}

runNode (LvStructure LvFor subVi) state0 state1 inlets idx =
   trc ("firing for") $
   loopStructure subVi shouldStop state0 state1 idx inlets
   where
      shouldStop st =
         trc (i' @@ " >= " @@ n) $ (i' >= n)
         where
            (LvI32 i) = getControl st iIndex (LvI32 0)
            i' = i + 1
            LvI32 n = coerceToInt $ getControl st nIndex (LvI32 0)

runNode (LvStructure LvWhile subVi) state0 state1 inlets idx =
   trc ("firing while") $
   loopStructure subVi shouldStop state0 state1 idx inlets
   where
      shouldStop st =
         not test
         where
            (LvBool test) = getIndicator st testIndex (error "test boolean in 'while' must be set")

\end{code}

\begin{code}

loopStructure :: LvVI -> (LvState -> Bool) -> LvState -> LvState -> Int -> [Maybe LvValue] -> (LvState, [(Int, LvValue)])
loopStructure loopVi shouldStop state0 state1 idx inlets =
   let
      nstates = sNodeStates state0 -- REFACTOR was 0
      nstate@(LvNodeState name k values) = index nstates idx
      q = (LvNodeAddr LvN idx)

      statek = 
         case k of
         Nothing -> initCounter $ feedInletsToVi inlets $ initialState ((sTs state1) + 1) loopVi
         Just (LvKState st) -> st
      statek'@(LvState _ qk _ _ _) = run statek loopVi
      nextk =
         if not $ null qk
         then Just (LvKState statek')
         else
            if shouldStop statek'
            then Nothing
            else trc ("let's go " @@ (i + 1)) $
               trc ("before: " @@ statek') $ Just (LvKState (nextStep loopVi statek' (i + 1)))
               where (LvI32 i) = getControl statek' iIndex (LvI32 999)
      nstate' = (LvNodeState name nextk values)
      thisq = if isJust nextk then [q] else []
      state2 = state1 { -- REFACTOR state0 to state1
         sTs = (sTs statek') + 1,
         sSched = (sSched state1) ++ thisq,
         sNodeStates = update idx nstate' nstates
      }
      pvs = map (\(p,v) -> (p, fromMaybe undefined v))
          $ filter (\(p,v) -> not $ isNothing v)
          $ zip (indices $ vIndicators loopVi) (toList $ sIndicatorValues statek') 
   in
      if isJust nextk 
      then (state2, [])
      else (state2, pvs)

initialState :: Int -> LvVI -> LvState
initialState ts vi = 
   LvState {
      sTs               = ts + 1,
      sSched            = initialSchedule vi,
      sNodeStates       = fromList $ map (makeNodeState)             (vNodes vi),
      sControlValues    = fromList $ map (makeControlValue . snd)    (vControls vi),
      sIndicatorValues  = fromList $ map (makeIndicatorValue . snd)  (vIndicators vi)
   }
   where
      makeNodeState :: (String, LvNode) -> LvNodeState
      makeNodeState (name, node) = LvNodeState name Nothing (emptyInlets $ nInlets node)
      nInlets (LvSubVI vi)        = length $ vControls vi
      nInlets (LvFunction name)   = numberOfInputs name
      nInlets (LvConstant v)      = 0
      nInlets (LvStructure _ vi)  = length $ vControls vi
      nInlets (LvFeedbackNode v)  = 1
      makeControlValue :: LvControl -> Maybe LvValue
      makeControlValue (LvControl    v)  = Just v
      makeControlValue (LvSRControl  v)  = Just v
      makeControlValue _                 = Nothing
      makeIndicatorValue :: LvIndicator -> Maybe LvValue
      makeIndicatorValue (LvIndicator v)  = Just v
      makeIndicatorValue _                = Nothing

-- FIXME schedule while loop
initialSchedule :: LvVI -> [LvNodeAddr]
initialSchedule vi = 
   (map (LvNodeAddr LvC) (indices $ vControls vi))
   ++ (map (LvNodeAddr LvN) $ filter (\i -> isBootNode (vNodes vi !! i)) (indices $ vNodes vi))
   where
      isBootNode (_, (LvConstant _)) = True
      isBootNode (_, (LvFeedbackNode _)) = True
      isBootNode (_, (LvFunction name)) | numberOfInputs name == 0 = True
      isBootNode _ = False

feedInletsToVi :: [Maybe LvValue] -> LvState -> LvState
feedInletsToVi inlets state =
   state {
      sTs = (sTs state) + 1,
      sControlValues = fromList $ map (uncurry orElse)
                                $ zip inlets (toList (sControlValues state))
   }      

iIndex = 0 -- counter control for both 'for' and 'while'
nIndex = 1 -- limit control for 'for'
testIndex = 0 -- test indicator for 'while'

initCounter :: LvState -> LvState
initCounter state =
   state {
      sTs = (sTs state) + 1,
      sControlValues = update iIndex (Just $ LvI32 0) (sControlValues state)
   }

nextStep :: LvVI -> LvState -> Int -> LvState
nextStep vi state i' =
   state {
      sTs = (sTs state) + 1,
      sSched = initialSchedule vi,
      sControlValues = cvs''
   }
   where
      cvs' = update iIndex (Just $ LvI32 i') (sControlValues state)
      cvs'' :: Seq (Maybe LvValue)
      cvs'' = foldl' shiftRegister cvs' (zip (vIndicators vi) (toList (sIndicatorValues state)))
         where
            shiftRegister :: Seq (Maybe LvValue) -> ((String, LvIndicator), Maybe LvValue) -> Seq (Maybe LvValue)
            shiftRegister cvs ((name, (LvSRIndicator cidx)), ival) = 
               update cidx (ival `orElse` (index cvs cidx)) cvs
            shiftRegister cvs _ = cvs

coerceToInt :: LvValue -> LvValue
coerceToInt v@(LvI32 _) = v
coerceToInt v@(LvDBL d) = LvI32 (floor d)

getControl :: LvState -> Int -> LvValue -> LvValue
getControl st idx def = fromMaybe def $ index (sControlValues st) idx

getIndicator :: LvState -> Int -> LvValue -> LvValue
getIndicator st idx def = fromMaybe def $ index (sIndicatorValues st) idx

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

%END LYX TEXT

\end{document}

\begin{code}
{-# LANGUAGE BangPatterns #-}
\end{code}
\documentclass[a4paper]{article}
%\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}
\usepackage{color}
\definecolor{darkblue}{rgb}{0,0,0.5}
\usepackage[unicode=true,pdfusetitle,
 bookmarks=true,bookmarksnumbered=false,bookmarksopen=false,
 breaklinks=true,pdfborder={0 0 0},backref=false,colorlinks=true,linkcolor=darkblue]
 {hyperref}

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
import qualified Data.Sequence as Seq (length, take)
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Foldable (toList)
import Data.Generics.Aliases (orElse)

\end{code}

\section{Debugging}

\begin{code}

import Debug.Trace

-- Debugging on:
tsi :: Show a => a -> a
tsi = traceShowId
trc :: String -> a -> a
trc = trace

shw x = (chr 27 : "[1;33m") ++ show x ++ (chr 27 : "[0m")

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
             |  LvWhile LvVI
             |  LvFor LvVI
             |  LvSequence LvVI
             |  LvCase [LvVI]
             |  LvFeedbackNode LvValue
   deriving Show

\end{code}

Unlike LabVIEW, our implementation allows arbitrarily recursive types (e.g. a
cluster of arrays of clusters). In the |LvArr| constructor, we have an 
extra value that serves as a marker for the type of the array values.

\begin{code}

data LvValue  =  LvDBL Double
              |  LvI32 Int
              |  LvSTR String
              |  LvBool Bool
              |  LvCluster [LvValue]
              |  LvArr [LvValue]
   deriving (Show, Eq, Ord)

\end{code}

We chose to implement only one floating-point and one integer type. Besides
the types listed above, LabVIEW includes the following types in total:
extended and single-precision floating-point numbers; fixed-point numbers;
signed and unsigned integers of 8, 16, 32 and 64 bits; single, double and
extended-precision complex numbers.

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

{-"\hypertarget{LvState}{}\nolinebreak"-}
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
                       nsInputs :: Seq (Maybe LvValue)
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
   show (LvKFunction _ args) = "KFunction(" ++ show args ++ ")"
   show (LvKState state) = "KState[" ++ show state ++ "]"

indices :: [a] -> [Int]
indices l = [0 .. (length l - 1)]

\end{code}

\section{Program construction}

\begin{code}

data LvStringWire = LvStringWire String String
   deriving Show

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
      convert (LvStringWire src dst) =
         let
            (srcType,  srcNode,  srcPort')  = findNode controls    LvC  vIndicators  src
            (dstType,  dstNode,  dstPort')  = findNode indicators  LvI  vControls    dst
         in
            LvWire (LvPortAddr srcType srcNode srcPort') (LvPortAddr dstType dstNode dstPort')

      findIndex :: [(String, a)] -> String -> Maybe Int
      findIndex es name = elemIndex name $ map fst es
      
      must :: (String -> Maybe a) -> String -> a
      must fn name = fromMaybe (error ("No such entry " ++ name)) (fn name)

      findNode ::  [(String, a)] -> LvNodeType -> (LvVI -> [(String, b)])
                   -> String -> (LvNodeType, Int, Int)
      findNode entries etype nodeEntries name
       | isJust $ find (== ':') name =
            let 
               [nodeName, portName] = splitOn ":" name
               node = (must . flip lookup) nodes nodeName
               findPort (LvWhile subVi)     = must $ findIndex (nodeEntries subVi)
               findPort (LvFor subVi)       = must $ findIndex (nodeEntries subVi)
               findPort (LvSequence subVi)  = must $ findIndex (nodeEntries subVi)
               findPort (LvCase subVis)     = must $ findIndex (nodeEntries (head subVis))
               findPort (LvFunction _)      = \s -> if null s then 0 else read s
               findPort _                   = \s -> 0
            in
               (LvN, (must . findIndex) nodes nodeName, findPort node portName)
       | otherwise =
          case findIndex entries name of
          Just i -> (etype, i, 0)
          Nothing -> findNode entries etype nodeEntries (name ++ ":0")

\end{code}

\section{Main loop}

\begin{code}

loop :: LvState -> LvVI -> IO ()
loop state@(LvState _ q _ _ _) program =
   do
      print state
      case q of
       []  -> return ()
       _   -> loop (run state program) program 

\end{code}

\begin{code}

run :: LvState -> LvVI -> LvState

run state@(LvState _ [] _ _ _) _ = state

run (LvState ts (q:qs) nstates cvs ivs) mainVi =
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

runThing (LvNodeAddr LvN idx) state0 mainVi =
   let
      nstates = sNodeStates state0
      nstate = index nstates idx
      k = nsCont nstate
      clearState = nstate { nsInputs = clear }
                   where clear = emptyInputs $ Seq.length $ nsInputs nstate
      state1 =
         if isNothing k
         then updateNode idx state0 clearState []
         else state0
      inputs =
         trc ("CHECK INPUTS (LVN, " ++ shw idx ++ ") ON STATE " ++ shw state0 ++ " FOR VI " ++ shw mainVi ++ " WITH K " ++ shw k) $
         case k of
         Nothing -> toList (nsInputs nstate)
         Just (LvKFunction _ kargs) -> map Just kargs
         Just (LvKState _) -> undefined
      (_, node) = vNodes mainVi !! idx
      (state2, pvs) = runNode node state1 inputs idx
   in
      trace ("RUNTHING (LVN, " ++ shw idx ++ ") ON STATE " ++ shw state0 ++ " FOR VI " ++ shw mainVi) $
      foldl' (\s (p, v) -> fire mainVi v (LvPortAddr LvN idx p) s) state2 pvs

-- Produce a sequence of n empty inputs
emptyInputs :: Int -> Seq (Maybe LvValue)
emptyInputs n = fromList (replicate n Nothing)

updateNode :: Int -> LvState -> LvNodeState -> [LvNodeAddr] -> LvState
updateNode idx st newNstate newSched =
   st {
      sTs = sTs st + 1,
      sSched = sSched st ++ newSched,
      sNodeStates = update idx newNstate (sNodeStates st)
   }

\end{code}

\begin{code}

fire :: LvVI -> LvValue -> LvPortAddr -> LvState -> LvState
fire vi value addr state =
   trc ("firing " ++ shw addr ++ " with value " ++ shw value ++ "\tvi <" ++ shw (take 30 (show vi)) ++ ">\tstate " ++ shw state) $!
   foldl' checkWire state (vWires vi)
      where
      checkWire s (LvWire src dst) =
         if addr == src
         then propagate value vi dst s
         else s

propagate :: LvValue -> LvVI -> LvPortAddr -> LvState -> LvState
propagate value vi (LvPortAddr LvI dnode _) state =
   let
      (_, indicator) = vIndicators vi !! dnode
      newValue = case indicator of
                 LvIndicator _                 -> value
                 LvSRIndicator _               -> value
                 LvTunIndicator LvLastValue    -> value
                 LvTunIndicator LvAutoIndexing -> insertIntoArray (fromMaybe (LvArr []) (index (sIndicatorValues state) dnode)) value []
   in
      state {
         sTs = sTs state + 1,
         sIndicatorValues = update dnode (Just newValue) (sIndicatorValues state)
      }

propagate value vi (LvPortAddr LvN dnode dport) state =
   state {
      sTs = sTs state + 1,
      sSched = sched',
      sNodeStates = nstates'
   }
   where
      nstates = sNodeStates state
      nstate = index nstates dnode
      inputs' = update dport (Just value) (nsInputs nstate)
      nstates' = update dnode (nstate { nsInputs = inputs' } ) nstates
      sched' =
         let
            sched = sSched state
            entry = LvNodeAddr LvN dnode
         in
            if shouldSchedule (snd $ vNodes vi !! dnode) inputs' && entry `notElem` sched
            then sched ++ [entry]
            else sched

shouldSchedule :: LvNode -> Seq (Maybe LvValue) -> Bool
shouldSchedule node inputs =
   case node of
      LvWhile vi -> shouldScheduleSubVI vi inputs
      LvFor vi -> shouldScheduleSubVI vi inputs
      LvSequence vi -> shouldScheduleSubVI vi inputs
      LvCase vis -> shouldScheduleSubVI (head vis) inputs
      LvFunction name ->
         isNothing $ elemIndexL Nothing mandatoryInputs
         where
            mandatoryInputs =
               case nrMandatoryInputs name of
               Nothing -> inputs
               Just n  -> Seq.take n inputs
      LvFeedbackNode _ ->
         False
      _ ->
         isNothing $ elemIndexL Nothing inputs

shouldScheduleSubVI :: LvVI -> Seq (Maybe LvValue) -> Bool
shouldScheduleSubVI vi inputs = 
   isNothing $ find unfilledTunnel (indices $ vControls vi)
      where
         unfilledTunnel cidx = 
            case vControls vi !! cidx of
               (_, LvTunControl) -> isNothing (index inputs cidx)
               (_, LvTunSRControl) -> isNothing (index inputs cidx)
               _ -> False

\end{code}

\subsection{Nodes}

\begin{code}

runNode ::  LvNode -> LvState -> [Maybe LvValue] -> Int
            -> (LvState, [(Int, LvValue)])

runNode (LvSubVI _) state1 _ _ = (state1, []) -- TODO initialize state, run subVI

runNode (LvFunction name) state1 inputs idx =
   let
      nstates = sNodeStates state1
      nstate = index nstates idx

      visible :: LvState -> LvVisibleState
      visible state = LvVisibleState (sTs state)

      ret =
         case nsCont nstate of
         Nothing -> applyFunction (visible state1) name inputs
         Just kf -> kFn kf        (visible state1)      (catMaybes inputs)
   in
      trc ("firing function " ++ name) $
      case ret of
      LvReturn outVals ->
         (state2, pvs)
         where
            state2 = updateNode idx state1 nstate{ nsCont = Nothing } []
            pvs = zip (indices outVals) outVals
      LvContinue k' ->
         (updateNode idx state1 nstate{ nsCont = Just k' } [LvNodeAddr LvN idx], [])

runNode (LvConstant value) state1 _ _ =
   trc ("firing constant " ++ shw value)
   (state1, [(0, value)])

\end{code}

In our model, an |LvFeedbackNode| always takes an initialization value. In the
LabVIEW UI, this value can be left out, in which case a default value of zero
is implied.

\begin{code}

runNode (LvFeedbackNode initVal) state1 inputs _ =
   (state1, [(0, fromMaybe initVal (head inputs) )])

\end{code}

\begin{code}

-- TODO for when no N is set and an array is given as input tunnel
-- TODO check what happens when both are given
runNode (LvFor subVi) state1 inputs idx =
   trc "firing for" $
   runStructure subVi initCounter shouldStop state1 idx inputs
   where
      shouldStop st =
         trc (shw i' ++ " >= " ++ shw n) (i' >= n)
         where
            (LvI32 i) = getControl st iIndex (LvI32 0)
            i' = i + 1
            LvI32 n = coerceToInt $ getControl st nIndex (LvI32 0)

runNode (LvWhile subVi) state1 inputs idx =
   trc "firing while" $
   runStructure subVi initCounter shouldStop state1 idx inputs
   where
      shouldStop st =
         not test
         where
            (LvBool test) = getIndicator st testIndex
                            (error "test boolean in 'while' must be set")

runNode (LvSequence vi) state1 inputs idx =
   let
      shouldStop st = True
      (state2, pvs) = runStructure vi id shouldStop state1 idx inputs -- TODO fix inputs
      nstate2 = index (sNodeStates state2) idx
      nextq = [ (0, LvBool True) | isNothing (nsCont nstate2) ]
   in
      (state2, pvs ++ nextq)

runNode (LvCase vis) state1 inputs idx =
   let
      nstate1 = index (sNodeStates state1) idx
      n = case nsCont nstate1 of
             Nothing ->
                case inputs of
                Just (LvI32 i) : _  -> i
                _                   -> 0
             Just _ ->
                (\(LvI32 i) -> i) $
                fromMaybe (error "no input 0!?") $ index (nsInputs nstate1) 0
      shouldStop st = True
      (state2, pvs) = runStructure (vis !! n) id shouldStop state1 idx inputs
      state3 =
         case nsCont nstate1 of
            Nothing ->
               let
                  nstate2 = index (sNodeStates state2) idx
                  nstate3 = nstate2 { nsInputs = update 0 (Just (LvI32 n)) (nsInputs nstate2) }
               in
                  updateNode idx state2 nstate3 []
            Just _ ->
               state2
   in
      (state3, pvs)

\end{code}

\begin{code}

runStructure ::  LvVI
                 -> (LvState -> LvState)
                 -> (LvState -> Bool)
                 -> LvState -> Int -> [Maybe LvValue]
                 -> (LvState, [(Int, LvValue)])
runStructure subVi initState shouldStop state1 idx inputs =
   let
      nstates = sNodeStates state1
      nstate = index nstates idx

      statek = 
         case nsCont nstate of
         Nothing -> initState
                    $ feedInputsToVI inputs
                    $ initialState (sTs state1 + 1) subVi
         Just (LvKState st) -> st
      statek'@(LvState _ qk _ _ _) = run statek subVi
      nextk
         | not $ null qk      = trace ("GOT QK " ++ shw qk ++ " IN STATEK " ++ shw statek) $ Just (LvKState statek')
         | shouldStop statek' = Nothing
         | otherwise =
              trc ("let's go " ++ shw (i + 1)) $
              trc ("before: " ++ shw statek') $
              Just (LvKState (nextStep subVi statek' (i + 1)))
         where (LvI32 i) = getControl statek' iIndex undefined
      nstate' = nstate { nsCont = nextk }
      qMe = trace ("QUEUED MYSELF? " ++ (shw $ isJust nextk) ++ "(LvN " ++ shw idx ++ ")") $ [LvNodeAddr LvN idx | isJust nextk]
      state2 = state1 {
         sTs = sTs statek' + 1,
         sSched = sSched state1 ++ qMe,
         sNodeStates = update idx nstate' nstates
      }
      pvs = map (\(p,v) -> (p, fromMaybe undefined v))
            $ filter (\(_,v) -> isJust v)
            $ zip (indices $ vIndicators subVi) (toList $ sIndicatorValues statek')
   in
      if isJust nextk 
      then (state2, [])
      else (state2, pvs)

\end{code}

A node is fired when all its connected inputs have incoming data. We
specifically check for connected inputs because some LabVIEW nodes have
optional inputs. We assume here for simplicity that the type-checking step
prior to execution verified that the correct set of mandatory inputs has been
connected. Here, we derive the number of connections of a node from the
list of wires.

\begin{code}

nrConnectedInputs :: Int -> LvVI -> Int
nrConnectedInputs idx vi =
   1 + foldl' maxInput (-1) (vWires vi)
   where
      maxInput :: Int -> LvWire -> Int
      maxInput mx (LvWire _ (LvPortAddr LvN i n)) | i == idx = max mx n
      maxInput mx _ = mx

\end{code}

\begin{code}

initialState :: Int -> LvVI -> LvState
initialState ts vi = 
   LvState {
      sTs               = ts + 1,
      sSched            = initialSchedule vi,
      sNodeStates       = fromList $ mapIdx makeNodeState            (vNodes vi),
      sControlValues    = fromList $ map (makeControlValue . snd)    (vControls vi),
      sIndicatorValues  = fromList $ map (makeIndicatorValue . snd)  (vIndicators vi)
   }
   where
      makeNodeState :: (Int, (String, LvNode)) -> LvNodeState
      makeNodeState (i, (name, node)) = LvNodeState  name Nothing
                                                     (emptyInputs $ nrInputs i node)

      nrInputs :: Int -> LvNode -> Int
      nrInputs _ (LvSubVI subVi)      = length $ vControls subVi
      nrInputs i (LvFunction _)       = nrConnectedInputs i vi
      nrInputs _ (LvConstant _)       = 0
      nrInputs _ (LvFor subVi)        = length $ vControls subVi
      nrInputs _ (LvWhile subVi)      = length $ vControls subVi
      nrInputs _ (LvSequence subVi)   = length $ vControls subVi
      nrInputs _ (LvCase subVis)      = length $ vControls (head subVis)
      nrInputs _ (LvFeedbackNode _)   = 1

      makeControlValue :: LvControl -> Maybe LvValue
      makeControlValue (LvControl    v)  = Just v
      makeControlValue (LvSRControl  v)  = Just v
      makeControlValue _                 = Nothing
      makeIndicatorValue :: LvIndicator -> Maybe LvValue
      makeIndicatorValue (LvIndicator v)  = Just v
      makeIndicatorValue _                = Nothing
      
      mapIdx :: ((Int, a) -> b) -> [a] -> [b]
      mapIdx fn l = zipWith (curry fn) (indices l) l

\end{code}

Note that the code below implies the initial schedule follows the order of
node given in the description of LvVI, but LabVIEW does not specify it.

\begin{code}

-- FIXME schedule while loop
initialSchedule :: LvVI -> [LvNodeAddr]
initialSchedule vi = 
   map (LvNodeAddr LvC) (indices $ vControls vi)
   ++ map (LvNodeAddr LvN) (filter  (\i -> isBootNode i (vNodes vi !! i))
                                    (indices $ vNodes vi))
   where
      isBootNode _ (_, LvConstant _) = True
      isBootNode _ (_, LvFeedbackNode _) = True
      isBootNode i (_, LvFunction _) | nrConnectedInputs i vi == 0 = True
      isBootNode _ _ = False

feedInputsToVI :: [Maybe LvValue] -> LvState -> LvState
feedInputsToVI inputs state =
   state {
      sTs = sTs state + 1,
      sControlValues = fromList $ zipWith orElse inputs (toList (sControlValues state))
   }  

iIndex :: Int
iIndex = 0 -- counter control for both 'for' and 'while'
nIndex :: Int
nIndex = 1 -- limit control for 'for'
testIndex :: Int
testIndex = 0 -- test indicator for 'while'

initCounter :: LvState -> LvState
initCounter state =
   state {
      sTs = sTs state + 1,
      sControlValues = update iIndex (Just $ LvI32 0) (sControlValues state)
   }

nextStep :: LvVI -> LvState -> Int -> LvState
nextStep vi state i' =
   state {
      sTs = sTs state + 1,
      sSched = initialSchedule vi,
      sControlValues = cvs''
   }
   where
      cvs' = update iIndex (Just $ LvI32 i') (sControlValues state)
      cvs'' :: Seq (Maybe LvValue)
      cvs'' = foldl' shiftRegister cvs'
              $ zip (vIndicators vi) (toList (sIndicatorValues state))
      shiftRegister ::  Seq (Maybe LvValue) -> ((String, LvIndicator), Maybe LvValue)
                        -> Seq (Maybe LvValue)
      shiftRegister cvs ((_, LvSRIndicator cidx), ival) = 
         update cidx (ival `orElse` index cvs cidx) cvs
      shiftRegister cvs _ = cvs

coerceToInt :: LvValue -> LvValue
coerceToInt v@(LvI32 _) = v
coerceToInt (LvDBL d) = LvI32 (floor d)

getControl :: LvState -> Int -> LvValue -> LvValue
getControl st idx def = fromMaybe def $ index (sControlValues st) idx

getIndicator :: LvState -> Int -> LvValue -> LvValue
getIndicator st idx def = fromMaybe def $ index (sIndicatorValues st) idx

\end{code}

\section{Operations}

\begin{code}

isOp :: String -> Bool
isOp f = f `elem` ["+", "-", "*", "/", ">", "<"]

applyFunction :: LvVisibleState -> String -> [Maybe LvValue] -> LvReturn

applyFunction _ "RandomNumber" [] = LvReturn [LvDBL 0.5] -- not very random :)

applyFunction _ "ArrayMax&Min" [Just (LvArr a)] =
   if null a
   then LvReturn [LvDBL 0,  LvI32 0,       LvDBL 0,  LvI32 0]
   else LvReturn [maxVal,   LvI32 maxIdx,  minVal,   LvI32 minIdx]
        where
           (maxVal, maxIdx) = foldPair (>) a
           (minVal, minIdx) = foldPair (<) a
           foldPair op l = foldl1 (\(x,i) (y,j) -> if op x y
                                                   then (x,i)
                                                   else (y,j))
                                  (zip l (indices l))

\end{code}

\begin{code}

applyFunction _ "InsertIntoArray" (Just arr : Just vs : idxs) =
   LvReturn [insertIntoArray arr vs (map numIdx idxs)]
   where
      numIdx i = case i of
                 Nothing -> -1
                 Just (LvI32 n) -> n

applyFunction _ "Bundle" args = 
   LvReturn [LvCluster (catMaybes args)]

applyFunction (LvVisibleState ts) "WaitUntilNextMs" [Just (LvI32 ms)] =
   LvContinue $ LvKFunction waitUntil [LvI32 nextMs]
   where
      nextMs = ts - (ts `mod` ms) + ms
      waitUntil (LvVisibleState now) arg@[LvI32 stop]
         | now >= stop  = LvReturn []
         | otherwise    = LvContinue $ LvKFunction waitUntil arg

applyFunction vst "WaitUntilNextMs" [Just (LvDBL msd)] =
   applyFunction vst "WaitUntilNextMs" [Just (LvI32 (floor msd))]

applyFunction _ "+" args = numOp   (+) (+)  args
applyFunction _ "-" args = numOp   (-) (-)  args
applyFunction _ "*" args = numOp   (*) (*)  args
applyFunction _ "/" args = numOp   (/) div  args
applyFunction _ "<" args = boolOp  (<) (<)  args
applyFunction _ ">" args = boolOp  (>) (>)  args

applyFunction _ fn args =
   error ("No rule to apply " ++ fn ++ " " ++ show args)

ndims :: LvValue -> Int
ndims (LvArr (v:_))  = 1 + ndims v
ndims (LvArr [])     = 1
ndims _              = 0

eqDim :: LvValue -> LvValue -> Bool
eqDim a b = ndims a == ndims b
neDim :: LvValue -> LvValue -> Bool
neDim a b = ndims a == ndims b + 1

zero :: LvValue -> LvValue
zero (LvArr l@(x:_))  = LvArr (replicate (length l) (zero x))
zero (LvDBL _)        = LvDBL 0.0
zero (LvI32 _)        = LvI32 0
zero (LvSTR _)        = LvSTR ""
zero (LvBool _)       = LvBool False
zero (LvCluster c)    = LvCluster (map zero c)
zero (LvArr [])       = LvArr []

resizeCurr :: (LvValue -> LvValue) -> [LvValue] -> [LvValue] -> [LvValue]
resizeCurr childOp xs@(x:_) ys = map childOp $ take (length xs) $ ys ++ (repeat . zero) x

childResizer :: LvValue -> (LvValue -> LvValue)
childResizer (LvArr x) = \(LvArr a) -> LvArr (resizeAll x a)
childResizer _ = id

resizeAll :: [LvValue] -> [LvValue] -> [LvValue]
resizeAll xs@(x:_) ys = resizeCurr (childResizer x) xs ys

resizeLower :: [LvValue] -> [LvValue] -> [LvValue]
resizeLower (x:_) ys = map (childResizer x) ys

insertAt :: Int -> [LvValue] -> [LvValue] -> LvValue
insertAt i lx ly = LvArr $ take i lx ++ ly ++ drop i lx

recurseTo :: [Int] -> [LvValue] -> [LvValue] -> LvValue
recurseTo is lx ly = LvArr $ zipWith (\a b -> insertIntoArray a b is) lx ly

\end{code}

In LabVIEW, the "Insert Into Array" node has a variable number of indexing
inputs depending on the number of dimensions of the array connected to it, but
only one of these can be connected at a time. Its behavior changes depending
on which of these inputs are connected: for example, when inserting a 1D array
into a 2D array, if the first indexing input is connected, it inserts a new
row into the matrix; if the second indexing output is connected, it inserts a
new column. When inserting into a $n$-dimensional array, the value to be inserted
must be either an $n$ or $(n-1)$-dimensional array (or in the case of inserting
into a 1-D array, it must be either a 1-D array or an element of the array's
base type).

\begin{code}

insertIntoArray :: LvValue -> LvValue -> [Int] -> LvValue
insertIntoArray vx vy idxs =
   case (vx, vy, idxs) of
   (LvArr lx, _, []) -> insertIntoArray vx vy [length lx]
   (LvArr lx@(LvArr x:_),  LvArr ly,  -1 : is)  -> recurseTo  is  lx (next x lx ly)
   (LvArr lx@(LvArr x:_),  LvArr ly,  i  : _ )  -> insertAt   i   lx (curr x lx ly)
   (LvArr lx,              _,         i  : _ )  -> insertAt   i   lx (base vy)
   _ -> error ("WHAT " ++ shw vx ++ " " ++ shw vy ++ " " ++ shw idxs)
   where
      (next, curr, base) =
         if ndims vx == ndims vy
         then (  \_ lx ly     -> resizeCurr id lx ly,
                 \_ lx ly     -> resizeLower lx ly,
                 \(LvArr ly)  -> ly)
         else (  \x _  ly     -> resizeCurr id x ly,
                 \x _  ly     -> [LvArr (resizeAll x ly)],
                 \_           -> [vy])

\end{code}

The |numOp| and |boolOp| functions apply binary operations implementing
coercion rules through the auxiliary function |binOp|.

\begin{code}

binOp :: (Double -> Double -> t) -> (t -> LvValue)
         -> (Int -> Int -> t1) -> (t1 -> LvValue)
         -> [Maybe LvValue] -> LvReturn
binOp opD typD _ _  [Just (LvDBL a),  Just (LvDBL b)]  = LvReturn [typD (opD a b)]
binOp opD typD _ _  [Just (LvI32 a),  Just (LvDBL b)]  = LvReturn [typD (opD (fromIntegral a) b)]
binOp opD typD _ _  [Just (LvDBL a),  Just (LvI32 b)]  = LvReturn [typD (opD a (fromIntegral b))]
binOp _ _ opI typI  [Just (LvI32 a),  Just (LvI32 b)]  = LvReturn [typI (opI a b)]
binOp _ _ _   _     _                                  = undefined

numOp :: (Double -> Double -> Double)
         -> (Int -> Int -> Int) -> [Maybe LvValue] -> LvReturn
numOp opD opI = binOp opD LvDBL opI LvI32

boolOp :: (Double -> Double -> Bool)
          -> (Int -> Int -> Bool) -> [Maybe LvValue] -> LvReturn
boolOp opD opI = binOp opD LvBool opI LvBool

nrMandatoryInputs :: String -> Maybe Int
nrMandatoryInputs "InsertIntoArray" = Just 2
nrMandatoryInputs _ = Nothing

\end{code}

%END LYX TEXT

\end{document}

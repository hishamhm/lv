\documentclass[a4paper]{article}
%\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}
\usepackage{color}
\definecolor{darkblue}{rgb}{0,0,0.5}
\usepackage[unicode=true,pdfusetitle,
 bookmarks=true,bookmarksnumbered=false,bookmarksopen=false,
 breaklinks=true,pdfborder={0 0 0},backref=false,colorlinks=false,linkcolor=darkblue]
 {hyperref}

%BEGIN LYX PREAMBLE

%include polycode.fmt
%include Lv_format.lhs

%END LYX PREAMBLE

\begin{document}

\title{An interpreter modeling the semantics of LabVIEW}
\author{Hisham Muhammad}

\maketitle{}

\section{Introduction}

This is an interpreter designed to model the core semantics of LabVIEW,
with a focus on the interesting features of its dataflow language.

This work was written in Literate Haskell, and is a description of the entire
interpreter, including its complete listings. The text was written so that it
should be understandable without a detailed reading of the source code, but
the sources are nevertheless included for completeness and as supporting
material for the interested reader. The source code in @.lhs@ format
(\LaTeX~with embedded Haskell) is also available at
\url{https://hisham.hm/thesis/}.

%BEGIN LYX TEXT

This implementation uses only standard modules included in the Haskell Platform:

\begin{code}

module LvInterpreter where

import Data.Sequence (Seq, fromList, index, update, elemIndexL)
import qualified Data.Sequence as Seq (length, take)
import Data.Char
import Data.List
import Data.Maybe
import Data.Foldable (toList)
import Data.Generics.Aliases (orElse)
-- ** BEGIN CUT
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
-- ** END CUT

\end{code}

\section{Representation of programs}

A program in LabVIEW (called a VI, or Virtual Instrument) is a graph
connecting different kinds of objects. In LabVIEW terminology, these objects
are called \emph{controls}, which are input-only, \emph{indicators}, which are
output-only, and \emph{nodes}, which are all other operations. Throughout the
implementation, we will use this nomenclature; in particular the name ``node''
will be used only for graph elements which are not controls or indicators.
Graph elements are connected through wires. 

We represent a VI as a record containing a series of lists, enumerating
controls, indicators, nodes and wires. Controls, indicators and nodes are
paired with their names for display purposes only. The list of wires
constitutes an adjacency list for the graph connections.

\begin{code}

data LvVI =  LvVI {
               vControls    :: [(String, LvControl)],
               vIndicators  :: [(String, LvIndicator)],
               vNodes       :: [(String, LvNode)],
               vWires       :: [LvWire]
             }
   deriving Show

\end{code}

A control in LabVIEW is an input widget in the VI's front panel, which also
gets a representation as an object in the block diagram. However, since
LabVIEW includes structured graphs composed of subgraphs representing
structures such as for- and while-loops, we build these graphs in the
interpreter recursively, declaring subgraphs as |LvVI| objects. For this
reason, we use controls and indicators not only to represent GUI objects of
the front panel, but also inputs and outputs of subgraphs. For this reason, we
declare a number of types of controls: a plain control which corresponds to a
GUI object; an "auto" control that represents an automatically-generated input
value, such as the increment count in a for-loop; a "tunnel" control, which is
an input that connects data from the enclosing graph to the subgraph; and a
"shift-register" control, which is the input terminator for shift registers (a
construct to send data across iterations of a loop).

\begin{code}

data LvControl  =  LvControl LvValue
                |  LvAutoControl
                |  LvTunControl
                |  LvSRControl LvValue
   deriving Show

\end{code}

An indicator in LabVIEW is an output widget in the VI's front panel. Like
controls, indicators are represented both in the front panel (as a GUI widget)
and in the block diagram (as a connectable object). For the same reasons as
explained above for controls, we have different kinds of indicators: the plain
indicator, which represents GUI indicators proper; the "shift-register"
indicator, which sends data to its respective shift-register control (represented
by the numeric index of the control in its constructor) for the next execution
of a loop; and "tunnel" indicators, which send data out of the subgraph back
to the enclosing graph.

Tunnel indicators can be of different types: "last value", which sends out the
value produced by the last iteration of the subgraph; "auto-indexing", which
produces an array accumulating all elements received by the tunnel across all
iterations of the subgraph; and "concatenating", which concatenates all
elements received. Here, we implement the "last value" and "auto-indexing"
modes, since the "concatenating" mode is a mere convenience, that could be
achieved by concatenating the elements of the array returned in the
"auto-indexing" mode.

The LabVIEW interface enables auto-indexing by default when sending data
out of for-loops, but this can be overridden by the user in the UI.

\begin{code}

data LvIndicator  =  LvIndicator LvValue
                  |  LvSRIndicator Int
                  |  LvTunIndicator LvTunnelMode
   deriving Show

data LvTunnelMode  =  LvAutoIndexing
                   |  LvLastValue
   deriving Show

\end{code}

There are several kinds of nodes in LabVIEW. The vast majority are functions,
which implement various operations. Depending on the function, identified here
by their name, they can zero or more input ports, and zero or more output ports.

A constant is a node that holds a value. It has a single output port and
immediately fires its value.

There are various kinds of control structures. Most of them contain a single
subgraph, and due to their shared implementation, we grouped them in the 
|LvStructure| type constructor: those are while-loops, for-loops, sequences,
and sub-VIs. The case-structure controls a list of sub-VIs, and for this
reason is handled separately with the |LvCase| constructor.

A feedback node holds the value it receives through its input port and fires
it the next time the program is executed.

\begin{code}

data LvNode  =  LvFunction String
             |  LvConstant LvValue
             |  LvStructure LvStrucType LvVI
             |  LvCase [LvVI]
             |  LvFeedbackNode LvValue
   deriving Show

data LvStrucType  = LvWhile
                  | LvFor
                  | LvSequence
                  | LvSubVI
   deriving Show

\end{code}

LabVIEW supports a large number of primitive data types. Here, we implement
four primitive data types (floating-point and integer numbers, string and
boolean), the \emph{cluster} type, which implements a heterogeneous tuple of
values (working like a record or "struct"), and homogeneous arrays.

We chose to implement only one floating-point and one integer type. Besides
the types listed here, LabVIEW includes the following numeric types in total:
extended and single-precision floating-point numbers; fixed-point numbers;
signed and unsigned integers of 8, 16, 32 and 64 bits; single, double and
extended-precision complex numbers.

Unlike LabVIEW, our implementation allows arbitrarily recursive types (e.g. we
support a cluster of arrays of arrays of clusters). Since we assume that
programs entered in the output are properly type-checked, implementing the
same restrictions that LabVIEW enforces to aggregate data types could be
easily done in the type-checking step.

\begin{code}

data LvValue  =  LvDBL Double
              |  LvI32 Int
              |  LvSTR String
              |  LvBool Bool
              |  LvCluster [LvValue]
              |  LvArr [LvValue]
   deriving (Show, Eq, Ord)

\end{code}

A wire is a connection between two objects, represented as a
source-destination pair of port addresses. Each port address, denoted
|LvPortAddr t e p|, is a triple containing the element type (control, indicator
or node), the element index and the port index within the element. For the
source tuple, the port index denotes the element's output port; for the
destination tuple, it denotes the input port. 

\begin{code}

data LvWire =  LvWire {
                  wSrc :: LvPortAddr,
                  wDst :: LvPortAddr
               }
   deriving Show

data LvPortAddr = LvPortAddr LvElemType Int Int
   deriving Eq

instance Show LvPortAddr where
   show (LvPortAddr typ eidx pidx) =
      "{" ++ show typ ++ " " ++ show eidx ++ ", " ++ show pidx ++ "}"

data LvElemType  =  LvC
                 |  LvI
                 |  LvN
   deriving (Show, Eq)

\end{code}

\section{Representation of state}

The representation of a state in our interpreter is a structure containing the
following values: the timestamp, a scheduler queue listing the next elements
that need to be processed, and three sequences that store the internal states
of nodes, controls and indicators. For controls and indicators, the sequences
store their values. A VI always initializes controls and indicators with
default values.

\begin{code}

-- ** {-"\hypertarget{LvState}{}"-}
data LvState =  LvState { 
                   sTs :: Int,
                   sSched :: [LvElemAddr],
                   sNodeStates :: Seq LvNodeState,
                   sControlValues :: Seq LvValue,
                   sIndicatorValues :: Seq LvValue
                }
   deriving Show

data LvElemAddr = LvElemAddr LvElemType Int
   deriving Eq

instance Show LvElemAddr where
   show (LvElemAddr typ eidx) =
      "{" ++ show typ ++ " " ++ show eidx ++ "}"

\end{code}

For node states, the interpreter stores the contents of the input ports and an
optional continuation. Each input port may be either empty or contain a single
value, in accordance with the static dataflow model.

\begin{code}

data LvNodeState =  LvNodeState {
                       nsInputs :: Seq (Maybe LvValue),
                       nsCont :: Maybe LvCont
                    }
   deriving Show

\end{code}

For functions, we use continuations to model computations that run over time.
An operation that needs to continue running beyond the current timestamp
implements the rest of the computation as a separate function, that will be
scheduled to run at the next time tick. In the |LvKFunction| constructor we
store the continuation function itself (|kFn|) and the values that will be
passed to it (|kArgs|). These values act as the operation's internal memory.
A continuation function returns either |LvReturn|, which contains the result
values to be sent through the function's output ports, or |LvContinue|, which
encapsulates the next continuation to be executed as the operation resumes
running.

For subgraph structures, such as loops, the continuation of its execution is
the state of the sub-VI. Note that, this way, the interpreter models a
hierarchical tree of scheduler queues, as each structure node keeps an
|LvState| with its own |sSched| queue. This way, multiple subgraphs can run
concurrently.

\begin{code}

data LvCont  =  LvKFunction {
                   kFn :: LvVisibleState -> [LvValue] -> LvReturn,
                   kArgs :: [LvValue]
                }
             |  LvKState LvState

instance Show LvCont where
   show (LvKFunction _ args)  = "KFunction(" ++ show args ++ ")"
   show (LvKState state)      = "KState[" ++ show state ++ "]"

data LvReturn  =  LvReturn [LvValue]
               |  LvContinue LvCont

\end{code}

In all functions implementing LabVIEW nodes, we add an additional argument
representing read access to the external world, which we call "visible state".
In our model, the visible state consists only of the execution timestamp,
which we will use below as a model of a "system clock" for timer-based
functions.

This is a simplified model since it implements a read-only view of the
external world, but it allows us to model impure functions whose effects
depend not only on the inputs received through wires in the dataflow graph. In
particular, this allows us to model the relationship between graph evaluation
and time.

\begin{code}
 
data LvVisibleState  =  LvVisibleState {
                           vsTs :: Int
                        }

\end{code}

\section{Execution}

The execution mode of LabVIEW is data-driven. The user enters data via
controls, which propagate their values through other nodes, eventually
reaching indicators, which provide feedback to the user via their
representations in the front panel.

This interpreter models a single-shot execution (as discussed in Section
\ref{sub:LabVIEW-execution-modes}). Continuous execution is semantically
equivalent as enclosing the entire VI in a while-loop.

\subsection{Main loop}

The execution of the interpreter is a loop of evaluation steps, which starts
from an initial state defined for the VI and runs producing new states until a
final state with an empty scheduler queue is produced.

\begin{code}

runVI :: LvVI -> IO ()
runVI vi =
   loop (initialState 0 vi)
   where
      loop state = do
         print state
         case sSched state of
          []  -> return ()
          _   -> loop (run state vi)

\end{code}

\subsection{Initial state}
\label{initialstate}

The initial state consists of the input values entered for controls,
the initial values of indicators, and empty states for each node, containing
the appropriate number of empty slots corresponding to their input ports.
It also contains the initial schedule, which is the initial list of graph
elements to be executed.

\begin{code}

initialState :: Int -> LvVI -> LvState
initialState ts vi = 
   LvState {
      sTs               = ts + 1,
      sControlValues    = fromList $ map (makeControlValue . snd)    (vControls vi),
      sIndicatorValues  = fromList $ map (makeIndicatorValue . snd)  (vIndicators vi),
      sNodeStates       = fromList $ mapIdx makeNodeState            (vNodes vi),
      sSched            = initialSchedule vi
   }
   where
      makeNodeState :: (Int, (String, LvNode)) -> LvNodeState
      makeNodeState (i, (name, node)) =  LvNodeState {
                                            nsInputs  = emptyInputs $ nrInputs i node,
                                            nsCont    = Nothing 
                                         }
      nrInputs :: Int -> LvNode -> Int
      nrInputs i (LvFunction _)         = nrConnectedInputs i vi
      nrInputs _ (LvConstant _)         = 0
      nrInputs _ (LvStructure _ subVi)  = length $ vControls subVi
      nrInputs _ (LvCase subVis)        = length $ vControls (head subVis)
      nrInputs _ (LvFeedbackNode _)     = 1

      makeControlValue :: LvControl -> LvValue
      makeControlValue (LvControl    v)  = v
      makeControlValue (LvSRControl  v)  = v
      makeControlValue _                 = LvI32 0
      makeIndicatorValue :: LvIndicator -> LvValue
      makeIndicatorValue (LvIndicator v)                  = v
      makeIndicatorValue (LvTunIndicator LvAutoIndexing)  = LvArr []
      makeIndicatorValue _                                = LvI32 0
      
      mapIdx :: ((Int, a) -> b) -> [a] -> [b]
      mapIdx fn l = zipWith (curry fn) (indices l) l

emptyInputs :: Int -> Seq (Maybe LvValue)
emptyInputs n = fromList (replicate n Nothing)

\end{code}

The initial schedule is defined as follows. All controls, constants and
feedback nodes are queued. Then, all function and structure nodes which do not
depend on other inputs are queued as well. Here, we make a simplifcation and
assume that VIs do not have any functions with mandatory inputs missing. This
could be verified in a type-checking step prior to execution.

Note also that the code below implies the initial schedule follows the order
of node given in the description of the |LvVI| record, leading to a
deterministic execution of our intpreter. LabVIEW does not specify a
particular order.

\begin{code}

initialSchedule :: LvVI -> [LvElemAddr]
initialSchedule vi = 
   map (LvElemAddr LvC) (indices $ vControls vi)
   ++ map (LvElemAddr LvN) (filter  (\i -> isBootNode i (vNodes vi !! i))
                                    (indices $ vNodes vi))
   where
      isBootNode _ (_, LvConstant _) = True
      isBootNode _ (_, LvFeedbackNode _) = True
      isBootNode i (_, LvFunction _)              | nrConnectedInputs i vi == 0 = True
      isBootNode i (_, LvStructure LvWhile _)     | nrConnectedInputs i vi == 0 = True
      isBootNode i (_, LvStructure LvSubVI _)     | nrConnectedInputs i vi == 0 = True
      isBootNode i (_, LvStructure LvSequence _)  | nrConnectedInputs i vi == 0 = True
      isBootNode _ _ = False

\end{code}

A node con only be fired when all its connected inputs have incoming data. We
specifically check for connected inputs because some LabVIEW nodes have
optional inputs. We assume here for simplicity that the type-checking step
prior to execution verified that the correct set of mandatory inputs has been
connected. Here, we derive the number of connections of a node from the list
of wires.

\begin{code}

nrConnectedInputs :: Int -> LvVI -> Int
nrConnectedInputs idx vi =
   1 + foldl' maxInput (-1) (vWires vi)
   where
      maxInput :: Int -> LvWire -> Int
      maxInput mx (LvWire _ (LvPortAddr LvN i n)) | i == idx = max mx n
      maxInput mx _ = mx

\end{code}

\subsection{Event processing}
\label{run}

The main operation of the interpreter consists of taking one entry off the
scheduler queue, incrementing the timestamp, and triggering the event corresponding
to that entry. Every time we produce a new state, we increment the timestamp.
The timestamp, therefore, is not a count of the number of evaluation steps, but
is a simulation of a system clock, to be used by timer operations.

\begin{code}

run :: LvState -> LvVI -> LvState

run state@(LvState _ [] _ _ _) _ = state

run state@(LvState ts (q:qs) _ _ _) vi =
   let  state0 = state { sTs = ts + 1, sSched = qs }
   in   runEvent q state0 vi

\end{code}

An event in the queue indicates the graph element to be executed next.
Function |runEvent| takes a |LvElemAddr| that identifies the element, a state
and a VI, and produces a new state, with the results of triggering that
element:

\begin{code}

runEvent :: LvElemAddr -> LvState -> LvVI -> LvState

\end{code}

When triggering a control, its effect is to fire its value through its output port.

\begin{code}

runEvent (LvElemAddr LvC idx) state0 vi =
   fire vi cv (LvPortAddr LvC idx 0) state0
   where
      cv = index (sControlValues state0) idx

\end{code}

When triggering a node for execution, the event may be triggering either an
initial execution from data fired through its input ports, or a continuation
of a previous execution that has not finished running. In the former case, the
interpreter fetches the data from the node's input ports and clears it from
the node state, ensuring incoming values are consumed only once. In the latter
case, the inputs come from the data previously stored in the continuation
object and the node state is kept as is. Once the inputs and state are
determined, |runEvent| calls |runNode|, which produces a new state and may
produce data to be fired through the node's output ports.

\begin{code}

runEvent (LvElemAddr LvN idx) state0 vi =
   trc ("runEvent (LVN, " ++ shw idx ++ ") ON STATE " ++ shw state0 ++ " FOR VI " ++ shw vi) $
   foldl' (\s (p, v) -> fire vi v (LvPortAddr LvN idx p) s) state2 pvs
   where
      nstate = index (sNodeStates state0) idx
      (state1, inputs) =
         case nsCont nstate of
         Nothing -> startNode 
         Just k  -> continueNode k
      (state2, pvs) = runNode (snd $ vNodes vi !! idx) state1 inputs idx
         
      startNode = (state1, inputs)
         where
            state1 = updateNode idx state0 clearState []
            inputs = toList (nsInputs nstate)
            clearState = nstate { nsInputs = clear }
            clear = emptyInputs (Seq.length (nsInputs nstate))
            
      continueNode k = (state1, inputs)
         where
            state1 = state0
            inputs = case k of
                     LvKFunction _ kargs  -> map Just kargs
                     LvKState _           -> undefined

\end{code}

When updating the internal state of a node, we use the auxiliary function
|updateNode|, which increments the timestamp, optionally appends events to the
scheduler queue, and replaces the node state for the node at the given index.

\begin{code}

updateNode :: Int -> LvState -> LvNodeState -> [LvElemAddr] -> LvState
updateNode idx st newNstate newSched =
   st {
      sTs = sTs st + 1,
      sSched = sSched st ++ newSched,
      sNodeStates = update idx newNstate (sNodeStates st)
   }

\end{code}

\subsection{Firing data to objects}
\label{firing}

As shown in the previous section, when objects are triggered for execution, they may produce new values
which are fired through their output ports. The function |fire| iterates through the adjacency list of
wires, identifying all outward connections of an object and propagating the value to their destination
nodes.

\begin{code}

fire :: LvVI -> LvValue -> LvPortAddr -> LvState -> LvState
fire vi value addr state =
   trc ("firing " ++ shw addr ++ " with value " ++ shw value ++ "\tvi <" ++ shw (take 30 (show vi)) ++ ">\tstate " ++ shw state) $
   foldl' checkWire state (vWires vi)
      where
      checkWire s (LvWire src dst) =
         if addr == src
         then propagate value vi dst s
         else s

\end{code}

When a value is propagated to an indicator, its value is stored in the
state, with the appropriate handling for different kinds of tunnel
indicators.

\begin{code}

propagate :: LvValue -> LvVI -> LvPortAddr -> LvState -> LvState
propagate value vi (LvPortAddr LvI dnode _) state =
   let
      (_, indicator) = vIndicators vi !! dnode
      arr = index (sIndicatorValues state) dnode
      newValue =
         case indicator of
         LvIndicator _                  -> value
         LvSRIndicator _                -> value
         LvTunIndicator LvLastValue     -> value
         LvTunIndicator LvAutoIndexing  -> insertIntoArray arr value []
   in
      state {
         sTs = sTs state + 1,
         sIndicatorValues = update dnode newValue (sIndicatorValues state)
      }

\end{code}

When a value is propagated to a node, the interpreter stores the value in the
|nsInputs| sequence of the node state. Then, it needs to decide whether the
node needs to be scheduled for execution.

\begin{code}

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
            entry = LvElemAddr LvN dnode
         in
            if shouldSchedule (snd $ vNodes vi !! dnode) inputs' && entry `notElem` sched
            then sched ++ [entry]
            else sched

\end{code}

To determine if a node needs to be scheduled, the interpreter checks if all
its required inputs contain values. For function nodes, this means that
all mandatory arguments must have incoming values. For structures, it means
that all tunnels going into the structure must have values available for
consumption.

Feedback nodes are never triggered by another node: when they receive
a value through its input port, this value remains stored for the next
single-shot execution of the whole graph. Constants do not have input ports,
so they cannot receive values.

\begin{code}

shouldSchedule :: LvNode -> Seq (Maybe LvValue) -> Bool
shouldSchedule node inputs =
   case node of
      LvFunction name   -> shouldScheduleNode name
      LvStructure _ vi  -> shouldScheduleSubVI vi inputs
      LvCase vis        -> shouldScheduleSubVI (head vis) inputs
      LvFeedbackNode _  -> False
      LvConstant _      -> undefined
   where
      shouldScheduleNode name =
         isNothing $ elemIndexL Nothing mandatoryInputs
         where
            mandatoryInputs =
               case nrMandatoryInputs name of
               Nothing -> inputs
               Just n  -> Seq.take n inputs
      shouldScheduleSubVI :: LvVI -> Seq (Maybe LvValue) -> Bool
      shouldScheduleSubVI vi inputs = 
         isNothing $ find unfilledTunnel (indices $ vControls vi)
            where
               unfilledTunnel cidx = 
                  case vControls vi !! cidx of
                     (_, LvTunControl) -> trc ("UNFILLED CTRL!") $ isNothing (index inputs cidx)
                     _ -> False

indices :: [a] -> [Int]
indices l = [0 .. (length l - 1)]

\end{code}

\section{Nodes and structures}

The function |runNode| takes care of implementing the general logic for each
kind of node. For functions, it handles the management of continuations; for
structures, it triggers their subgraphs according to each structure's rules
of iteration and conditions of termination.

The function |runNode| takes a node, an input state, a list of input values,
the integer index that identifies the node in the VI, and produces a 
new state and a list of index-value pairs, listing values to be sent through
output ports.

\begin{code}

runNode ::  LvNode -> LvState -> [Maybe LvValue] -> Int
            -> (LvState, [(Int, LvValue)])

\end{code}

\subsection{Constant nodes}

When executed, a constant node simply sends out its value through its
single output port.

\begin{code}

runNode (LvConstant value) state1 _ _ =
   trc ("firing constant " ++ shw value) $
   (state1, [(0, value)])

\end{code}

\subsection{Feedback nodes}

A feedback node behaves like a constant node: it sends out the value it stores
through its output port. In spite of having an input port, a feedback node is
only triggered at the beginning of the execution of the graph, as determined
by the initial state (Section \ref{initialstate}) and firing rules (Section
\ref{firing}).

In our model, an |LvFeedbackNode| always takes an initialization value. In the
LabVIEW UI, this value can be left out, in which case a default value for the
appropriate data type, such as zero or an empty string, is implied.

\begin{code}

runNode (LvFeedbackNode initVal) state1 inputs _ =
   (state1, [(0, fromMaybe initVal (head inputs) )])

\end{code}

\subsection{Function nodes}

When running a function node, the interpreter first checks if it has an
existing continuation pending for the node. If there is one, it resumes the
continuation, applying the function stored in the continuation object |k|.
Otherwise, it triggers the function (identified by its name) using
|applyFunction|.

The function may return either a |LvReturn| value, which contains the list of
result values be propagated through its output ports, or a |LvContinue| value,
which contains the next continuation |k'| to be executed. When a continuation
is returned, the node itself (identified by its address |idx|) is also
scheduled back in the queue, and no values are produced to be sent to the
node's output ports.

\begin{code}

runNode (LvFunction name) state1 inputs idx =
   let
      nstates = sNodeStates state1
      nstate = index nstates idx
      visible state = LvVisibleState (sTs state)
      ret =
         case nsCont nstate of
         Nothing -> applyFunction name (visible state1) inputs
         Just k  -> kFn k              (visible state1) (catMaybes inputs)
   in
      trc ("firing function " ++ name) $
      case ret of
      LvReturn outVals ->
         (state2, pvs)
         where
            state2 = updateNode idx state1 nstate{ nsCont = Nothing } []
            pvs = zip (indices outVals) outVals
      LvContinue k' ->
         (updateNode idx state1 nstate{ nsCont = Just k' } [LvElemAddr LvN idx], [])

\end{code}

\subsection{Structures}

The interpreter supports five kinds of structures: for-loop, while-loop,
sequence, case and sub-VI. They are all implemented similarly, by running a
subgraph (itself represented as an instance of |LvVI|, like the main graph),
and storing a state object for this subgraph as a continuation object of the
node state for the enclosing graph (represented as |LvState|, like the main
state). Running this subgraph may take several evaluation steps, so the
enclosing graph will continuously queue it for execution until it decides it
should finish running. Each time the scheduler of the enclosing graph triggers
the structure node, it will run the subgraph consuming one event of the
internal state's own scheduler queue. This will, in effect, produce a
round-robin of all structures that may be running concurrently.

This common behavior is implemented in the |runStructure| function that
will be presented below. The implementations of |runNode| for all
structures use |runStructure|, differing by the way they control
triggering and termination of subgraphs.

% TODO for when no N is set and an array is given as input tunnel
% TODO check what happens when both are given

The for-loop provides |runStructure| with a termination function |shouldStop|
which determines if the loop should stop comparing the value of the counter
control (at index 0) with the limit control (at index 1). Also, it uses the
helper function |initCounter| to force the initial value of control 0 when the
structure is triggered for the first time (that is, when it is not resuming a
continuation).

\begin{code}

runNode (LvStructure LvFor subVi) state1 inputs idx =
   trc ("firing for") $
   runStructure subVi shouldStop state1 idx (initCounter state1 idx inputs)
   where
      shouldStop st =
         trc (shw (i + 1) ++ " >= " ++ shw n) $
         (i + 1 >= n)
         where
            (LvI32 i) = index (sControlValues st) 0
            LvI32 n = coerceToInt $ index (sControlValues st) 1
      coerceToInt v@(LvI32 _) = v
      coerceToInt (LvDBL d) = LvI32 (floor d)

\end{code}

The while-loop structure in LabVIEW always provides an iteration counter,
implemented in the interpreter as a counter control at index 0.
As in the for-loop, it is initialized using helper function |initCounter|.
The termination function for the while-loop checks for the boolean
value at the indicator at index 0.

\begin{code}

runNode (LvStructure LvWhile subVi) state1 inputs idx =
   trc ("firing while") $
   runStructure subVi shouldStop state1 idx (initCounter state1 idx inputs)
   where
      shouldStop st =
         not test
         where
            (LvBool test) = index (sIndicatorValues st) 0

\end{code}

% TODO move this explanation to the main text
% TODO add figure
Sequence nodes in LabVIEW are a way to enforce order of execution irrespective
of data dependencies. In the LabVIEW UI, sequences are presented as a series
of frames presented like a film-strip. In our interpreter, we implement each
frame of the film-strip as a separate |LvStructure| object containing a
boolean control at input port 0 and a boolean indicator at output port 0.
Frames of a sequence are connected through a wire connecting the frame's
indicator 0 to the next frame's control 0. This way, we force a
data dependency between frames, and the implementation of |runNode| for
sequences pushes a boolean value to output port 0 to trigger the execution of
the next frame in the sequence. This connection is explicit in our model,
but it could be easily hidden in the application's UI.

\begin{code}

runNode (LvStructure LvSequence vi) state1 inputs idx =
   let
      (state2, pvs) = runStructure vi (const True) state1 idx inputs
      nstate2 = index (sNodeStates state2) idx
      nextq = [ (0, LvBool True) | isNothing (nsCont nstate2) ]
   in
      (state2, pvs ++ nextq)

\end{code}

Case structures are different from the other ones because they contain
a list of subgraphs. All subgraphs representing cases are assumed to
have the same set of controls and indicators, and they all have a
numeric control at index 0 which determines which case is active.
LabVIEW denotes cases using enumeration types, but in the interpreter
we simply use an integer.

When a case node is triggered, |runNode| needs to choose which VI to use with
|runStructure|. In its first execution, it reads from the input data sent to
control 0; it subsequent executions, when those inputs are no longer
available, it reads directly from the control value, which it stores in the
node state. Note that since case VIs have the same set of controls and
indicators, they are structurally equivalent, and the initialization routine
in Section \ref{initialstate} simply uses the first case when constructing the
initial empty state.

A case subgraph does not iterate: it may take several schedule events to run
through a full single-shot execution, but once the subgraph scheduler queue is
empty, it should not run again. For this reason, the termination function is
simply |const True|.

\begin{code}

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
      (state2, pvs) = runStructure (vis !! n) (const True) state1 idx inputs
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

Finally, a sub-VI structure has a simple implementation, where we launch the
subgraph with |runStructure|, directing it to run once and performing no
additional operations to its state.

\begin{code}

runNode (LvStructure LvSubVI subVi) state1 inputs idx =
   trc ("firing subvi") $
   runStructure subVi (const True) state1 idx inputs

\end{code}

The core to the execution of all structure nodes is the |runStructure|
function, which we present here. This function takes as arguments
the subgraph to execute, the termination function to apply, the
enclosing graph's state, the index of the structure in the enclosing
VI, and returns a pair with the new state and a list of port-value
pairs to fire through output ports. 

\begin{code}

runStructure ::  LvVI
                 -> (LvState -> Bool)
                 -> LvState -> Int -> [Maybe LvValue]
                 -> (LvState, [(Int, LvValue)])

\end{code}

Its execution works as follows. First, it determines |statek|, which is the
state to use when running the subgraph. If there is no continuation, a new
state is constructed using |initialState| (Section \ref{initialstate}), with
the input values received as arguments entered as values for the structure's
controls. If there is a continuation, it means it is resuming execution of an
existing state, so it reuses the state stored in the |LvKState| object,
merely updating its timestamp.

Then, it calls the main function |run| (Section \ref{run}) on the subgraph
|subVi| and state |statek|. This produces a new state, |statek'|. If the
scheduler queue in this state is not empty, this means that the single-shot
execution of the graph did not finish. In this case, the interpreter stores
this new state in a continuation object |nextk| and enqueues the structure
in the main state so it runs again.

If the scheduler queue is empty, |runStructure| runs the termination check
|shouldStop| to determine if it should schedule a new iteration of the
subgraph. If a new iteration is required, a new state is produced with
|nextStep|, which increments the iterator and processes shift registers.

At last, if the execution does not produce a continuation, this means the
structure terminated its single-shot execution: the values of the indicators
are sent out to the structure's output ports.

\begin{code}

runStructure subVi shouldStop state1 idx inputs =
   let
      nstates = sNodeStates state1
      nstate = index nstates idx
      ts' = sTs state1 + 1

      statek = 
         case nsCont nstate of
         Nothing -> setControlValues inputs $ initialState ts' subVi
         Just (LvKState st) -> st { sTs = ts' }
      statek'@(LvState _ qk _ _ _) = run statek subVi
      nextk
         | not $ null qk      = trc ("GOT QK " ++ shw qk ++ " IN STATEK " ++ shw statek) $ Just (LvKState statek')
         | shouldStop statek' = Nothing
         | otherwise =
              trc ("let's go " ++ shw (i + 1)) $
              trc ("before: " ++ shw statek') $
              Just (LvKState (nextStep subVi statek' (i + 1)))
         where (LvI32 i) = index (sControlValues statek') 0
      nstate' = nstate { nsCont = nextk }
      qMe = trc ("QUEUED MYSELF? " ++ (shw $ isJust nextk) ++ "(LvN " ++ shw idx ++ ")") $ [LvElemAddr LvN idx | isJust nextk]
      state2 = state1 {
         sTs = sTs statek' + 1,
         sSched = sSched state1 ++ qMe,
         sNodeStates = update idx nstate' nstates
      }
      pvs = zip (indices $ vIndicators subVi) (toList $ sIndicatorValues statek')

      setControlValues inputs state =
         state {
            sTs = sTs state + 1,
            sControlValues = fromList $ zipWith fromMaybe (toList $ sControlValues state) inputs
         }  
   in
      (state2, if isJust nextk then [] else pvs)

\end{code}

\begin{code}

initCounter :: LvState -> Int -> [Maybe LvValue] -> [Maybe LvValue]
initCounter state idx inputs =
   case nsCont (index (sNodeStates state) idx) of
   Nothing -> Just (LvI32 0) : tail inputs
   _       -> inputs

nextStep :: LvVI -> LvState -> Int -> LvState
nextStep vi state i' =
   state {
      sTs = sTs state + 1,
      sSched = initialSchedule vi,
      sControlValues = cvs''
   }
   where
      cvs' = update 0 (LvI32 i') (sControlValues state)
      cvs'' :: Seq LvValue
      cvs'' = foldl' shiftRegister cvs'
              $ zip (vIndicators vi) (toList (sIndicatorValues state))
      shiftRegister ::  Seq LvValue -> ((String, LvIndicator), LvValue)
                        -> Seq LvValue
      shiftRegister cvs ((_, LvSRIndicator cidx), ival) = 
         update cidx ival cvs
      shiftRegister cvs _ = cvs

\end{code}

\section{Operations}

\begin{code}
applyFunction :: String -> LvVisibleState -> [Maybe LvValue] -> LvReturn
\end{code}

\subsection{Random Number}

\begin{code}
applyFunction "RandomNumber" _ [] = LvReturn [LvDBL 0.5] -- not very random :)
\end{code}

\subsection{Array Max \& Min}

\begin{code}
applyFunction "ArrayMax&Min" _ [Just (LvArr a)] =
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

\subsection{Insert Into Array}

\begin{code}

applyFunction "InsertIntoArray" _ (Just arr : Just vs : idxs) =
   LvReturn [insertIntoArray arr vs (map numIdx idxs)]
   where
      numIdx i = case i of
                 Nothing -> -1
                 Just (LvI32 n) -> n

\end{code}

\subsection{Bundle}

\begin{code}

applyFunction "Bundle" _ args = 
   LvReturn [LvCluster (catMaybes args)]

\end{code}

\subsection{Wait Until Next Ms}

\begin{code}

applyFunction "WaitUntilNextMs" (LvVisibleState ts) [Just (LvI32 ms)] =
   LvContinue $ LvKFunction waitUntil [LvI32 nextMs]
   where
      nextMs = ts - (ts `mod` ms) + ms
      waitUntil (LvVisibleState now) arg@[LvI32 stop]
         | now >= stop  = LvReturn []
         | otherwise    = LvContinue $ LvKFunction waitUntil arg

applyFunction "WaitUntilNextMs" vst [Just (LvDBL msd)] =
   applyFunction "WaitUntilNextMs" vst [Just (LvI32 (floor msd))]

\end{code}

\subsection{Numeric and relational operators}

\begin{code}

applyFunction "+" _ args = numOp   (+) (+)  args
applyFunction "-" _ args = numOp   (-) (-)  args
applyFunction "*" _ args = numOp   (*) (*)  args
applyFunction "/" _ args = numOp   (/) div  args
applyFunction "<" _ args = boolOp  (<) (<)  args
applyFunction ">" _ args = boolOp  (>) (>)  args

applyFunction fn _ args =
   error ("No rule to apply " ++ fn ++ " " ++ show args)

\end{code}

\begin{code}

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

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
import Data.Bits
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
will be used only for graph objects which are not controls or indicators.
Graph objects are connected through wires. To avoid confusion with objects
in the interpreter implementation, we will refer to graph objects (controls,
indicators and nodes) as \emph{elements}.

We represent a VI as a record containing a series of lists, enumerating
controls, indicators, nodes and wires. Controls, indicators and nodes are
paired with their names for display purposes only. The list of wires
constitutes an adjacency list for the graph connections.

\begin{code}

data LvVI =  LvVI {
               vCtrls   :: [(String, LvControl)],
               vIndics  :: [(String, LvIndicator)],
               vNodes   :: [(String, LvNode)],
               vWires   :: [LvWire]
             }
   deriving Show

\end{code}

A control in LabVIEW is an input widget in the VI's front panel, which also
gets a representation as an object in the block diagram. However, since
LabVIEW includes structured graphs composed of subgraphs representing
structures such as for- and while-loops, we build these graphs in the
interpreter recursively, declaring subgraphs as |LvVI| objects. For this
reason, we use controls and indicators not only to represent GUI objects of
the front panel, but also inputs and outputs of subgraphs. To do this, we
declare a number of types of controls: a plain control that corresponds to a
GUI object; an ``auto'' control that represents an automatically-generated input
value, such as the increment count in a for-loop; a ``tunnel'' control, which is
an input that connects data from the enclosing graph to the subgraph; and a
``shift-register'' control, which is the input terminator for shift registers (a
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
indicator, which represents a GUI indicator proper; the ``shift-register''
indicator, which sends data to its respective shift-register control (represented
by the numeric index of the control in its constructor) for the next execution
of a loop; and the ``tunnel'' indicator, which sends data out of the subgraph back
to the enclosing graph.

Tunnel indicators can be of different types: ``last value'', which sends out the
value produced by the last iteration of the subgraph; ``auto-indexing'', which
produces an array accumulating all values received by the tunnel across all
iterations of the subgraph; and ``concatenating'', which concatenates all
values received. Here, we implement the ``last value'' and ``auto-indexing''
modes, since the ``concatenating'' mode is a mere convenience that could be
achieved by concatenating the values of the array returned in the
``auto-indexing'' mode.

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
but there are also control structures, constants and feedback nodes.

Functions are identified in our implementation by their their names. They can
have zero or more input ports, and zero or more output ports.

There are various kinds of control structures. Due to the fact that many of
them share code in our implementation, we grouped them in the |LvStructure|
type constructor: those are while-loops, for-loops, sequences, and sub-VIs.
The case-structure controls a list of sub-VIs, and for this reason is handled
separately with the |LvCase| constructor.

A constant is a node that holds a value. It has a single output port and
immediately fires its value.

A feedback node holds the value it receives through its input port and fires
it the next time the program is executed, when running in continuous mode
as explained in Section~\ref{subsec:LabVIEW-execution-modes}.

\begin{code}

data LvNode  =  LvFunction String
             |  LvStructure LvStrucType LvVI
             |  LvCase [LvVI]
             |  LvConstant LvValue
             |  LvFeedbackNode LvValue
   deriving Show

data LvStrucType  = LvWhile
                  | LvFor
                  | LvSequence
                  | LvSubVI
   deriving Show

\end{code}

LabVIEW supports a large number of primitive numeric types: single, double and
extended-precision floating-point numbers; fixed-point numbers; signed and
unsigned integers of 8, 16, 32 and 64 bits; single, double and
extended-precision complex numbers. We chose to implement only one
floating-point and one integer type.

Besides these, the interpreter also supports the following types: strings;
booleans; the \emph{clusters}, which are a heterogeneous tuple of values
(working like a record or ``struct''); and homogeneous arrays.

Unlike LabVIEW, our implementation allows arbitrarily recursive types (e.g. we
support a cluster of arrays of arrays of clusters). 

Though LabVIEW supports arrays of clusters, and clusters of arrays, it does
not support arrays of arrays. The recommended alternative is to use an ``array
of cluster of array'': an array where elements are single-element clusters
containing an array. This limitation is an explicit design decision, harking
back to the development of LabVIEW 2.0 in
1988\footnote{\url{https://forums.ni.com/t5/LabVIEW-Idea-Exchange/Add-Support-for-Array-of-Array/idi-p/1875123}}.

Since we assume that input programs are properly type-checked, implementing
the same restrictions that LabVIEW enforces to aggregate data types could be
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

The representation of a state in our interpreter is a record containing the
following values: the timestamp, a scheduler queue listing the next elements
that need to be processed, and three sequences that store the internal states
of nodes, controls and indicators. For controls and indicators, the sequences
store their values. A VI always initializes controls and indicators with
default values. Elements in the scheduler queue are denoted as |LvElemAddr t
e|, where |t| is the type of the element (control, indicator or node) and |e|
is the numeric index of the element in its appropriate list in the |LvVI|
object.

\begin{code}

-- ** {-"\hypertarget{LvState}{}"-}
data LvState =  LvState { 
                   sTs         :: Int,
                   sPrng       :: Int,
                   sSched      :: [LvElemAddr],
                   sNStates    :: Seq LvNodeState,
                   sCtrlVals   :: Seq LvValue,
                   sIndicVals  :: Seq LvValue
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
                       nsInputs  :: Seq (Maybe LvValue),
                       nsCont    :: Maybe LvCont
                    }
   deriving Show

\end{code}

For functions, we use continuations to model computations that run over time.
An operation that needs to continue running beyond the current timestamp
implements the rest of the computation as a separate function, which will be
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
                   kFn    :: LvWorld -> [LvValue] -> (LvWorld, LvReturn),
                   kArgs  :: [LvValue]
                }
             |  LvKState LvState

instance Show LvCont where
   show (LvKFunction _ args)  = "KFunction(" ++ show args ++ ")"
   show (LvKState s)          = "KState[" ++ show s ++ "]"

data LvReturn  =  LvReturn [LvValue]
               |  LvContinue LvCont

\end{code}

In all functions implementing LabVIEW nodes, we include an additional argument
and an additional result representing access to side-effects that affect the
state of the external world.

These extra values allow us to model impure functions whose effects depend not
only on the inputs received through wires in the dataflow graph. In
particular, this allows us to model the relationship between graph evaluation
and time.

In our model, a simplified view of this ``external world'' is implemented as
the |LvWorld| type. It consists of a read-only timestamp, which we will use as
a model of a ``system clock'' for timer-based functions, and the read-write
pseudo-random number generator (PRNG) state, which can be consumed and
updated.

\begin{code}
 
data LvWorld  =  LvWorld {
                    wTs :: Int,
                    wPrng :: Int
                 }

\end{code}

Note that |LvWorld| is a subset of our |LvState| object, which represents the
memory of the VI being executed. In this sense, this is the part of the
outside world that is visible to the function.

\section{Execution}

The execution mode of LabVIEW is data-driven. The user enters data via
controls, which propagate their values through other nodes, eventually
reaching indicators, which provide feedback to the user via their
representations in the front panel.

This interpreter models a single-shot execution (as discussed in
Section~\ref{subsec:LabVIEW-execution-modes}). Continuous execution is
semantically equivalent as enclosing the entire VI in a while-loop.

\subsection{Main loop}

The execution of the interpreter is a loop of evaluation steps, which starts
from an initial state defined for the VI and runs producing new states until a
final state with an empty scheduler queue is produced.

\begin{code}

runVI :: LvVI -> IO ()
runVI vi =
   loop (initialState 0 42 vi)
   where
      loop s = do
         print s
         case sSched s of
          []  -> return ()
          _   -> loop (run s vi)

\end{code}

\subsection{Initial state}
\label{initialstate}

The initial state consists of the input values entered for controls,
the initial values of indicators, and empty states for each node, containing
the appropriate number of empty slots corresponding to their input ports.
It also contains the initial schedule, which is the initial list of graph
elements to be executed.

\begin{code}

initialState :: Int -> Int -> LvVI -> LvState
initialState ts prng vi = 
   LvState {
      sTs         = ts + 1,
      sPrng       = prng,
      sCtrlVals   = fromList $ map (makeCtrlVal . snd)   (vCtrls vi),
      sIndicVals  = fromList $ map (makeIndicVal . snd)  (vIndics vi),
      sNStates    = fromList $ mapIdx makeNState         (vNodes vi),
      sSched      = initialSchedule vi
   }
   where
      makeNState :: (Int, (String, LvNode)) -> LvNodeState
      makeNState (i, (name, node)) = 
         LvNodeState {
            nsInputs  = emptyInputs $ nrInputs i node,
            nsCont    = Nothing 
         }
      nrInputs :: Int -> LvNode -> Int
      nrInputs i (LvFunction _)         = nrWiredInputs i vi
      nrInputs _ (LvConstant _)         = 0
      nrInputs _ (LvStructure _ subvi)  = length $ vCtrls subvi
      nrInputs _ (LvCase subvis)        = length $ vCtrls (head subvis)
      nrInputs _ (LvFeedbackNode _)     = 1

      makeCtrlVal :: LvControl -> LvValue
      makeCtrlVal (LvControl    v)  = v
      makeCtrlVal (LvSRControl  v)  = v
      makeCtrlVal _                 = LvI32 0

      makeIndicVal :: LvIndicator -> LvValue
      makeIndicVal (LvIndicator v)                  = v
      makeIndicVal (LvTunIndicator LvAutoIndexing)  = LvArr []
      makeIndicVal _                                = LvI32 0
      
      mapIdx :: ((Int, a) -> b) -> [a] -> [b]
      mapIdx fn l = zipWith (curry fn) (indices l) l

emptyInputs :: Int -> Seq (Maybe LvValue)
emptyInputs n = fromList (replicate n Nothing)

\end{code}

The initial schedule is defined as follows. All controls, constants and
feedback nodes are queued. Then, all function and structure nodes which do not
depend on other inputs are queued as well. Here, we make a simplification and
assume that VIs do not have any functions with mandatory inputs missing. This
could be verified in a type-checking step prior to execution.

Note also that the code below implies the initial schedule follows the order
of nodes given in the description of the |LvVI| record, leading to a
deterministic execution of our intpreter. LabVIEW does not specify a
particular order.

\begin{code}

initialSchedule :: LvVI -> [LvElemAddr]
initialSchedule vi = 
   map (LvElemAddr LvC) (indices $ vCtrls vi)
   ++ map (LvElemAddr LvN) (filter  (\i -> isBootNode i (vNodes vi !! i)) (indices $ vNodes vi))
   where
      isBootNode _ (_, LvConstant _) = True
      isBootNode _ (_, LvFeedbackNode _) = True
      isBootNode i (_, LvFunction _)              | nrWiredInputs i vi == 0 = True
      isBootNode i (_, LvStructure LvWhile _)     | nrWiredInputs i vi == 0 = True
      isBootNode i (_, LvStructure LvSubVI _)     | nrWiredInputs i vi == 0 = True
      isBootNode i (_, LvStructure LvSequence _)  | nrWiredInputs i vi == 0 = True
      isBootNode _ _ = False

\end{code}

A node can only be fired when all its connected inputs have incoming data. We
specifically check for connected inputs because some LabVIEW nodes have
optional inputs. We assume here for simplicity that the type-checking step
prior to execution verified that the correct set of mandatory inputs has been
connected. Here, we derive the number of connections of a node from the list
of wires.

\begin{code}

nrWiredInputs :: Int -> LvVI -> Int
nrWiredInputs idx vi =
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

run s vi
 | null (sSched s) = s
 | otherwise =
    case sSched s of
    (q:qs) ->  let  s0 = s { sTs = (sTs s) + 1, sSched = qs }
               in   runEvent q s0 vi

\end{code}

An event in the queue indicates the graph element to be executed next.
Function |runEvent| takes a |LvElemAddr| that identifies the element, a state
and a VI, and produces a new state, with the results of triggering that
element:

\begin{code}

runEvent :: LvElemAddr -> LvState -> LvVI -> LvState

\end{code}

When triggering a control, its effect is to fire its value through its sole output port.

\begin{code}

runEvent (LvElemAddr LvC idx) s0 vi =
   fire vi cv (LvPortAddr LvC idx 0) s0
   where
      cv = index (sCtrlVals s0) idx

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

runEvent (LvElemAddr LvN idx) s0 vi =
   trc ("runEvent (LVN, " ++ shw idx ++ ") ON STATE " ++ shw s0 ++ " FOR VI " ++ shw vi) $
   foldl' (\s (p, v) -> fire vi v (LvPortAddr LvN idx p) s) s2 pvs
   where
      ns = index (sNStates s0) idx
      (s1, inputs) =
         case nsCont ns of
         Nothing  -> startNode 
         Just k   -> continueNode k
      (s2, pvs) = runNode (snd $ vNodes vi !! idx) s1 inputs idx
         
      startNode = (s1, inputs)
         where
            s1          = updateNode idx s0 clearState []
            inputs      = toList (nsInputs ns)
            clearState  = ns { nsInputs = clear }
            clear       = emptyInputs (Seq.length (nsInputs ns))
            
      continueNode k = (s1, inputs)
         where
            s1      = s0
            inputs  = case k of
                      LvKFunction _ kargs  -> map Just kargs
                      LvKState _           -> undefined

\end{code}

When updating the internal state of a node, we use the auxiliary function
|updateNode|, which increments the timestamp, optionally appends events to the
scheduler queue, and replaces the node state for the node at the given index.

\begin{code}

updateNode :: Int -> LvState -> LvNodeState -> [LvElemAddr] -> LvState
updateNode idx s ns sched =
   s {
      sTs       = sTs s + 1,
      sSched    = sSched s ++ sched,
      sNStates  = update idx ns (sNStates s)
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
fire vi value addr s =
   trc ("firing " ++ shw addr ++ " with value " ++ shw value ++ "\tvi <" ++ shw (take 30 (show vi)) ++ ">\ts " ++ shw s) $
   foldl' checkWire s (vWires vi)
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
propagate value vi (LvPortAddr LvI dnode _) s =
   let
      (_, indicator)  = vIndics vi !! dnode
      newValue        =
         case indicator of
         LvIndicator _                  -> value
         LvSRIndicator _                -> value
         LvTunIndicator LvLastValue     -> value
         LvTunIndicator LvAutoIndexing  ->  let arr = index (sIndicVals s) dnode
                                            in insertIntoArray arr value []
   in
      s {
         sTs         = sTs s + 1,
         sIndicVals  = update dnode newValue (sIndicVals s)
      }

\end{code}

When a value is propagated to a node, the interpreter stores the value in the
|nsInputs| sequence of the node state. Then, it needs to decide whether the
node needs to be scheduled for execution.

\begin{code}

propagate value vi (LvPortAddr LvN dnode dport) s =
   s {
      sTs       = sTs s + 1,
      sSched    = sched',
      sNStates  = nss'
   }
   where
      nss      = sNStates s
      ns       = index nss dnode
      inputs'  = update dport (Just value) (nsInputs ns)
      nss'     = update dnode (ns { nsInputs = inputs' } ) nss
      sched'   =
         let
            sched  = sSched s
            entry  = LvElemAddr LvN dnode
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

This interpreter implements a single node accepting optional inputs,
@InsertIntoArray@ (Section~\ref{arrayfns}); for all other nodes, all
inputs are mandatary.

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
               Nothing  -> inputs
               Just n   -> Seq.take n inputs
      shouldScheduleSubVI :: LvVI -> Seq (Maybe LvValue) -> Bool
      shouldScheduleSubVI vi inputs = 
         isNothing $ find unfilledTunnel (indices $ vCtrls vi)
            where
               unfilledTunnel cidx = 
                  case vCtrls vi !! cidx of
                     (_, LvTunControl)  -> isNothing (index inputs cidx)
                     _                  -> False

nrMandatoryInputs :: String -> Maybe Int
nrMandatoryInputs "InsertIntoArray" = Just 2
nrMandatoryInputs _ = Nothing

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

runNode (LvConstant value) s1 _ _ =
   trc ("firing constant " ++ shw value) $
   (s1, [(0, value)])

\end{code}

\subsection{Feedback nodes}

A feedback node behaves like a constant node: it sends out the value it stores
through its output port. In spite of having an input port, a feedback node is
only triggered at the beginning of the execution of the graph, as determined
by the initial state (Section~\ref{initialstate}) and firing rules
(Section~\ref{firing}).

In our model, an |LvFeedbackNode| always takes an initialization value. In the
LabVIEW UI, this value can be left out, in which case a default value for the
appropriate data type, such as zero or an empty string, is implied.

\begin{code}

runNode (LvFeedbackNode initVal) s1 inputs _ =
   (s1, [(0, fromMaybe initVal (head inputs) )])

\end{code}

\subsection{Function nodes}
\label{functionnodes}

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

runNode (LvFunction name) s1 inputs idx =
   let
      nss      = sNStates s1
      ns       = index nss idx
      world s  = LvWorld { wTs = sTs s, wPrng = sPrng s }
      ret      =
         case nsCont ns of
         Nothing  -> applyFunction name (world s1) inputs
         Just k   -> kFn k              (world s1) (catMaybes inputs)
      (w, mk, q, pvs) =
         case ret of
         (w, LvReturn outVals)  -> ( w, Nothing,  [], zip (indices outVals) outVals )
         (w, LvContinue k')     -> ( w, Just k',  [LvElemAddr LvN idx], [] )
      updateWorld w s = s { sPrng = wPrng w }
   in
      (updateWorld w $ updateNode idx s1 ns { nsCont = mk } q, pvs)

\end{code}


\subsection{Control structures}

The interpreter supports five kinds of control structures: for-loop,
while-loop, sequence, case and sub-VI. They are all implemented similarly, by
running a subgraph (itself represented as an instance of |LvVI|, like the main
graph), and storing a state object for this subgraph as a continuation object
of the node state for the enclosing graph (represented as |LvState|, like the
main state). Running this subgraph may take several evaluation steps, so the
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

runNode (LvStructure LvFor subvi) s1 inputs idx =
   trc ("firing for") $
   runStructure subvi shouldStop s1 idx (initCounter s1 idx inputs)
   where
      shouldStop s =
         trc (shw (i + 1) ++ " >= " ++ shw n) $
         (i + 1 >= n)
         where
            LvI32 i  = index (sCtrlVals s) 0
            LvI32 n  = coerceToInt $ index (sCtrlVals s) 1
      coerceToInt v@(LvI32 _)  = v
      coerceToInt (LvDBL d)    = LvI32 (floor d)

\end{code}

The while-loop structure in LabVIEW always provides an iteration counter,
implemented in the interpreter as a counter control at index 0. As in the
for-loop, it is initialized using the helper function |initCounter|. The
termination function for the while-loop checks for the boolean value at the
indicator at index 0.

\begin{code}

runNode (LvStructure LvWhile subvi) s1 inputs idx =
   trc ("firing while") $
   runStructure subvi shouldStop s1 idx (initCounter s1 idx inputs)
   where
      shouldStop s =
         not test
         where
            LvBool test = index (sIndicVals s) 0

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

runNode (LvStructure LvSequence subvi) s1 inputs idx =
   let
      (s2, pvs)  = runStructure subvi (const True) s1 idx inputs
      ns2        = index (sNStates s2) idx
      nextq          = [ (0, LvBool True) | isNothing (nsCont ns2) ]
   in
      (s2, pvs ++ nextq)

\end{code}

Case structures are different from the other ones because they contain
a list of subgraphs. All subgraphs representing cases are assumed to
have the same set of controls and indicators, and they all have a
numeric control at index 0 which determines which case is active.
LabVIEW denotes cases using enumeration types, but in the interpreter
we simply use an integer.

When a case node is triggered, |runNode| needs to choose which VI to use with
|runStructure|. In its first execution, it reads from the input data sent to
control 0; in subsequent executions, when those inputs are no longer
available, it reads directly from the control value, which is stored in the
node state. Note that since case VIs have the same set of controls and
indicators, they are structurally equivalent, and the initialization routine
in Section~\ref{initialstate} simply uses the first case when constructing the
initial empty state.

A case subgraph does not iterate: it may take several schedule events to run
through a full single-shot execution, but once the subgraph scheduler queue is
empty, it should not run again. For this reason, the termination function is
simply |const True|.

\begin{code}

runNode (LvCase subvis) s1 inputs idx =
   let
      ns1 = index (sNStates s1) idx
      n = case nsCont ns1 of
             Nothing ->  case inputs of
                         Just (LvI32 i) : _  -> i
                         _                   -> 0
             Just _ ->   (\(LvI32 i) -> i) $
                         fromMaybe (error "no input 0") $ index (nsInputs ns1) 0
      (s2, pvs) = runStructure (subvis !! n) (const True) s1 idx inputs
      s3 =
         case nsCont ns1 of
            Nothing ->  let
                           ns2 = index (sNStates s2) idx
                           inputs = update 0 (Just (LvI32 n)) (nsInputs ns2)
                           ns3 = ns2 { nsInputs = inputs }
                        in
                           updateNode idx s2 ns3 []
            Just _ ->   s2
   in
      (s3, pvs)

\end{code}

Finally, a sub-VI structure has a simple implementation, where we launch the
subgraph with |runStructure|, directing it to run once and performing no
additional operations to its state.

\begin{code}

runNode (LvStructure LvSubVI subvi) s1 inputs idx =
   trc ("firing subvi") $
   runStructure subvi (const True) s1 idx inputs

\end{code}

The core to the execution of all structure nodes is the |runStructure|
function, which we present here. This function takes as arguments
the subgraph to execute, the termination function to apply, the
enclosing graph's state, and the index of the structure in the enclosing
VI; it returns a pair with the new state and a list of port-value
pairs to fire through output ports. 

\begin{code}

runStructure ::  LvVI
                 -> (LvState -> Bool)
                 -> LvState -> Int -> [Maybe LvValue]
                 -> (LvState, [(Int, LvValue)])

\end{code}

Its execution works as follows. First, it determines |sk|, which is the
state to use when running the subgraph. If there is no continuation, a new
state is constructed using |initialState| (Section~\ref{initialstate}), with
the input values received as arguments entered as values for the structure's
controls. If there is a continuation, it means it is resuming execution of an
existing state, so it reuses the state stored in the |LvKState| object,
merely updating its timestamp.

Then, it calls the main function |run| (Section~\ref{run}) on the subgraph
|subvi| and state |sk|. This produces a new state, |sk'|. If the
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

runStructure subvi shouldStop s1 idx inputs =
   let
      nss   = sNStates s1
      ns    = index nss idx
      ts'   = sTs s1 + 1
      prng  = sPrng s1

      sk   = 
         case nsCont ns of
         Nothing             -> setCtrlVals inputs (initialState ts' prng subvi)
         Just (LvKState st)  -> st { sTs = ts' }

      setCtrlVals inputs s =
         s {
            sTs        = sTs s + 1,
            sCtrlVals  = fromList (zipWith fromMaybe (toList $ sCtrlVals s) inputs)
         }  

      sk'  = run sk subvi

      nextk
         | not (null (sSched sk'))  =  Just (LvKState sk')
         | shouldStop sk'           =  Nothing
         | otherwise                =  let LvI32 i = index (sCtrlVals sk') 0
                                       in Just (LvKState (nextStep subvi sk' (i + 1)))

      qMyself = [LvElemAddr LvN idx | isJust nextk]

      s2 = s1 {
         sTs       = sTs sk' + 1,
         sPrng     = sPrng sk',
         sSched    = sSched s1 ++ qMyself,
         sNStates  = update idx (ns { nsCont = nextk }) nss
      }

      pvs = zip (indices $ vIndics subvi) (toList $ sIndicVals sk')

   in
      (s2, if isJust nextk then [] else pvs)

\end{code}

Structure nodes use the following auxiliary functions, already mentioned
above. Function |initCounter| checks whether the node state has a
continuation, and initializes the iteration counter if it doesn't. Function
|nextStep| resets the scheduler for the state of the subgraph, and implements
the shift register logic, copying values from indicators marked as
|LvSRIndicator| to their corresponding controls in the new state.

\begin{code}

initCounter :: LvState -> Int -> [Maybe LvValue] -> [Maybe LvValue]
initCounter s idx inputs =
   case nsCont (index (sNStates s) idx) of
   Nothing  -> Just (LvI32 0) : tail inputs
   _        -> inputs

nextStep :: LvVI -> LvState -> Int -> LvState
nextStep vi s i' =
   s {
      sTs        = sTs s + 1,
      sSched     = initialSchedule vi,
      sCtrlVals  = cvs''
   }
   where
      cvs'   = update 0 (LvI32 i') (sCtrlVals s)
      cvs''  =  foldl' shiftRegister cvs' $ zip (vIndics vi) (toList (sIndicVals s))

      shiftRegister ::  Seq LvValue -> ((String, LvIndicator), LvValue) -> Seq LvValue
      shiftRegister cvs ((_, LvSRIndicator cidx), ival) = 
         update cidx ival cvs
      shiftRegister cvs _ = cvs

\end{code}

\section{Operations}

The final section of the interpreter is the implementation of the various
operations available in the language as function nodes. These operations
are implemented as cases for function |applyFunction|, which takes a
string with the name of the function, an instance of the outside world,
the list of input values, and produces a return, which may be a list of
results or a continuation, along with the updated state of the world.

\begin{code}
applyFunction :: String -> LvWorld -> [Maybe LvValue] -> (LvWorld, LvReturn)
\end{code}

However, in the spirit of dataflow, most function nodes implement pure
functions (that is, they do not read or affect the outside world). We
represent them as such, removing the occurrences of |LvWorld| from the
signature:

\begin{code}
applyPureFunction :: String -> [Maybe LvValue] -> LvReturn
\end{code}

To fit the interpreter's execution model, these pure functions can then be
converted to match the expected signature using the following combinator,
which is able to convert the signature of |applyPureFunction| into that of
|applyFunction|, by simply forwarding the |LvWorld| object unchanged:

\begin{code}
withWorld :: (a -> r) -> (w -> a -> (w, r))
withWorld f = \ w args -> (w, f args)
\end{code}

Our goal in this interpreter is not to reproduce the functionality of LabVIEW
with respect to its domain in engineering, but to describe in detail the
semantics of the dataflow language at its core. For this reason, we include
below only a small selection of functions, which should be enough to
illustrate the behavior of the interpreter through examples.

The following pure functions are implemented: arithmetic and relational
operators (Section~\ref{binop}), array functions @Array Max & Min@ and
@Insert Into Array@ (Section~\ref{arrayfns}), and @Bundle@ (a simple function
which packs values into a cluster).

To demonstrate impure functions, the interpreter includes the timer
function @Wait Until Next Ms@ (Section~\ref{waituntilnextms}) and the
PRNG function @Random Number@ (Section~\ref{randomnumber}).

\begin{code}

applyPureFunction name =
   case name of
   "+"                -> numOp   (+)  (+)
   "-"                -> numOp   (-)  (-)
   "*"                -> numOp   (*)  (*)
   "/"                -> numOp   (/)  div
   "<"                -> boolOp  (<)  (<)
   ">"                -> boolOp  (>)  (>)
   "ArrayMax&Min"     -> returnArrayMaxMin
   "InsertIntoArray"  -> returnInsertIntoArray
   "Bundle"           -> returnBundle
   otherwise          -> error ("No rule to apply " ++ name)
   where
      returnArrayMaxMin [Just (LvArr a)] =
         LvReturn (arrayMaxMin a)
      returnInsertIntoArray (Just arr : Just vs : idxs) =
         LvReturn [insertIntoArray arr vs (map toNumber idxs)]
         where toNumber i =  if isNothing i
                             then (-1)
                             else (\(Just (LvI32 n)) -> n) i
      returnBundle args =
         LvReturn [LvCluster (catMaybes args)]

\end{code}

\subsection{Numeric and relational operators}
\label{binop}

LobVIEW nodes automatically perform coercions between integers and doubles.
Since ports in our implementation do not carry type information (it assumes
the input VI has been type-checked prior to execution), we pragmatically
include the coercion logic directly in the implementation for numeric and
relational operator nodes, codified in the |binOp| function, to which the
|numOp| and |boolOp| functions below delegate.

It is worth noting that the LabVIEW UI gives visual feedback when a coercion
takes place, by adding a small circle attached to the input port. This could
be considered an automatically inserted coercion node, not unlike the automatic
insertion of feedback nodes. However, since these are not separate nodes in
LabVIEW (for instance, they cannot be probed as separate objects by the
LabVIEW debugging facilities, unlike feedback nodes), we chose to not implement
them as separate nodes, so keep node structure in input programs for this
interpreter more alike to that of actual LabVIEW programs.


\begin{code}

numOp :: (Double -> Double -> Double)
         -> (Int -> Int -> Int) -> [Maybe LvValue] -> LvReturn
numOp opD opI = LvReturn . return . binOp opD LvDBL opI LvI32

boolOp :: (Double -> Double -> Bool)
          -> (Int -> Int -> Bool) -> [Maybe LvValue] -> LvReturn
boolOp opD opI = LvReturn . return . binOp opD LvBool opI LvBool

binOp :: (Double -> Double -> t) -> (t -> LvValue)
         -> (Int -> Int -> t1) -> (t1 -> LvValue)
         -> [Maybe LvValue] -> LvValue
binOp opD tD _ _  [Just (LvDBL a),  Just (LvDBL b)]  = tD (opD a b)
binOp opD tD _ _  [Just (LvI32 a),  Just (LvDBL b)]  = tD (opD (fromIntegral a) b)
binOp opD tD _ _  [Just (LvDBL a),  Just (LvI32 b)]  = tD (opD a (fromIntegral b))
binOp _ _ opI tI  [Just (LvI32 a),  Just (LvI32 b)]  = tI (opI a b)
binOp _ _ _   _     _                                  = undefined

\end{code}

\subsection{Array functions}
\label{arrayfns}

Representing aggregate data structures and processing them efficiently is a
recognized issue in dataflow
languages~\cite{Johnston:2004:ADP:1013208.1013209}. LabVIEW includes support
for arrays and clusters, and provides a large library of functions to support
these data types. We illustrate two such functions in the interpreter. 

@Array Max & Min@ is a function that takes an array and produces four output
values: the maximum value of the array, the index of this maximum value, the
minimum value of the array, and the index of this minimum value. The design of
this node reflects one concern which appears often in the LabVIEW
documentation and among their users: avoiding excessive array copying. While
languages providing similar functionality typically provide separate functions
for @min@ and @max@, here the language provides all four values at once, to
dissuade the user from processing the array multiple times in case more than
one value is needed. LabVIEW also provides a control structure called @In
Place Element Structure@, not implemented in this interpreter, where an array
and one or more indices are entered as inputs, producing input and output
tunnels for each index, so that values can be replaced in an aggregate data
structure without producing copies. More recent versions of LabVIEW avoid
array copying through optimization, reducing the usefulness of this control
structure.

\begin{code}
arrayMaxMin a =
   if null a
   then [LvDBL 0,  LvI32 0,       LvDBL 0,  LvI32 0]
   else [maxVal,   LvI32 maxIdx,  minVal,   LvI32 minIdx]
        where
           (maxVal, maxIdx) = foldPair (>) a
           (minVal, minIdx) = foldPair (<) a
           foldPair op l = foldl1 (\(x,i) (y,j) -> if op x y
                                                   then (x,i)
                                                   else (y,j))
                                  (zip l (indices l))
\end{code}

An example of a surprisingly large amount of functionality condensed into one
function node is LabVIEW's @Insert Into Array@ operation. To insert into an
array |x| a value |y|, this nodes features as input ports the target array
(|x|), the data to be inserted (|y|, which may also be an array) and one
indexing input port for each dimension of |x|. However, only one indexing port
can be connected; the other ones must remain disconnected, and this indicates
on which dimension the insertion should take place.

The behavior of the function changes depending on which of the inputs are
connected and what are the number of dimensions of array |x| and data |y|.

Given a $n$-dimensional array |x|, value |y| must be either an $n$ or
$(n-1)$-dimensional array (or in the case of inserting into a 1-D array, it
must be either a 1-D array or an element of the array's base type).

For example, if |x| is a 2D array $p \times q$ and |y| is a 1D array of size
$n$, if the first indexing input is connected, it inserts a new row into the
matrix, producing an array $p+1 \times q$; if the second index is connected,
it inserts a new column, and the resulting array size is $p \times q+1$. This
also works in higher dimensions: for example, one can insert a 2D matrix into
a 3D array along one of its three axes.

When the dimensions are the same, the results are different: inserting an
array of size $m \times n$ into an array of size $p \times q$ may produce an
array of size $p+m \times q$ or $p \times q+n$. For all operations, the
dimensions of |y| are cropped or expanded with null values (such as zero or
the empty string) to match the dimensions of |x|.

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
         then (  \ _ lx ly    -> resizeCurr id lx ly,
                 \ _ lx ly    -> resizeLower lx ly,
                 \(LvArr ly)  -> ly)
         else (  \x _  ly     -> resizeCurr id x ly,
                 \x _  ly     -> [LvArr (resizeAll x ly)],
                 \ _          -> [vy])
   
      insertAt i lx ly = LvArr $ take i lx ++ ly ++ drop i lx
      
      recurseTo is lx ly = LvArr $ zipWith (\a b -> insertIntoArray a b is) lx ly
      
      resizeCurr childOp xs@(x:_) ys =
         map childOp $ take (length xs) $ ys ++ (repeat . zero) x
         where
            zero (LvArr l@(x:_))  = LvArr (replicate (length l) (zero x))
            zero (LvDBL _)        = LvDBL 0.0
            zero (LvI32 _)        = LvI32 0
            zero (LvSTR _)        = LvSTR ""
            zero (LvBool _)       = LvBool False
            zero (LvCluster c)    = LvCluster (map zero c)
            zero (LvArr [])       = LvArr []
      
      resizeLower (x:_) ys = map (childResizer x) ys
      
      resizeAll xs@(x:_) ys = resizeCurr (childResizer x) xs ys
      
      childResizer (LvArr x) = \(LvArr a) -> LvArr (resizeAll x a)
      childResizer _ = id
      
      ndims (LvArr (v:_))  = 1 + ndims v
      ndims (LvArr [])     = 1
      ndims _              = 0

\end{code}

\subsection{@Random Number@}
\label{randomnumber}

@Random Number@ is an example of an impure function which produces a
side-effect beyond the value sent through its output port. In our definition
of the ``outside world'', which is part of the ongoing state computed in
our model, we have the state of the pseudo-random number generator, which
needs to be updated each time this node produces a value.

In this interpreter, we implement the PRNG using the 32-bit variant of the
Xorshift algorithm~\cite{xorshift}.

\begin{code}

applyFunction "RandomNumber" w [] =
   let
      mask  = foldl1 (\v b -> v .|. bit b) (0:[0..31])
      n0    = wPrng w
      n1    = (n0 `xor` (n0 `shiftL` 13)) .&. mask
      n2    = (n1 `xor` (n1 `shiftR` 17)) .&. mask
      n3    = (n2 `xor` (n2 `shiftL` 25)) .&. mask
      f     = abs $ (fromIntegral n3) / 2 ^ 32
   in (w { wPrng = n3 }, LvReturn [LvDBL f])

\end{code}

\subsection{@Wait Until Next Ms@}
\label{waituntilnextms}

Node @Wait Until Next Ms@ demonstrates both the use a value coming
from the outside world (the timestamp) and the use of a continuation.
Its goal is to wait until the timestamp matches or exceeds the next
multiple of the given argument. Using this object in loop structures
that are running concurrently causes them to iterate in lockstep,
if the inserted delay is long enough. This is a simple way to produce
an acceptable level of synchronization for the typical domain of
instrument data acquisition which LabVIEW specializes on.

When the function is applied, it immediately returns a continuation,
containing the function |waitUntil| and the target timestamp |nextMs| as its
argument. As we saw in Section~\ref{functionnodes}, this will cause the
function to be rescheduled. The implementation of |waitUntil| checks
the current time received in the |LvWorld| argument: if it has
not reached the target time, the function returns another continuation
rescheduling itself; otherwise, it returns producing no value, since
the function node for this operation has no output ports. This node
relies on the fact that a (sub)graph as a whole keeps running as long
as some node is scheduled.

\begin{code}

applyFunction "WaitUntilNextMs" w [Just (LvI32 ms)] =
   (w, LvContinue $ LvKFunction waitUntil [LvI32 nextMs])
   where
      ts = wTs w
      nextMs = ts - (ts `mod` ms) + ms
      waitUntil w@(LvWorld now _) arg@[LvI32 stop]
         | now >= stop  = (w, LvReturn [])
         | otherwise    = (w, LvContinue $ LvKFunction waitUntil arg)

applyFunction "WaitUntilNextMs" vst [Just (LvDBL msd)] =
   applyFunction "WaitUntilNextMs" vst [Just (LvI32 (floor msd))]

\end{code}

Finally, we finish the definition of |applyFunction| by delegating
the remaining functions to |applyPureFunction|.

\begin{code}

applyFunction n w a = (withWorld . applyPureFunction) n w a

\end{code}

%END LYX TEXT

\end{document}

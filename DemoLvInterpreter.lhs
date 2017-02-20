\documentclass[a4paper]{article}
%\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}
%include polycode.fmt
%include Lv_format.lhs

\begin{document}

\title{Example programs for the LabVIEW interpretere}
\author{Hisham Muhammad}

\maketitle{}

\begin{code}

import LvInterpreter

\end{code}

\begin{code}

main = 
   do
      print program
      loop (initialState 0 program) program
         where program = testingWhile

\end{code}

\section{A for-loop}

On continuous run, running this with 0 in the input produces 0, incrementing
to 1 it remains at 0, and incrementing to 2 it increments the output forever.
This is due to the shift register. Uncommenting the node and the wire that are
marked *** stops the endless increment, but it busy-waits.

\begin{code}

testingFor =
   makeVI
         [ -- controls
            ("input", LvControl (LvDBL 10.0))
         ]
         [ -- indicators
            ("indicator", LvIndicator (LvDBL (-999.0)))
         ]
         [ -- nodes
            -- |("0", LvConstant (LvDBL 0.00)),| -- ***
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("i", LvAutoControl),
                  ("N", LvTunControl),
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
            -- |zwire "0" "shift reg out",| -- ***
            zwire "input" "N",
            zwire "out" "indicator"
         ]            

\end{code}

\section{A while-loop}

Numeric2 only sets after the for-loop is done, this means the while-loop only
starts after its input tunnels have data.

\begin{code}

testingWhile =
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
                  ("i", LvAutoControl),
                  ("N", LvTunControl),
                  ("shift reg out", LvSRControl (LvDBL 0.0))
               ]
               [ -- indicators
                  ("shift reg in", LvSRIndicator 2),
                  ("out", LvTunIndicator LvLastValue)
               ]
               [ -- nodes
                  ("+", LvFunction "+"),
                  ("WaitUntilNextMs", LvFunction "WaitUntilNextMs"),
                  ("100", LvConstant (LvI32 100))
               ]
               [ -- wires
                  nwire "shift reg out" 0 "+" 0,
                  nwire "i" 0             "+" 1,
                  zwire "+" "shift reg in",
                  zwire "+" "out",
                  zwire "100" "WaitUntilNextMs"
               ]
            )),
            ("While loop", LvStructure LvWhile (makeVI
               [ -- controls
                  ("i", LvAutoControl),
                  ("while tunnel", LvTunControl)
               ]
               [ -- indicators
                  ("Test", LvIndicator (LvBool True)),
                  ("Numeric", LvIndicator (LvDBL 0.0)),
                  ("Numeric 2", LvIndicator (LvDBL 0.0))
               ]
               [ -- nodes
                  ("+", LvFunction "+"),
                  ("<", LvFunction "<"),
                  ("WaitUntilNextMs", LvFunction "WaitUntilNextMs"),
                  ("10", LvConstant (LvI32 10)),
                  ("50", LvConstant (LvI32 50)),
                  ("12345", LvConstant (LvI32 12345))
               ]
               [ -- wires
                  nwire "i" 0 "<" 0,
                  nwire "50" 0 "<" 1,
                  nwire "i" 0 "+" 0,
                  nwire "while tunnel" 0 "+" 1,
                  zwire "+" "Numeric",
                  zwire "12345" "Numeric 2",
                  zwire "10" "WaitUntilNextMs",
                  zwire "<" "Test"
               ]
            ))
         ]
         [ -- wires
            --zwire "0" "shift reg out", -- ***
            zwire "input" "N",
            zwire "out" "indicator",
            zwire "out" "while tunnel"
         ]

\end{code}

\section{An example with arrays and clusters}

This is the example displayed in Figure [*** figure number ***]. @Numeric2@
only sets after the for-loop is done, this means the while-loop only starts
after its input tunnels have data.

\begin{code}

randomXY =
   makeVI
         [ -- controls
            ("Number of measurements", LvControl (LvI32 10)),
            ("Delay (sec)", LvControl (LvDBL 2.0))
         ]
         [ -- indicators
            ("XY graph", LvIndicator (LvCluster [LvArr (LvDBL 0.0) [], LvArr (LvDBL 0.0) []]))
         ]
         [ -- nodes
            ("1000", LvConstant (LvDBL 1000.00)),
            ("Delay * 1000", LvFunction "*"),
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("i", LvAutoControl),
                  ("N", LvTunControl),
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

\end{code}

\end{document}

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
         where program = testingCase

wire :: String -> String -> LvStringWire
wire a b = LvStringWire a b

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
         ("output", LvIndicator (LvDBL (-999.0)))
      ]
      [ -- nodes
         -- |("0", LvConstant (LvDBL 0.00)),| -- ***
         ("For loop", LvFor forSum)
      ]
      [ -- wires
         -- |wire "0" "shift reg out",| -- ***
         wire "input"         "For loop:N",
         wire "For loop:out"  "output"
      ]

forSum =
   makeVI
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
         wire "shift reg out"  "+",
         wire "i"              "+:1",
         wire "+"              "shift reg in",
         wire "+"              "out"
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
            ("For", LvFor forSumTimer),
            ("While", LvWhile whileSumTimer)
         ]
         [ -- wires
            --wire "0" "shift reg out", -- ***
            wire "input" "For:N",
            wire "For:out" "indicator",
            wire "For:out" "While:while tunnel"
         ]

forSumTimer = 
   makeVI
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
            ("20", LvConstant (LvI32 20))
         ]
         [ -- wires
            wire "shift reg out"  "+:0",
            wire "i"              "+:1",
            wire "+"              "shift reg in",
            wire "+"              "out",
            wire "20"             "WaitUntilNextMs"
         ]

whileSumTimer =
   makeVI
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
         wire "i"             "<:0",
         wire "50"            "<:1",
         wire "i"             "+:0",
         wire "while tunnel"  "+:1",
         wire "+"             "Numeric",
         wire "12345"         "Numeric 2",
         wire "10"            "WaitUntilNextMs",
         wire "<"             "Test"
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
            ("Delay (sec)", LvControl (LvDBL 0.5))
         ]
         [ -- indicators
            ("XY graph", LvIndicator (LvCluster [LvArr [], LvArr []]))
         ]
         [ -- nodes
            ("1000", LvConstant (LvDBL 1000.00)),
            ("Delay * 1000", LvFunction "*"),
            ("For", LvFor (makeVI
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
                  ("inc", LvFunction "+"),
                  ("mul delay", LvFunction "*"),
                  ("feedback + *", LvFunction "+"),
                  ("100", LvConstant (LvI32 100)),
                  ("mul random", LvFunction "*"),
                  ("RandomNumber", LvFunction "RandomNumber")
               ]
               [ -- wires
                  wire "* tunnel"              "WaitUntilNextMs",
                  wire "i"                     "inc:0",
                  wire "1"                     "inc:1",
                  wire "inc"                   "mul delay:0",
                  wire "delay tunnel"          "mul delay:1",
                  wire "feedback loop tunnel"  "feedback + *:0",
                  wire "mul delay"             "feedback + *:1",
                  wire "feedback + *"          "keys array",
                  wire "100"                   "mul random:0",
                  wire "RandomNumber"          "mul random:1",
                  wire "mul random"            "values array"
               ]
            )),
            ("insert keys", (LvFunction "InsertIntoArray")),
            ("insert values", (LvFunction "InsertIntoArray")),
            ("bundle", (LvFunction "Bundle")),
            ("max key", (LvFunction "ArrayMax&Min")),
            ("feedback to keys array", (LvFeedbackNode (LvArr []))),
            ("feedback to values array", (LvFeedbackNode (LvArr []))),
            ("feedback to loop", (LvFeedbackNode (LvDBL 0.0)))
         ]
         [ -- wires
            wire "Number of measurements"    "For:N",
            wire "Delay (sec)"               "Delay * 1000:0",
            wire "1000"                      "Delay * 1000:1",
            wire "Delay * 1000"              "For:* tunnel",
            wire "Delay (sec)"               "For:delay tunnel",

            wire "feedback to keys array"    "insert keys:0",
            wire "For:keys array"            "insert keys:1",
            wire "insert keys"               "feedback to keys array",
            wire "insert keys"               "max key",
            wire "max key"                   "feedback to loop",
            wire "feedback to loop"          "For:feedback loop tunnel",

            wire "feedback to values array"  "insert values:0",
            wire "For:values array"          "insert values:1",
            wire "insert values"             "feedback to values array",
            
            wire "insert keys"               "bundle:0",
            wire "insert values"             "bundle:1",

            wire "bundle"                    "XY graph"
         ]

\end{code}

\section{An example with a sequence}

\begin{code}

testingSeq =
   makeVI
      [ -- controls
         ("Entry", LvControl (LvI32 5))
      ]
      [ -- indicators
      ]
      [ -- nodes
         ("step1", LvSequence step1),
         ("step2", LvSequence stepN),
         ("step3", LvSequence stepN)
      ]
      [ -- wires
         wire "step1:NEXT" "step2:GO",
         wire "step2:NEXT" "step3:GO",
         wire "Entry" "step1:count",
         wire "Entry" "step2:count",
         wire "step2:out" "step3:count"
      ]

step1 =
   makeVI
      [ -- controls
         ("count", LvTunControl)
      ]
      [ -- indicators
         ("NEXT", LvTunIndicator LvLastValue)
      ]
      [ -- nodes
         ("For", LvFor forSumTimer)
      ]
      [ -- wires
         wire "count" "For:N"
      ]

stepN =
   makeVI
      [ -- controls
         ("GO", LvTunControl),
         ("count", LvTunControl)
      ]
      [ -- indicators
         ("NEXT", LvTunIndicator LvLastValue),
         ("out", LvTunIndicator LvLastValue)
      ]
      [ -- nodes
         ("For", LvFor forSumTimer)
      ]
      [ -- wires
         wire "count" "For:N",
         wire "For:out" "out"
      ]

\end{code}

\section{An example with cases}

\begin{code}

testingCase =
   makeVI
      [ -- controls
      ]
      [ -- indicators
         ("result", LvIndicator (LvArr []))
      ]
      [ -- nodes
         ("3", LvConstant (LvI32 3)),
         ("for", LvFor (makeVI
            [ -- controls
               ("i", LvAutoControl),
               ("N", LvTunControl)
            ]
            [ -- indicators
               ("out", LvTunIndicator LvAutoIndexing)
            ]
            [ -- nodes
               ("case", LvCase [
                  (makeVI
                     [ -- controls
                        ("case", LvControl (LvI32 0)),
                        ("in",   LvControl (LvI32 0))
                     ]
                     [ -- indicators
                        ("out", LvIndicator (LvI32 0))
                     ]
                     [ -- nodes
                        ("+", LvFunction "+"),
                        ("10", LvConstant (LvI32 10))
                     ]
                     [ -- wires
                        wire "in" "+:0",
                        wire "10" "+:1",
                        wire "+" "out"
                     ]
                  ),
                  (makeVI
                     [ -- controls
                        ("case", LvControl (LvI32 0)),
                        ("in",   LvControl (LvI32 0))
                     ]
                     [ -- indicators
                        ("out", LvIndicator (LvI32 0))
                     ]
                     [ -- nodes
                        ("-", LvFunction "-"),
                        ("10", LvConstant (LvI32 10))
                     ]
                     [ -- wires
                        wire "in" "-:0",
                        wire "10" "-:1",
                        wire "-" "out"
                     ]
                  ),
                  (makeVI
                     [
                        ("case", LvControl (LvI32 0)),
                        ("in",   LvControl (LvI32 0))
                     ]
                     [ -- indicators
                        ("out", LvIndicator (LvI32 0))
                     ]
                     [ -- nodes
                        ("*", LvFunction "*"),
                        ("/", LvFunction "/"),
                        ("10", LvConstant (LvI32 10)),
                        ("2", LvConstant (LvI32 2))
                     ]
                     [ -- wires
                        wire "in" "*:0",
                        wire "10" "*:1",
                        wire "*" "/:0",
                        wire "2" "/:1",
                        wire "/" "out"
                     ]
                  )
               ])
            ]
            [ -- wires
               wire "i"        "case:case",
               wire "i"        "case:in",
               wire "case:out" "out"
            ]
         ))
      ]
      [ -- wires
         wire "3" "for:N",
         wire "for:out" "result"
      ]


testingNestedFor =
   makeVI
      [ -- controls
      ]
      [ -- indicators
      ]
      [ -- nodes
         ("outer 3", LvConstant (LvI32 3)),
         ("outer for", LvFor (makeVI
            [ -- controls
               ("i", LvAutoControl),
               ("N", LvTunControl)
            ]
            [ -- indicators
               ("out", LvTunIndicator LvAutoIndexing)
            ]
            [ -- nodes
               ("inner 3", LvConstant (LvI32 3)),
               ("inner for", LvFor (makeVI
                  [ -- controls
                     ("i", LvAutoControl),
                     ("N", LvTunControl)
                  ]
                  [ -- indicators
                     ("out", LvTunIndicator LvAutoIndexing)
                  ]
                  [ -- nodes
                  ]
                  [ -- wires
                  ]
               ))
            ]
            [ -- wires
               wire "inner 3"        "inner for:N"
            ]
         ))
      ]
      [ -- wires
         wire "outer 3" "outer for:N"
      ]

\end{code}

\end{document}

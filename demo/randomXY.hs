
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
            ("For", LvStructure LvFor (makeVI
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

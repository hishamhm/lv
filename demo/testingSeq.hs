
testingSeq =
   makeVI
      [ -- controls
         ("Entry", LvControl (LvI32 5))
      ]
      [ -- indicators
      ]
      [ -- nodes
         ("step1", LvStructure LvSequence step1),
         ("step2", LvStructure LvSequence stepN),
         ("step3", LvStructure LvSequence stepN)
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
         ("For", LvStructure LvFor forSumTimer)
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
         ("For", LvStructure LvFor forSumTimer)
      ]
      [ -- wires
         wire "count" "For:N",
         wire "For:out" "out"
      ]

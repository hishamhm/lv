testingFor =
   makeVI
      [ -- controls
         ("input 1", LvControl (LvDBL 10.0)),
         ("input 2", LvControl (LvDBL 20.0))
      ]
      [ -- indicators
         ("output 1", LvIndicator (LvDBL (-999.0))),
         ("output 2", LvIndicator (LvDBL (-999.0)))
      ]
      [ -- nodes
         ("For loop 1", LvStructure LvFor forSum),
         ("For loop 2", LvStructure LvFor forSum)
      ]
      [ -- wires
         wire "input 1"         "For loop 1:N",
         wire "For loop 1:out"  "output 1",
         wire "input 2"         "For loop 2:N",
         wire "For loop 2:out"  "output 2"
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


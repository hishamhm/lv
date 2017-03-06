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

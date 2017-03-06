
testingCase =
   makeVI
      [ -- controls
      ]
      [ -- indicators
         ("result", LvIndicator (LvArr []))
      ]
      [ -- nodes
         ("3", LvConstant (LvI32 3)),
         ("for", LvStructure LvFor (makeVI
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

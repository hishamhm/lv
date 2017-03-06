
testingNestedFor =
   makeVI
      [ -- controls
      ]
      [ -- indicators
      ]
      [ -- nodes
         ("outer 3", LvConstant (LvI32 3)),
         ("outer for", LvStructure LvFor (makeVI
            [ -- controls
               ("i", LvAutoControl),
               ("N", LvTunControl)
            ]
            [ -- indicators
               ("out", LvTunIndicator LvAutoIndexing)
            ]
            [ -- nodes
               ("inner 3", LvConstant (LvI32 3)),
               ("inner for", LvStructure LvFor (makeVI
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

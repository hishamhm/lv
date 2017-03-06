
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
            ("For", LvStructure LvFor forSumTimer),
            ("While", LvStructure LvWhile whileSumTimer)
         ]
         [ -- wires
            --wire "0" "shift reg out", -- ***
            wire "input" "For:N",
            wire "For:out" "indicator",
            wire "For:out" "While:while tunnel"
         ]


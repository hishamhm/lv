
import Data.Complex
import Data.List
import Data.Maybe

data LvValue = LvEXT Double
             | LvDBL Double
             | LvSGL Float
             | LvFXP Double
             | LvI64 Int
             | LvI32 Int
             | LvI16 Int
             | LvI8  Int
             | LvU64 Int
             | LvU32 Int
             | LvU16 Int
             | LvU8  Int
             | LvBoolean Bool
             | LvString String
             | LvCluster [LvValue]
             | LvArray Int LvValue
             -- | LvCXT Complex
             -- | LvCDB Complex
             -- | LvCSG Complex

data LvControl = LvControl LvValue
               | LvTunnel LvValue -- used in structures
               -- TODO: tables?

data LvIndicator = LvIndicator LvValue
                 | LvAutoIndexedTunnel LvValue -- used in loops
                 -- TODO: tables?

data LvPanel = LvPanel [(String, LvControl)] [(String, LvIndicator)]

data LvType = LvN
            | LvC
            | LvI

data LvWire = LvWire
                LvType -- source node type
                Int -- node index
                Int -- port index
                LvType -- destination node type
                Int -- node index
                Int -- port index

data LvStringWire = LvStringWire (String, Int) (String, Int)

data LvStrucType = LvWhile
                 | LvFor

data LvNode = LvSubVI
            | LvFunction String
            | LvConstant LvValue
            | LvStructure LvStrucType LvVI
            | LvFeedbackNode LvValue

data LvDiagram = LvDiagram 
                    [(String, LvNode)] -- nodes
                    [LvWire] -- wires

data LvVI = LvVI LvPanel LvDiagram

swire a b = LvStringWire (a, 0) (b, 0)

nwire a i b j = LvStringWire (a, i) (b, j)

makeVI :: [(String, LvControl)] -> [(String, LvIndicator)] -> [(String, LvNode)] -> [LvStringWire] -> LvVI
makeVI controls indicators nodes stringWires =
   LvVI (LvPanel controls indicators) (LvDiagram nodes wires)
   where
      findNode :: String -> (LvType, Int)
      findNode name =
         let
            scanList :: [String] -> Maybe (LvType, Int)
            scanList l =
               case elemIndex name l of
                  Just i -> Just (LvC, i)
                  Nothing -> Nothing
         in
            case find isJust $ map scanList [(map fst controls), (map fst indicators), (map fst nodes)] of
               Just (Just pair) -> pair
               Nothing -> error ("Not found: " ++ name)
            
      convert (LvStringWire (from, fromPort) (to, toPort)) =
         let
            (fromType, fromNode) = findNode from
            (toType,   toNode)   = findNode to
         in
            LvWire fromType fromNode fromPort toType toNode toPort
      wires = map convert stringWires

example1 =
   makeVI
         [ -- controls
            ("Number of measurements", LvControl (LvI32 10)),
            ("Delay (sec)", LvControl (LvDBL 0.1))
         ]
         [ -- indicators
            ("XY graph", LvIndicator (LvCluster [LvArray 1 (LvDBL 0.0), LvArray 1 (LvDBL 0.0)]))
         ]
         [ -- nodes
            ("1000", LvConstant (LvDBL 1000.00)),
            ("Delay * 1000", LvFunction "*"),
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("N", LvControl (LvI32 0)),
                  ("i", LvControl (LvI32 0)),
                  ("delay tunnel", LvTunnel (LvDBL 0.0)),
                  ("* tunnel", LvTunnel (LvDBL 0.0))
               ]
               [ -- indicators
                  ("keys array", LvAutoIndexedTunnel (LvDBL 0.0)),
                  ("values array", LvAutoIndexedTunnel (LvDBL 0.0))
               ]
               [ -- nodes
                  ("WaitUntilNextMs", LvFunction "WaitUntilNextMs"),
                  ("i * delay", LvFunction "*"),
                  ("100", LvConstant (LvI32 100)),
                  ("100 * RandomNumber", LvFunction "*"),
                  ("RandomNumber", LvFunction "RandomNumber")
               ]
               [ -- wires
                  nwire "i" 0            "i * delay" 0,
                  nwire "delay tunnel" 0 "i * delay" 1,
                  swire "i * delay" "keys array",
                  swire "* tunnel" "WaitUntilNextMs",
                  nwire "100" 0          "100 * RandomNumber" 0,
                  nwire "RandomNumber" 0 "100 * RandomNumber" 1,
                  swire "100 * RandomNumber" "values array"
               ]
            )),
            ("Bundle", LvFunction "Bundle")
         ]
         [ -- wires
            swire "Number of measurements" "N",
            swire "Delay (sec)" "delay tunnel",
            nwire "Delay (sec)" 0 "Delay * 1000" 0,
            nwire "1000" 0        "Delay * 1000" 1,
            swire "Delay * 1000" "* tunnel",
            nwire "keys array" 0 "Bundle" 0,
            nwire "values array" 0 "Bundle" 1,
            swire "Bundle" "XY graph"
         ]

randomXY =
   makeVI
         [ -- controls
            ("Number of measurements", LvControl (LvI32 10)),
            ("Delay (sec)", LvControl (LvDBL 1.0))
         ]
         [ -- indicators
            ("XY graph", LvIndicator (LvCluster [LvArray 1 (LvDBL 0.0), LvArray 1 (LvDBL 0.0)]))
         ]
         [ -- nodes
            ("1000", LvConstant (LvDBL 1000.00)),
            ("Delay * 1000", LvFunction "*"),
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("N", LvControl (LvI32 0)),
                  ("i", LvControl (LvI32 0)),
                  ("delay tunnel", LvTunnel (LvDBL 0.0)),
                  ("* tunnel", LvTunnel (LvDBL 0.0)),
                  ("feedback loop tunnel", LvTunnel (LvDBL 0.0))
               ]
               [ -- indicators
                  ("keys array", LvAutoIndexedTunnel (LvDBL 0.0)),
                  ("values array", LvAutoIndexedTunnel (LvDBL 0.0))
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
                  swire "* tunnel" "WaitUntilNextMs",
                  nwire "i" 0 "i + 1" 0,
                  nwire "1" 0 "i + 1" 1,
                  nwire "i + 1" 0        "i+1 * delay" 0,
                  nwire "delay tunnel" 0 "i+1 * delay" 1,
                  nwire "feedback loop tunnel" 0 "feedback + *" 0,
                  nwire "i+1 * delay" 0          "feedback + *" 1,
                  swire "feedback + *" "keys array",
                  nwire "100" 0          "100 * RandomNumber" 0,
                  nwire "RandomNumber" 0 "100 * RandomNumber" 1,
                  swire "100 * RandomNumber" "values array"
               ]
            )),
            ("insert keys", (LvFunction "InsertIntoArray")),
            ("insert values", (LvFunction "InsertIntoArray")),
            ("bundle", (LvFunction "Bundle")),
            ("max key", (LvFunction "ArrayMax&Min")),
            ("feedback to keys array", (LvFeedbackNode (LvDBL 0.0))),
            ("feedback to loop", (LvFeedbackNode (LvDBL 0.0)))
         ]
         [ -- wires
            swire "Number of measurements" "N",
            nwire "Delay (sec)" 0 "Delay * 1000" 0,
            nwire "1000" 0        "Delay * 1000" 1,
            swire "Delay * 1000" "* tunnel",
            swire "Delay (sec)" "delay tunnel",

            nwire "feedback to keys array" 0 "insert keys" 0,
            nwire "keys array" 0             "insert keys" 1,
            swire "insert keys" "feedback to keys array",
            swire "insert keys" "max key",
            swire "max key" "feedback to loop",
            swire "feedback to loop" "feedback loop tunnel",

            nwire "feedback to values array" 0 "insert values" 0,
            nwire "values array" 0             "insert values" 1,
            swire "insert values" "feedback to values array",
            
            nwire "insert keys"   0 "bundle" 0,
            nwire "insert values" 0 "bundle" 1,

            swire "bundle" "XY graph"

         ]

main = print (randomXY)

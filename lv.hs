
import Data.List
import Data.Maybe
import Data.Sequence
import Data.Foldable (toList)
import Debug.Trace

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
   deriving Show

data LvControlType = LvCtrl
                   | LvTunCtrl -- used in structures
                   -- TODO: tables?
   deriving Show

data LvControl = LvControl LvControlType LvValue
   deriving Show

data LvTunnelMode = LvAutoIndexing
                  -- TODO other kinds
   deriving Show

data LvIndicatorType = LvIndic
                     | LvTunIndic LvTunnelMode -- used in loops
                     -- TODO: tables?
   deriving Show

data LvIndicator = LvIndicator LvIndicatorType LvValue
   deriving Show

data LvPanel = LvPanel
                  [(String, LvControl)]
                  [(String, LvIndicator)]
   deriving Show

data LvType = LvN
            | LvC
            | LvI
            | LvERR String
   deriving Show

data LvWire = LvWire
                LvType -- source node type
                Int -- node index
                Int -- port index
                LvType -- destination node type
                Int -- node index
                Int -- port index
   deriving Show

data LvStringWire = LvStringWire (String, Int) (String, Int)
   deriving Show

data LvStrucType = LvWhile
                 | LvFor
   deriving Show

data LvNode = LvSubVI LvVI
            | LvFunction String
            | LvConstant LvValue
            | LvStructure LvStrucType LvVI
            | LvFeedbackNode LvValue
   deriving Show

data LvDiagram = LvDiagram 
                    [(String, LvNode)] -- nodes
                    [LvWire] -- wires
   deriving Show

data LvVI = LvVI LvPanel LvDiagram
   deriving Show

data LvState = LvState
                  [LvValue] -- control values
                  [LvValue] -- indicator values
                  [Maybe LvState] -- structures and subVIs
                  [Maybe LvValue] -- wire values
   deriving Show

swire a b = LvStringWire (a, 0) (b, 0)

nwire a i b j = LvStringWire (a, i) (b, j)

data FromTo = From | To
   deriving Eq



makeVI :: [(String, LvControl)] -> [(String, LvIndicator)] -> [(String, LvNode)] -> [LvStringWire] -> LvVI
makeVI controls indicators nodes stringWires =
   LvVI (LvPanel controls indicators) (LvDiagram nodes wires)
   where
      findNode :: FromTo -> String -> Int -> (LvType, Int, Int)
      findNode side name port =
         let
            pickSide :: a -> a -> a
            pickSide a b = if side == From then a else b

            findPort1 ctrls indics = elemIndex name $ pickSide (map fst ctrls) (map fst indics)

            findPort ctrls indics = elemIndex name $ pickSide (map fst ctrls) (map fst indics)
            
            checkNode :: Int -> (String, LvNode) -> Maybe (Int, Int)
            checkNode nodeIdx (_, LvStructure _ (LvVI (LvPanel nodeCtrls nodeIndics) _)) =
               case findPort nodeIndics nodeCtrls of
                  Just i -> Just (nodeIdx, i)
                  Nothing -> Nothing
            checkNode nodeIdx (nodeName, node) =
               if name == nodeName
               then Just (nodeIdx, port)
               else Nothing

-- TEST STRUCTURE

         in
            case findPort1 controls indicators of
               Just i -> (pickSide LvC LvI, i, port)
               Nothing -> case find isJust $ toList $ mapWithIndex checkNode (fromList nodes) of
                  Just (Just (node, nodePort)) -> (LvN, node, nodePort)
                  Nothing -> (LvERR name, 0, port)

      convert :: LvStringWire -> LvWire      
      convert (LvStringWire (from, fromPort) (to, toPort)) =
         let
            (fromType, fromNode, fromPort') = findNode From from fromPort
            (toType,   toNode,   toPort')   = findNode To   to   toPort
         in
            LvWire fromType fromNode fromPort' toType toNode toPort'
      
      wires :: [LvWire]
      wires = map convert stringWires

example1 =
   makeVI
         [ -- controls
            ("Number of measurements", LvControl LvCtrl (LvI32 10)),
            ("Delay (sec)", LvControl LvCtrl (LvDBL 0.1))
         ]
         [ -- indicators
            ("XY graph", LvIndicator LvIndic (LvCluster [LvArray 1 (LvDBL 0.0), LvArray 1 (LvDBL 0.0)]))
         ]
         [ -- nodes
            ("1000", LvConstant (LvDBL 1000.00)),
            ("Delay * 1000", LvFunction "*"),
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("N", LvControl LvCtrl (LvI32 0)),
                  ("i", LvControl LvCtrl (LvI32 0)),
                  ("delay tunnel", LvControl LvTunCtrl (LvDBL 0.0)),
                  ("* tunnel", LvControl LvTunCtrl (LvDBL 0.0))
               ]
               [ -- indicators
                  ("keys array", LvIndicator (LvTunIndic LvAutoIndexing) (LvDBL 0.0)),
                  ("values array", LvIndicator (LvTunIndic LvAutoIndexing) (LvDBL 0.0))
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
            ("Number of measurements", LvControl LvCtrl (LvI32 10)),
            ("Delay (sec)", LvControl LvCtrl (LvDBL 1.0))
         ]
         [ -- indicators
            ("XY graph", LvIndicator LvIndic (LvCluster [LvArray 1 (LvDBL 0.0), LvArray 1 (LvDBL 0.0)]))
         ]
         [ -- nodes
            ("1000", LvConstant (LvDBL 1000.00)),
            ("Delay * 1000", LvFunction "*"),
            ("For loop", LvStructure LvFor (makeVI
               [ -- controls
                  ("N", LvControl LvCtrl (LvI32 0)),
                  ("i", LvControl LvCtrl (LvI32 0)),
                  ("delay tunnel", LvControl LvTunCtrl (LvDBL 0.0)),
                  ("* tunnel", LvControl LvTunCtrl (LvDBL 0.0)),
                  ("feedback loop tunnel", LvControl LvTunCtrl (LvDBL 0.0))
               ]
               [ -- indicators
                  ("keys array", LvIndicator (LvTunIndic LvAutoIndexing)  (LvDBL 0.0)),
                  ("values array", LvIndicator (LvTunIndic LvAutoIndexing)  (LvDBL 0.0))
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
            ("feedback to values array", (LvFeedbackNode (LvDBL 0.0))),
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

initialState :: LvVI -> LvState
initialState (LvVI (LvPanel controls indicators) (LvDiagram nodes wires)) =
   LvState controlValues indicatorValues subStates wireValues
      where
         controlValues = map (\(_, LvControl _ v) -> v) controls
         indicatorValues = map (\(_, LvIndicator _ v) -> v) indicators
         subStates = map expandNode nodes
            where
               expandNode :: (String, LvNode) -> Maybe LvState
               expandNode node = 
                  case node of
                     (_, LvSubVI vi) -> Just $ initialState vi
                     (_, LvStructure _ vi) -> Just $ initialState vi
                     otherwise -> Nothing
         wireValues = map (\_ -> Nothing) wires

-- TODO this is implying ordering, but LabVIEW does not specify it
run :: LvState -> LvVI -> LvState
run state (LvVI (LvPanel controls indicators) (LvDiagram nodes wires)) =
   (\state' -> foldl' fireIndicator state' indicators) $ foldl' fireNode state nodes
   where
      -- TODO
      fireNode state node = state
      -- TODO
      fireIndicator state indicator = state


main = 
   let
      program = randomXY
      state = run (initialState program) program
   in do
      print state


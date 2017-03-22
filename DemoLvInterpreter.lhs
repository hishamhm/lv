\documentclass[a4paper]{article}
%\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}
%include polycode.fmt
%include Lv_format.lhs

\begin{document}

\title{Example programs for the LabVIEW interpreter}
\author{Hisham Muhammad}

\maketitle{}

%BEGIN LYX DEMO

Again, the implementation uses only standard modules included in the Haskell Platform.

\begin{code}

import LvInterpreter
import Data.Sequence (fromList, elemIndexL)
import Data.List
import Data.Maybe
import Data.List.Split

\end{code}

\begin{code}

main = 
   do
      print vi
      runVI vi
         where vi = randomXY

\end{code}

\section{Program construction}

To ease the writing of tests, we construct |LvVI| objects using a convenience
function which converts the definition of wires from textual names to the
numeric indices expected by the interpreter.

\begin{code}

data LvStringWire = LvStringWire String String
   deriving Show

wire :: String -> String -> LvStringWire
wire a b = LvStringWire a b

makeVI ::  [(String, LvControl)] -> [(String, LvIndicator)]
           -> [(String, LvNode)] -> [LvStringWire] -> LvVI
makeVI ctrls indics nodes stringWires =
   LvVI {
      vCtrls = ctrls,
      vIndics = indics,
      vNodes = nodes,
      vWires = map convert stringWires
   }
   where
      convert :: LvStringWire -> LvWire      
      convert (LvStringWire src dst) =
         let
            (srcType,  srcElem,  srcPort')  = findElem ctrls   LvC  vIndics  src
            (dstType,  dstElem,  dstPort')  = findElem indics  LvI  vCtrls   dst
         in
            LvWire (LvPortAddr srcType srcElem srcPort')
                   (LvPortAddr dstType dstElem dstPort')

      findIndex :: [(String, a)] -> String -> Maybe Int
      findIndex es name = elemIndex name $ map fst es
      
      must :: (String -> Maybe a) -> String -> a
      must fn name = fromMaybe (error ("No such entry " ++ name)) (fn name)

      findElem ::  [(String, a)] -> LvElemType -> (LvVI -> [(String, b)])
                   -> String -> (LvElemType, Int, Int)
      findElem entries etype elems name
       | isJust $ find (== ':') name =
            let 
               [elemName, portName] = splitOn ":" name
               elem = (must . flip lookup) nodes elemName
               findPort (LvStructure _ subVi)  = must $ findIndex (elems subVi)
               findPort (LvCase subVis)        = must $ findIndex (elems (head subVis))
               findPort (LvFunction _)         = \s -> if null s then 0 else read s
               findPort _                      = \s -> 0
            in
               (LvN, (must . findIndex) nodes elemName, findPort elem portName)
       | otherwise =
          case findIndex entries name of
          Just i -> (etype, i, 0)
          Nothing -> findElem entries etype elems (name ++ ":0")

\end{code}

\section{Demonstration of the VI}

This is the example displayed in Figure \ref{fig:LabVIEW-windows}.

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

\end{code}

%END LYX DEMO

\section{Concurrent for-loops}

This example demonstrates two structures running concurrently. Two identical
for-loops run with different inputs.

\begin{code}

#include "demo/testingFor.hs"

\end{code}

\section{A while-loop}

Numeric2 only sets after the for-loop is done, this means the while-loop only
starts after its input tunnels have data.

\begin{code}

#include "demo/testingWhile.hs"

#include "testutil/sumTimers.hs"

\end{code}

\section{An example with a sequence}

\begin{code}

#include "demo/testingSeq.hs"

\end{code}

\section{An example with cases}

\begin{code}

#include "demo/testingCase.hs"

\end{code}

\end{document}

\documentclass[a4paper]{article}
%\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}
%include polycode.fmt
%include Lv_format.lhs

\begin{document}

\title{Example programs for the LabVIEW interpreter}
\author{Hisham Muhammad}

\maketitle{}

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

\begin{code}

#include "testutil/makeVI.hs"

\end{code}

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

\section{An example with arrays and clusters}

This is the example displayed in Figure [*** figure number ***]. @Numeric2@
only sets after the for-loop is done, this means the while-loop only starts
after its input tunnels have data.

\begin{code}

#include "demo/randomXY.hs"

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

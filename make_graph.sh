#!/bin/sh

FRAMERATE=2
FORMAT=png
FORK=6

PROGRAM=$1
GRAPH=$PROGRAM.mp4

rm -f graph/graph-*dot
rm -f graph/graph-*$FORMAT

$PROGRAM 2> /dev/null | lua lv_viewer.lua $FRAMERATE

frames=`ls graph/graph-*.dot | wc -l`
time (
   for j in `seq 0 $[$FORK-1]`
   do (
      for i in `seq 1 $FORK $frames`
      do
         n=`printf "%07.0f" $[i+j]`
         echo $n/$frames
         dot -T$FORMAT graph/graph-$n.dot > graph/graph-$n.$FORMAT || rm graph/graph-$n.$FORMAT
      done ) &
   done
   wait
)

rm -f $GRAPH
ffmpeg -f image2 -framerate $FRAMERATE -i graph/graph-%07d.$FORMAT -vcodec h264 -qp 0 -preset veryslow $GRAPH


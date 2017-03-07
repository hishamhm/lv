#!/bin/sh

ok="\033[1;32mOK\033[0m"
failed="\033[1;31mFailed\033[0m"

rm -f .failed
for i in demo/*.hs
do
(
   b=$(basename $i .hs)
   dir=$(mktemp -d)
   cp LvInterpreter.in.lhs $dir/LvInterpreter.lhs
   src="$dir/Test_$b.hs"
   exe="Test_$b"
   cat <<EOF > $src

import LvInterpreter
import Data.Sequence (fromList, elemIndexL)
import Data.List
import Data.Maybe
import Data.List.Split

main = 
   do
      print vi
      runVI vi
         where vi = $b

EOF
   cat testutil/* >> $src
   cat $i >> $src
   ghc -i. -i$dir -o $exe $src > /dev/null || {
      rm LvInterpreter.lhs
      echo -e "$failed compiling $src"
      rm -rf "$dir"
      touch .failed
      exit 1
   }
   ./$exe 2> /dev/null > $exe.out || {
      echo -e "$failed running $exe"
      rm -rf "$dir"
      touch .failed
      exit 1
   }
   rm -rf "$dir"
   if diff $exe.out $exe.ref > /dev/null
   then
      echo -e "$ok! Test_$b"
   else
      echo -e "$failed! Test_$b"
      touch .failed
      exit 1
   fi
) &
done

wait

if [ -e .failed ]
then
   rm -f .failed
   exit 1
fi
rm -f .failed
exit 0

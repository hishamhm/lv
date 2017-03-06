#!/bin/sh

ok="\033[1;32mOK\033[0m"
failed="\033[1;31mFailed\033[0m"

for i in demo/*.hs
do
   b=$(basename $i .hs)
   src="Test_$b.hs"
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
   echo "Compiling Test_$b"
   cp LvInterpreter.in.lhs LvInterpreter.lhs
   ghc -i. -o $exe $src || {
      rm LvInterpreter.lhs
      echo -e "$failed compiling $src"
      exit 1
   }
   rm LvInterpreter.lhs
   echo "Running Test_$b"
   ./$exe 2> /dev/null > $exe.out || {
      echo -e "$failed running $exe"
      exit 1
   }
   if diff $exe.out $exe.ref
   then
      echo -e "$ok! Test_$b"
   else
      echo -e "$failed! Test_$b"
      exit 1
   fi
done

echo
echo -e "\033[1;32mAll passed! :)\033[0m"
echo

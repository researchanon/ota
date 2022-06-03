#!/bin/bash

if [ -z $1 ]
then
	REP=1O
else
	REP=$1
fi


make cleanota
make ota
echo -e "version\tthreads\ttime" > "Set70RResults.txt"

for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ; 
        do
        	./TestOTASet 3000 2000 5000 $NTHREADS ops70Reads.txt set2000.txt +RTS -N$NTHREADS >> "Set70RResults.txt"
        done
done 

make cleanstm
make stm


for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ;
        do
        	./TestSTMSet 3000 2000 5000 $NTHREADS ops70Reads.txt set2000.txt +RTS -N$NTHREADS >> "Set70RResults.txt"
        done
done

make cleanota
make ota
echo -e "version\tthreads\ttime" > "Set70WResults.txt"

for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ; 
        do
        	./TestOTASet 3000 2000 5000 $NTHREADS ops70Write.txt set2000.txt +RTS -N$NTHREADS >> "Set70WResults.txt"
        done
done 

make cleanstm
make stm


for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ;
        do
        	./TestSTMSet 3000 2000 5000 $NTHREADS ops70Write.txt set2000.txt +RTS -N$NTHREADS >> "Set70WResults.txt"
        done
done


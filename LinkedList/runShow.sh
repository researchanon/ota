#!/bin/bash

if [ -z $1 ]
then
	REP=1O
else
	REP=$1
fi


make cleancas
make cas
echo -e "version\tthreads\ttime" > "LL70RResults.txt"

for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ;
        do
        	./TestCASLL 3000 2000 5000 $NTHREADS ops70Reads.txt set2000.txt +RTS -N$NTHREADS >> "LL70RResults.txt"
        done
done



make cleanota
make ota

for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ; 
        do
        	./TestOTAFRWLL 3000 2000 5000 $NTHREADS ops70Reads.txt set2000.txt +RTS -N$NTHREADS >> "LL70RResults.txt"
        done
done 

make cleanotato
make otato

for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ; 
        do
        	./TestOTATOLL 3000 2000 5000 $NTHREADS ops70Reads.txt set2000.txt +RTS -N$NTHREADS >> "LL70RResults.txt"
        done
done

make cleanstm
make stm


for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ;
        do
        	./TestSTMLL 3000 2000 5000 $NTHREADS ops70Reads.txt set2000.txt +RTS -N$NTHREADS >> "LL70RResults.txt"
        done
done



make cleancas
make cas
echo -e "version\tthreads\ttime" > "LL70WResults.txt"

for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ; 
        do
        	./TestCASLL 3000 2000 5000 $NTHREADS ops70Write.txt set2000.txt +RTS -N$NTHREADS >> "LL70WResults.txt"
        done
done 


make cleanota
make ota

for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ; 
        do
        	./TestOTATOLL 3000 2000 5000 $NTHREADS ops70Write.txt set2000.txt +RTS -N$NTHREADS >> "LL70WResults.txt"
        done
done 



make cleanstm
make stm


for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ;
        do
        	./TestSTMLL 3000 2000 5000 $NTHREADS ops70Write.txt set2000.txt -N$NTHREADS >> "LL70WResults.txt"
        done
done


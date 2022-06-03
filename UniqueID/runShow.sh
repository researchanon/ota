#!/bin/bash

if [ -z $1 ]
then
	REP=1O
else
	REP=$1
fi


make cleancas
make cas
echo -e "version\tthreads\ttime" > "IDResults.txt"

for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ; 
        do
        	./IDCAS 1000000 $NTHREADS +RTS -N$NTHREADS >> "IDResults.txt"
        done
done 


make cleanota
make ota

for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ;
        do
        	./IDOTA 1000000 $NTHREADS +RTS -N$NTHREADS >> "IDResults.txt"
        done
done

make cleanstm
make stm


for NTHREADS in 2 4 8 16 32 64;
     do
        for i in $(seq 1 $REP) ;
        do
        ./IDSTM 1000000 $NTHREADS +RTS -N$NTHREADS >> "IDResults.txt"
        done
done

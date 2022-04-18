#!/bin/bash

for i in $(seq 4000 100 5000)
do
    ./dft-display -n test.log ${i} > tree_structure/tree_${i}.txt
    
done

for i in $(seq 4000 100 5000)
do
    tail -n +9 tree_structure/tree_${i}.txt > tree_structure/tree_${i}_trimmed.txt
done
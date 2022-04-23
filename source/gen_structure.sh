#!/bin/bash

for i in $(seq 2000 20 4000)
do
    ./dft-display -n test1.log ${i} > tree_structure/tree_${i}.txt
done

for i in $(seq 2000 20 4000)
do
    tail -n +9 tree_structure/tree_${i}.txt > tree_structure/tree_${i}_trimmed.txt
done
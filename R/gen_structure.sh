#!/bin/bash

for i in $(seq 5000 20 7000)
do
    ./dft-display -n test1.log ${i} > tree_structure1/tree_${i}.txt
    ./dft-display -n test3.log ${i} > tree_structure3/tree_${i}.txt
done

for i in $(seq 5000 20 7000)
do
    tail -n +9 tree_structure1/tree_${i}.txt > tree_structure1/tree_${i}_trimmed.txt
    tail -n +9 tree_structure3/tree_${i}.txt > tree_structure3/tree_${i}_trimmed.txt
done
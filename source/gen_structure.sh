#!/bin/bash

for i in $(seq 1500 25 4000)
do
    ./dft-display -n test1.log ${i} > test_tree_structure/tree_${i}.txt
    ./dft-display -n cytof.log ${i} > cytof_tree_structure/tree_${i}.txt
done

for i in $(seq 1500 25 4000)
do
    tail -n +9 test_tree_structure/tree_${i}.txt > test_tree_structure/tree_${i}_trimmed.txt
    tail -n +9 cytof_tree_structure/tree_${i}.txt > cytof_tree_structure/tree_${i}_trimmed.txt
done
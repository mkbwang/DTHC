#!/bin/bash


for i in $(seq 1 4000)
do
    ./dft-pred pa test1.log ${i}:${i} | head -6 | tail +6 >> test_lik.txt
    ./dft-pred pa cytof.log ${i}:${i} | head -6 | tail +6 >> cytof_lik.txt
done

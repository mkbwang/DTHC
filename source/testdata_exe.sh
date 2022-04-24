#!/bin/bash

./dft-spec test1.log 0 2 / 0.1:0.5:0.5 0.01:0.5 0.01:0.5 0.01:0.5

./data-spec test1.log 0 2 / testdatavar1@1:160 . testdatavar1@1:160 . 

./mc-spec test1.log sample-locations slice-positions 20 met-terminals 20 slice-div 1 20 gibbs-sigmas 20

./dft-mc test1.log 4000


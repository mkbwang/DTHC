#!/bin/bash


./dft-spec cytof.log 0 3 / 0.5:0.5:0.5 - 0.5:0.5 0.5:0.5

./model-spec cytof.log real 0.8:0.8

./data-spec cytof.log 0 3 / CyTOFdata@1:796 . CyTOFdata@1:796 . 

./mc-spec cytof.log sample-locations slice-positions 20 met-terminals 20 slice-div 1 20 gibbs-sigmas 20

./dft-mc cytof.log 4000


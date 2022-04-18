#!/bin/bash

./dft-spec test1.log 0 2 / 0.1:0.5:0.5 0.01:0.5 0.01:0.5 0.01:0.5

./data-spec test1.log 0 2 / testdatavar1@1:160 . 

./mc-spec test1.log repeat 25 sample-locations slice-positions gibbs-sigmas

./dft-mc test1.log 7000



./dft-spec test3.log 0 2 / 0.1:0.5:0.5 0.01:0.5 0.01:0.5 0.01:0.5

./data-spec test3.log 0 2 / testdatavar3@1:160 . 

./mc-spec test3.log repeat 25 sample-locations slice-positions gibbs-sigmas

./dft-mc test3.log 7000



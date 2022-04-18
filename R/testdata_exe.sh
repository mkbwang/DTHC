#!/bin/bash

./dft-spec test.log 0 2 / 0.1:0.5:0.5 0.01:0.5 0.01:0.5 0.01:0.5

./data-spec test.log 0 2 / testdata@1:160 . 

./mc-spec test.log repeat 25 sample-locations slice-positions gibbs-sigmas

./dft-mc test.log 5000


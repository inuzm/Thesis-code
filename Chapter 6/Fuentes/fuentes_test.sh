#!/usr/bin/env bash

# This code runs, via R, a chi squared test and a bayesian test for whether
# there is homogeneity between the chatacters in La silla del aguila

Rscript --vanilla ../Rcode/fuentes.R ./data/ \
 ./fuentes-kappa-0.6-tau-64-batchsize-256/wordtable.txt

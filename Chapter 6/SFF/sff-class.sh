#!/usr/bin/env bash

# Adjusting logistic regression to classify between sci-fi and fantasy

# Creating contingency table for novels and topics within them.
# Comment the next line if the table is already created.

Rscript --vanilla ../Rcode/sff-contingency.R ./data \
 ./sff-kappa-2.0-tau-64-batchsize-16/wordtable.txt ./results/topics.txt

# Running LOOCV to get best glmnet parameters
# Comment if the optimization has taken place

Rscript --vanilla ../Rcode/sffprobs.R ./results/topics.txt \
 ./results/best_param.txt ./results/sff_probs.txt

# Outputting graphs

Rscript --vanilla ../Rcode/sff-predictions.R ./results/topics.txt \
 ./results/sff_probs.txt \
 ./results/unnamedodds.png \
 ./results/namedodds.png

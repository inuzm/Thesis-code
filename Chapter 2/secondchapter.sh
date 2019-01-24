#!/usr/bin/env bash

# Geting graphs with faithful data

Rscript --vanilla ./faithful.R ./faithful_nodentisy.png ./faithful-density.png

Rscript --vanilla ./k-means-iris-R \
    ./iris_original \
    ./iris_0.png \
    ./iris_1.png \
    ./iris_final

Rscript --vanilla ./gaussian-mixture.R \
    ./galaxy-density.png \
    ./iris-estimated-association.png \
    ./iris-real-association.png

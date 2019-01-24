#!/usr/bin/env Rscript

# arguments from bash

args <- commandArgs(trailingOnly = TRUE)

# Test if there is at least one argument: if not, return an error

if (length(args) <= 1) {
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}

# Define similarity matrix

sim.mat <- function(in.table){
	N <- ncol(in.table)
	n <- nrow(in.table)
	ret.mat <- diag(1, N)
	for(i in 1:N){
		for(j in i:N){
			ret.mat[i,j] <- length(intersect(in.table[,i], in.table[,j])) / n
			ret.mat[j,i] <- ret.mat[i,j]
		}
	}
	return(ret.mat)
}

words <- read.table(file = args[1], header = F, sep = "\t", stringsAsFactors = F)
A <- sim.mat(words)

require(ggplot2)
require(reshape2)

p <- ggplot(data = melt(A), aes(x = Var1, y = Var2, fill = value)) +
		geom_tile() +
		scale_fill_gradient(low = "white", high = "black", guide = FALSE) +
		xlab("") +
		ylab("") +
		theme(axis.text = element_blank(),
			axis.ticks = element_blank(),
			panel.background = element_blank())

ggsave(filename = args[2], plot = p, device = "png", scale = 2)

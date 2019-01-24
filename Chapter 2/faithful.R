#!/usr/bin/env Rscript
# Where are the graphs to be saved?

args <- commandArgs(trailingOnly = TRUE)

# Test if there is at least one argument: if not, return an error

if (length(args) <= 1) {
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}

require(ggplot2)

data("faithful")

# Graph the standarized values of the faithful data

p1 <- ggplot(data = faithful, aes(x = scale(eruptions), y = scale(waiting))) +
    geom_point() +
    ylab("Waiting time bewtween eruptions") +
    xbal("Eruption duration")

# Graph the standarized values of the faithful data along with an estimation
# of the density

p2 <- ggplot(data = faithful, aes(x = scale(eruptions), y = scale(waiting))) +
    geom_point() +
    ylab("Waiting time bewtween eruptions") +
    xlab("Eruption duration") +
    geom_density2d()

png(args[1], width = 500, height = 520)
plot(p1)
dev.off()

png(args[2], width = 500, height = 520,)
plot(p2)
dev.off()

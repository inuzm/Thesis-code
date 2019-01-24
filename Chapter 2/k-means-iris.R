#!/usr/bin/env Rscript
# Where are the graphs to be saved?

args <- commandArgs(trailingOnly = TRUE)

# Test if there is at least one argument: if not, return an error

if (length(args) <= 1) {
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}

data <- iris

library(ggplot2)
library(tidyverse)

# Original distribution of the data (taking only petak length and with into account)
data_m <- data %>%
    group_by(Species) %>%
    summarise(
        Petal.Length = mean(Petal.Length),
        Petal.Width = mean(Petal.Width)
    )

p1 <- ggplot(data = data,
       aes(x = Petal.Length,
           y = Petal.Width,
           group = Species,
           color = Species)) +
    geom_point() +
    geom_point(data = data_m, size = 4) +
    labs(color = "Species",
         x = "Length",
         y = "Width",
         title = "Petal size")

# Species names
species <- factor(x = unique(data$Species))

# K-means initialization
data_u <- data %>%
    select(Petal.Length, Petal.Width, Species)
data_u$Species <- species[sample(1:3, size = 150, replace = TRUE)]
data_m <- data_u %>%
    group_by(Species) %>%
    summarise(
        Petal.Length = mean(Petal.Length),
        Petal.Width = mean(Petal.Width)
    )

# Initial classification of the method
p2 <- ggplot(data = data_u,
       aes(x = Petal.Length,
           y = Petal.Width,
           group = Species,
           color = Species)) +
    geom_point() +
    geom_point(data = data_m, size = 4) +
    labs(color = "Species",
         x = "Length",
         y = "Width",
         title = "Petal size")

# First iteration
change <- rep(0, 150)
for(i in 1:150){
    difnor <- rep(0, 3)
    for(j in 1:3){
        difnor[j] <- norm(x = data_u[i, -3] - data_m[j, -1], type = "2")
    }
    asig <- species[which.min(difnor)]
    change[i] <- (asig != data_u$Species[i])
    data_u$Species[i] <- asig
}

# Calculating new means
data_m <- data_u %>%
    group_by(Species) %>%
    summarise(
        Petal.Length = mean(Petal.Length),
        Petal.Width = mean(Petal.Width)
    )

# Graph after first iteration
p3 <- ggplot(data = data_u,
       aes(x = Petal.Length,
           y = Petal.Width,
           group = Species,
           color = Species)) +
    geom_point() +
    geom_point(data = data_m, size = 4) +
    labs(color = "Species",
         x = "Length",
         y = "Width",
         title = "Petal size")

iter <- 1
while(sum(change) > 0){
    for(i in 1:150){
        difnor <- rep(0, 3)
        for(j in 1:3){
            difnor[j] <- norm(x = data_u[i, -3] - data_m[j, -1], type = "2")
        }
        asig <- species[which.min(difnor)]
        change[i] <- (asig != data_u$Species[i])
        data_u$Species[i] <- asig
    }

    data_m <- data_u %>%
        group_by(Species) %>%
        summarise(
            Petal.Length = mean(Petal.Length),
            Petal.Width = mean(Petal.Width)
        )
    iter <- iter + 1
}

# Final classification of the iris data set.
p4 <- ggplot(data = data_u,
       aes(x = Petal.Length,
           y = Petal.Width,
           group = Species,
           color = Species)) +
    geom_point() +
    geom_point(data = data_m, size = 4) +
    labs(color = "Species",
         x = "Length",
         y = "Width",
         title = "Petal size")

# Saving the plots externally

png(args[1], width = 543, height = 374)
plot(p1)
dev.off()

png(args[2], width = 543, height = 374)
plot(p2)
dev.off()

png(args[3], width = 543, height = 374)
plot(p3)
dev.off()

png(args[4], width = 543, height = 374)
plot(p4)
dev.off()

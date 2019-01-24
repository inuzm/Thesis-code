#!/usr/bin/env Rscript

# Where are the files?

args <- commandArgs(trailingOnly = TRUE)

# Test if there is at least one argument: if not, return an error

if (length(args) <= 1) {
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}

# Read contingency table
cat(sprintf("Reading contingency table.\n"))
topics.sff <- read.table(file = args[1],
                header = TRUE,
                stringsAsFactors = FALSE)

require(glmnet)

less.sff <- topics.sff[,-1]
less.sff$genre <- factor(less.sff$genre, levels = 0:1, labels = c("f", "sf"))

# Now that I know which parameters work, a full LOOCV penalized
# logistic regression will be ran and classification probabilities will be
# estimated for every left item to plot them.

probs <- data.frame(f = numeric(length = 130), sf = numeric(length = 130))

for(i in 1:130){
	fit1 <- glmnet(
		x = as.matrix(less.sff[-i, -301]),
		y = less.sff$genre[-i],
		family = "binomial",
		alpha = 16/99,
		lambda = 0.02
		)
	aux <- predict(fit1, as.matrix(less.sff[i,-301]), type = "response")
	probs[i, ] <- c(1 - aux, aux)
}

# Loading other predictions

caret_model <- read.table(
    file = args[2],
    header = TRUE, sep = " "
)

# Plotting logodds

require(ggplot2)

full.table <- cbind(data.frame(fp = probs[, 1], sp = probs[, 2]), caret_model)

full.table$logodds <- log(full.table[,2]/full.table[,1])


full.table$predpart <- factor(ifelse(
    full.table$logodds > 0,
    "sf",
    "f"),
    levels = c("f", "sf")
    )

full.table$names <- topics.sff$names

logodds.cutoffs <- unique(c(
    seq(0, max(full.table$logodds), len = 10),
    seq(min(full.table$logodds), 0, len = 20)
))

p1 <- ggplot(
    data = full.table[,-(1:4)],
    aes(x = names,
        y = logodds,
        fill = obs == predpart
        )
    ) +
    geom_bar(stat = "identity", width = 1, size = 0.2) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_rect(fill = "white"),
          legend.position = "top") +
    labs(fill = "Genre classification") +
    scale_fill_manual(labels = c("Incorrect", "Correct"),
                       values = c("red", "light blue")) +
    xlab(label = "Novel") +
    ylab(label = "Logodds") +
    geom_text(aes(y = ifelse(logodds > 0, -0.2, 0.2),
                  label = names,
                  hjust = ifelse(logodds < 0, 0, 1)),
              size = 3, colour = "black") +
    coord_flip()


p <- ggplot(
    data = full.table[,-(1:4)],
    aes(x = names, y = logodds, colour = obs == predpart )
) +
    geom_hline(
        yintercept = sort(logodds.cutoffs),
        linetype = "dotted", color = "light blue"
    ) +
    geom_point() +
    geom_segment(aes(xend = names), yend = 0) +
    expand_limits(y = 0) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.margin = unit(c(1,3,1,1), "lines"),
          legend.position = "top") +
    labs(colour = "Genre classification") +
    scale_color_manual(labels = c("Incorrect", "Correct"),
                       values = c("red", "blue")) +
    xlab(label = "Novels") +
    ylab(label = "Logodds") +
    coord_cartesian(clip = 'off')

for(i in 1:length(logodds.cutoffs)){
    p <- p + annotate(geom = "text", x = 130.25,
                      y = logodds.cutoffs[i],
                      label = ifelse(
                          logodds.cutoffs[i] > 0,
                          sprintf("%3d:1", ceiling(exp(logodds.cutoffs[i]))),
                          ifelse(logodds.cutoffs[i] < 0,
                                 sprintf("  1:%1d", ceiling(exp(-logodds.cutoffs[i]))),
                                 "  1:1")),
                      hjust = 0, size = 2)
}


#ggsave(filename = args[4], plot = p1, width = 1800, height = 2000)
#ggsave(filename = args[5], plot = p, width = 1100, height = 700)

png(args[4], width = 1800, height = 2000, res = 72)
plot(p1)
dev.off()

png(args[3], width = 1100, height = 700, res = 72)
plot(p)
dev.off()

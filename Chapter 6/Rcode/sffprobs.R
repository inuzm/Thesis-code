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

##########################################
# Adjusting model via caret
##########################################

require(caret)
require(glmnet)

less.sff <- topics.sff[,-1]
less.sff$genre <- factor(less.sff$genre, levels = 0:1, labels = c("f", "sf"))
print(less.sff$genre)

set.seed(849)

cctrl1 <- trainControl(
    method = "LOOCV",
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    verboseIter = TRUE)


cat(sprintf("Running the model via caret.\n"))
fit.model <- train(genre ~.,
                data = less.sff,
                method = "glmnet",
                trControl = cctrl1,
                tuneGrid = expand.grid(
                    alpha = seq(0, 1, len = 1e2),
                    lambda = seq(1e-3,1,by = 1e-3)
                    ),
                metric = "ROC"
)
cat(sprintf("Caret has finished running.\n"))
cat(sprintf("Printing best parameters.\n"))
write.table(
    modelo$bestTune,
    row.names = F,
    file = args[2]
    )

probabilities <- extractProb(models = list(fit.modelo))

cat(sprintf("\n\n\nPrinting results from caret in %s.\n", args[3]))
cat(sprintf("Note that the probabilities are for a penalized logistic regression fitted on the whole data set.\n"))

write.table(x = probabilities[,1:4],
    file = args[3],
    row.names = FALSE)

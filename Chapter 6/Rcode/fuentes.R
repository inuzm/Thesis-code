#!/usr/bin/env Rscript

# Where are the files?

args <- commandArgs(trailingOnly = TRUE)

# Test if there is at least one argument: if not, return an error

if (length(args) <= 1) {
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}

## Read corpus

require(tm)

corpus.fuentes <- SimpleCorpus(DirSource(
    directory = args[1]
))

fuentes.matrix <- DocumentTermMatrix(corpus.fuentes)

inspect(fuentes.matrix)

characters <- unique(gsub("_.*", "", fuentes.matrix$dimnames$Docs))
words <- fuentes.matrix$dimnames$Terms

new.fuentes <- setNames(
    data.frame(matrix(nrow = length(names), ncol = length(words) + 1)),
    c("names", words)
    )

new.fuentes$names <- names

rm("corpus.fuentes")

cat(sprintf("Counting how many times a character writes a word.\n"))

pb <- txtProgressBar(min = 0, max = length(names), style = 3)
for (i in 1:length(names)){
    new.fuentes[i,-1] <- colSums(
        as.matrix(
            fuentes.matrix[grep(names[i],
                                fuentes.matrix$dimnames$Docs), ]
            )
        )
        setTxtProgressBar(pb, value = i)
}
close(pb)

fuentes.words.topics <- read.table(
    file = args[2],
    sep = "\t",
    stringsAsFactors = FALSE
    )

cat(sprintf("Creating contingency table for characters and topics.\n"))

topics.fuentes <- setNames(data.frame(matrix(0,
    nrow = length(names),
    ncol = dim(fuentes.words.topics)[2] + 1
    )),
    c("names", paste0("topic",1:dim(fuentes.words.topics)[2]))
)

topics.fuentes$names <- names

pb <- txtProgressBar(min = 0,
    max = length(names) * prod(dim(fuentes.words.topics)),
    style = 3)
l <- 0
for (i in 1:dim(fuentes.words.topics)[2]){
    for (j in 1:dim(fuentes.words.topics)[1]) {
        for (k in 1:length(names)) {
            topics.fuentes[k,i+1] <- topics.fuentes[k,i+1] +
                new.fuentes[k, which(fuentes.words.topics[j,i] == names(new.fuentes)[-1]) + 1]
                setTxtProgressBar(pb, value = l)
        }
    }
}
close(pb)

cat(sprintf("contingency table created.\n"))
cat(sprintf("\n Running Pearson's chi-squared test.\n\n"))

summary(chisq.test(topics.fuentes[, -1]))

##########################################
# Bayesian test of homogeneity
##########################################
require(MCMCpack)
require(Matrix)

dim1 <- dim(topics.fuentes)[1]
dim2 <- dim(topics.fuentes)[-1] - 1

counts <- 0
muestra <- matrix(nrow = dim1, ncol = dim2)
tol <- 0.1

cat(sprintf("\n\nCalculating probability of being homogeneous with MCMC.\n\n"))
pb <- txtProgressBar(min = 0, max = 1e5, style = 3)
for (i in 1:1e5) {
    for(j in 1:dim1){
        muestra[j,] <- rdirichlet(n = 1,
            alpha = as.numeric(topics.fuentes[j, -1]) + 1)
    }
    projection <- matrix(
        rep(colMeans(muestra), dim1),
        nrow = dim1,
        byrow = TRUE
    )
    aux <- norm(muestra - projection, "F")
    counts[aux < tol] <- counts[aux < tol] + 1
    setTxtProgressBar(pb, value = i)
}
close(pb)

cat(
    sprintf(
        "The estimated probability of the rows being homogeneous, with tolerance %1.3f, is %1.3f.\n\n\n",
    tol,
    counts / 1e5
    )
)

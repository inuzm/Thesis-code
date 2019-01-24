#!/usr/bin/env Rscript

# Where are the files?

args <- commandArgs(trailingOnly = TRUE)

# Test if there is at least one argument: if not, return an error

if (length(args) <= 1) {
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}

## Read corpus

require(tm)

corpus.sff <- SimpleCorpus(DirSource(
    directory = args[1]
))

sff.matrix <- DocumentTermMatrix(corpus.sff)

inspect(sff.matrix)

names <- unique(
    gsub(
        "*.txt",
        "",
        gsub(
            "[(*)]",
            "",
            sff.matrix$dimnames$Docs
            )
        )
    )
words <- sff.matrix$dimnames$Terms

new.sff <- setNames(
    data.frame(matrix(nrow = length(names), ncol = length(words) + 1)),
    c("names", words)
)

new.sff$names <- names

rm("corpus.sff")

for (i in 1:length(names)){
    new.sff[i,-1] <- as.matrix(sff.matrix[i, ])
}

sff.words.topics <- read.table(
    file = args[2],
    sep = "\t",
    stringsAsFactors = FALSE
)

topics.sff <- setNames(data.frame(matrix(0,
                                            nrow = length(names),
                                            ncol = dim(sff.words.topics)[2] + 1
)),
c("names", paste0("topic",1:dim(sff.words.topics)[2]))
)

topics.sff$names <- names

l <- 0
cat(sprintf("Creating contingency table.\n"))
pb <- txtProgressBar(
    min = 0,
    max = length(names) * dim(sff.words.topics)[2] * dim(sff.words.topics)[1],
    style = 3)
for (i in 1:dim(sff.words.topics)[2]){
    for (j in 1:dim(sff.words.topics)[1]) {
        for (k in 1:length(names)) {
            topics.sff[k,i+1] <- topics.sff[k,i+1] +
                new.sff[k, which(sff.words.topics[j,i] == names(new.sff)[-1]) + 1]
            l <- l + 1
            setTxtProgressBar(pb, value = l)
        }
    }
}
close(pb)
cat(sprintf("The table is created.\n"))

##########################################
# Correct classification of the novels
genre <- factor(c(
    "sf", "sf", "f", "sf", "f", "sf", "f", "sf", "sf","f",
    "sf", "sf", "f", "f", "sf", "f", "f", "sf", "sf", "sf",
    "sf", "f", "sf", "sf", "sf", "f", "f", "sf", "sf", "sf",
    "sf", "f", "f", "f", "f", "sf", "sf", "sf", "sf", "sf",
    "sf", "f", "f", "f", "f", "f", "f", "f", "f", "f",
    "sf", "sf", "sf", "sf", "sf", "f", "sf", "f", "sf", "f",
    "sf", "sf", "sf", "f", "f", "f", "sf", "sf", "f", "f",
    "sf", "f", "f", "f", "sf", "f", "sf", "sf", "sf", "sf",
    "sf", "sf", "f", "f", "sf", "sf", "sf", "sf", "sf", "f",
    "f", "sf", "sf", "sf", "sf", "f", "sf", "sf", "sf", "sf",
    "f", "sf", "sf", "f", "sf", "f", "sf", "sf", "f", "f",
    "f", "sf", "f", "sf", "sf", "sf", "sf", "sf", "sf", "f",
    "f", "f", "f", "f", "sf", "sf", "sf", "sf", "f", "sf"
), levels = c("f","sf"), labels = 0:1)

topics.sff$genre <- genre

write.table(topics.sff, file = args[3],
            row.names = FALSE)

cat(sprintf("The table was printed in %s.\n", args[3]))

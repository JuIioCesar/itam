suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))


source("../auxiliary_functions.R")

#####################################
####tags 
from.json <- function(x) {
  line <- fromJSON(x)
  line$internalName
}

json.tags <- read.csv("../tags-catalog.json", header=F,
                      sep="\n", stringsAsFactors=F, quote="\'")

#from json to character
tags <- apply(json.tags, 1, from.json)

#cleaning tags
tags.df <- data.frame(tags=tags)
tags.df$tags <- sapply(tags.df$tags, function(x) gsub("-"," ", x=x))
tags.df$clean <- sapply(tags.df$tags, function(x) gsub("\\."," ", x=x))
tags.df$clean.unique <- sapply(tags.df$clean, function(x) eliminateDuplicated(x))
tags.unique <- unique(tags.df$clean.unique)

tags.vector.source <- VectorSource(x=tags.unique)

tags.corpus <- VCorpus(tags.vector.source, 
                       readerControl=list(language="es"))

tags.cleaned <- cleaningCorpus(tags.corpus)

tags.matrix <- TermDocumentMatrix(tags.cleaned)

tags.df.matrix <- data.frame(rownames(tags.matrix),
                             inspect(tags.matrix),
                             row.names=NULL, 
                             stringsAsFactors=F)
names(tags.df.matrix) <- c("term",paste0("doc",colnames(tags.matrix)))


save(tags.df.matrix, file="../tags.df.matrix.RData")
save(tags.df, file="../tags.df.RData")

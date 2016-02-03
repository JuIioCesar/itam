suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))

# s <- VCorpus(DirSource(directory="corpus",
#           encoding="UTF-8",
#           pattern="cnnmexico",
#           mode="text"),
#         readerControl=list(language="es"))
source("auxiliary_functions.R")

#####################################
####tags 
from.json <- function(x) {
  line <- fromJSON(x)
  line$internalName
}

json.tags <- read.csv("tags-catalog.json", header=F,
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
tags.cleaned.char <- character()
for(i in 1:length(tags.unique)){
  tags.cleaned.char <- rbind(tags.cleaned.char, content(tags.cleaned[[i]]))
}
  
unique.tags.df.cleaned <- as.character(unique(tags.cleaned.char))

tags.cleaned.source <- VectorSource(x=unique.tags.df.cleaned)

tags.cleaned.corpus <- VCorpus(tags.cleaned.source, 
                       readerControl=list(language="es"))

tags.matrix <- TermDocumentMatrix(tags.cleaned.corpus)

tags.df.matrix <- data.frame(rownames(tags.matrix),
                      inspect(tags.matrix),
                      row.names=NULL, 
                      stringsAsFactors=F)
names(tags.df.matrix) <- c("term",paste0("doc",colnames(tags.matrix)))


#######
##content
#load("file_content.RData")

###make corpus of news content
#content.vector.source <- VectorSource(x=file.content$content)

content.vector.source <- VectorSource(content)
corpus.content <- VCorpus(content.vector.source, 
             readerControl=list(language="es"))

corpus.cleaned <- cleaningCorpus(corpus.content)

#content(corpus.content)[1] obtiene el texto original que se quiere etiquetar

N <- NValue(tags.df.matrix)
#dlValues <- documentsLengths(doc.matrix)
#document lengts
document.length <- integer()
for(i in 1:N){
  document.length <- rbind(document.length, 
               length(unlist(str_split(as.character(content(tags.cleaned)[[i]]), " "))))
}
dlValues <- as.integer(document.length)
names(dlValues) <- paste0("doc", seq(1:N))

#average length in documents set
avgdl <- mean(dlValues) 

ranking <- data.frame(doc=character(),
                      rank=numeric(),
                      query=numeric())

ptm <- proc.time()
for(i in 1:length(corpus.cleaned)){
  ptm <- proc.time()
  query <- as.character(corpus.cleaned[[i]])
  
  ranks <- bm25(N=N, dlValues=dlValues, avgdl=avgdl, 
       doc.matrix=tags.df.matrix, query=query)
  print(proc.time() - ptm)
  ranks$query <- rep(i,10)
  ranking <- rbind(ranking, ranks)
}
print(proc.time() - ptm)




for(i in 1:length(rownames(ranks))){
  print(content(tags.cleaned.corpus[[rownames(ranks)[i]]]))
}


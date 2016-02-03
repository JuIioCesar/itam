suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))


load("../tags.df.matrix.RData")
source("../auxiliary_functions.R")

getTags <- function(content) {
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
  
  
  for(i in 1:length(corpus.cleaned)){
    ptm <- proc.time()
    query <- as.character(corpus.cleaned[[i]])
    
    ranks <- bm25(N=N, dlValues=dlValues, avgdl=avgdl, 
                  doc.matrix=tags.df.matrix, query=query)
    print(proc.time() - ptm)
    #ranks$query <- rep(i,10)
    ranking <- rbind(ranking, ranks)
  }
  
  suggested.tag <- character()
  for(i in 1:length(rownames(ranks))){
     suggested.tag <- rbind(suggested.tag,
                            content(tags.cleaned.corpus[[rownames(ranks)[i]]]))
  }
  
  ranking$tag <- suggested.tag
  
  #hacer match a >60% de los tags seleccionados
  ranking
}



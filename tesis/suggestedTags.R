suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))


##
getTags <- function(content) {
  load("../tags.df.matrix.RData")
  source("../auxiliary_functions.R")
  
  
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

  ranking$tag <- tags.unique[as.numeric(rownames(ranking))]
  ranking$original <- sapply(ranking$tag, function(x)
    tags.df$tag[grep(paste0("^",x,"$"), tags.df$clean.unique)[1]])
  
  
  suggested.tags <- data.frame(suggested.tag=ranking$original,
                               bm25=ranking$bm25)
  save(ranking, file="../ranking.RData")
  save(corpus.cleaned, file="../corpusCleaned.RData")
  
  suggested.tags
}


####
refinationAnalysis <- function(elements) {
  last.terms.source <- VectorSource(x=elements)
  last.terms.corpus <- VCorpus(last.terms.source, 
                               readerControl=list(language="es"))
  last.terms.cleaned <- cleaningCorpus(last.terms.corpus)
  
  terms <- sapply(last.terms.cleaned, function(x)
    str_split(content(x), " "))
  
  
  num.terms <- length(unlist(terms))
  freqs <- sapply(terms[[1]], function(x) 
    tryCatch({inspect(corpus.matrix[x,])[1,1]}, 
             error=function(err){return(0)}, 
             finally={print(x)}))
  freqs.bin <- ifelse(freqs > 0, 1,0)
  prop <- ifelse(sum(freqs.bin)/num.terms >= 0.6, 1, 0) 
  
  return(prop)
}





#once we have the suggested tags we refine the hierarchy to suggest
#if 60% of the terms in the hierarchy are on the query that hierarchy is correct
refineHierarchy <- function(content) {
  load("../ranking.RData")
  load("../corpusCleaned.RData")
  
  corpus.matrix <- TermDocumentMatrix(corpus.cleaned)

  
  hierarchies <- sapply(ranking$original, function(x) 
      str_split(x, "\\."))
  
  hierarchy.level <- rep(0, length(hierarchies))
  for(i in 1:length(hierarchies)){
    terms <- unlist(hierarchies[i])
    for(j in length(terms):1) {
      refinement <- refinationAnalysis(terms[j])
      if(refinement == 1){
          hierarchy.level[i] <- j
          j <- 1
      } 
    } 
  }
  
  cat(length(hierarchies))
  hierarchies <- hierarchies[which(hierarchy.level > 0)]
  hierarchy.level <- hierarchy.level[which(hierarchy.level > 0)]
  hierarchy.ref <- character()
  
  for(i in 1:length(hierarchy.level)) {
    hierarchy.refined <- hierarchies[[i]][1:hierarchy.level[i]]
    tag.refined <- ""
    for(j in 1:hierarchy.level[i]){
      ifelse(j == 1,
        tag.refined <- paste0(hierarchy.refined[j]),
        tag.refined <- paste0(tag.refined, ".", hierarchy.refined[j])
      )
    }
    hierarchy.ref <- rbind(hierarchy.ref, tag.refined)
  }
  
  hierarchy.ref <- unique(hierarchy.ref)
  new.hierarchy <- sapply(hierarchy.ref, function(x) grep(paste0("^",x), ranking$original))
  refined.suggests <- sapply(new.hierarchy, function(x) max(ranking$bm25[unlist(x)]))
  
  rf.sug <- data.frame(refined.tag=names(refined.suggests),
             bm25=refined.suggests)
  
  rf.sug <- rf.sug[with(rf.sug, order(-bm25)),]
  
  return(rf.sug)
}





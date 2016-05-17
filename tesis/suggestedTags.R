suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))

load("../tags.df.matrix.RData")
source("../auxiliary_functions.R")

##
getTags <- function(content) {
  content.vector.source <- VectorSource(content)
  corpus.content <- VCorpus(content.vector.source, 
                            readerControl=list(language="es"))
  
  corpus.cleaned <- cleaningCorpus(corpus.content)
  
  N <- NValue(tags.df.matrix)

  #document lengths
  document.length <- sapply(tags.cleaned, function(x)
    str_split(as.character(content(x))," ") %>% unlist() %>% length())
  
  dlValues <- document.length
  names(dlValues) <- paste0("doc", seq(1:N))
  
  #average length in documents set
  avgdl <- mean(dlValues) 
  
  # ranking <- data.frame(doc=character(),
  #                       rank=numeric(),
  #                       query=numeric())
  
  
  #for(i in 1:length(corpus.cleaned)){
    ptm <- proc.time()
    query <- as.character(corpus.cleaned[[1]])
    query <- changeAccents(query)
    
    ranks <- bm25(N=N, dlValues=dlValues, avgdl=avgdl, 
                  doc.matrix=tags.df.matrix, query=query)
    
    print(proc.time() - ptm)

    ###bm25
    ranking <- ranks$bm25
  #}

  ranking$tag <- tags.unique[as.numeric(rownames(ranking))]
  ranking$original <- sapply(ranking$tag, function(x)
    tags.df$tag[grep(paste0("^",x,"$"), tags.df$clean.unique)[1]])
  
  if(sum(ranking$bm25) >0) {
    suggested.tags <- data.frame(suggested.tag=ranking$original,
                               bm25=ranking$bm25)
  } else{
    suggested.tags <- data.frame(suggested.tag="None",
                                 bm25=0)
  }
  
  save(ranking, file="../ranking.RData")
  save(corpus.cleaned, file="../corpusCleaned.RData")
  
  ###tfidf 
  
  ranking_tfidf <- ranks$tfidf
  #}
  
  ranking_tfidf$tag <- tags.unique[as.numeric(rownames(ranking_tfidf))]
  ranking_tfidf$original <- sapply(ranking_tfidf$tag, function(x)
    tags.df$tag[grep(paste0("^",x,"$"), tags.df$clean.unique)[1]])
  
  if(sum(ranking_tfidf$tfidf) >0) {
    suggested_tags_tfidf <- data.frame(suggested_tag_tfidf=ranking_tfidf$original,
                                 tfidf=ranking_tfidf$tfidf)
  } else{
    suggested_tags_tfidf <- data.frame(suggested_tag_tfidf="None",
                                 tfidf=0)
  }
  
  save(ranking_tfidf, file="../ranking_tfidf.RData")
  
  
  suggested.tags
}


####
pruningAnalysis <- function(elements, corpus.matrix) {
  last.terms.source <- VectorSource(x=elements)
  last.terms.corpus <- VCorpus(last.terms.source, 
                               readerControl=list(language="es"))
  last.terms.cleaned <- cleaningCorpus(last.terms.corpus)
  
  terms.new <- sapply(last.terms.cleaned, function(x)
    str_split(content(x), " "))
  
  num.terms <- length(unlist(terms.new))
  freqs <- sapply(terms.new[[1]], function(x) 
    tryCatch({inspect(corpus.matrix[x,])[1,1]}, 
             error=function(err){return(0)}, 
             finally={print(x)}))
  
  freqs.bin <- ifelse(freqs > 0, 1,0)
  #if the length of the terms are even then the probability for succes should be 50 and not 60%
  if(num.terms %% 2 == 0) {
    prop <- ifelse(sum(freqs.bin)/num.terms >= 0.5, 1, 0)
  }else{
    prop <- ifelse(sum(freqs.bin)/num.terms >= 0.6, 1, 0) 
  }
  
  return(prop)
}


getHierarchiesLevels <- function(tag) {
  terms <- unlist(tag)
  for(j in length(terms):1) {
    refinement <- pruningAnalysis(terms[j], corpus.matrix)
    if(as.numeric(refinement) == 1){
      hierarchy_level <- rbind(hierarchy_level, j)
      j <- 1
    } 
  }
  
  if(length(hierarchy_level) > 0) {
    hierarchy_level[length(hierarchy_level)] 
  } else{
    0
  }
}



#once we have the suggested tags we refine their hierarchy to suggest the abstraction
#of each tag, if the lenght of the terms to verify is even then is enough that 50% of
#them make match with the content, else 60% must match to pass the best abstraction
pruneHierarchy <- function() {
  load("../ranking.RData")
  load("../ranking_tfidf.RData")
  load("../corpusCleaned.RData")
  

  corpus.matrix <- TermDocumentMatrix(corpus.cleaned)
  
  #validate that the ranking is greater than 0... it actually gave some suggestions
  if(sum(ranking$bm25) > 0) {
    hierarchies <- sapply(ranking$original, function(x) 
      unlist(str_split(x, "\\.")))
    
    hierarchy_level <- numeric()
     <- sapply(hierarchies, function(x) getHierarchiesLevels(x))
    
  
    
    hierarchies.new <- hierarchies[which(hierarchy.level > 0)]
    hierarchy.level.new <- hierarchy.level[which(hierarchy.level > 0)]
    
    hierarchy.ref <- character()
    for(i in 1:length(hierarchies.new)) {
      hierarchy.refined <- hierarchies.new[[i]][1:hierarchy.level.new[i]]
      tag.pruned <- ""
      for(j in 1:hierarchy.level.new[i]){
        ifelse(j == 1,
          tag.pruned <- paste0(hierarchy.refined[j]),
          tag.pruned <- paste0(tag.pruned, ".", hierarchy.refined[j])
        )
      }
      hierarchy.ref <- rbind(hierarchy.ref, tag.pruned)
    }
    
    hierarchy.ref <- unique(hierarchy.ref)
    new.hierarchy <- sapply(hierarchy.ref, function(x) grep(paste0("^",x), ranking$original))
    pruned.suggests <- sapply(new.hierarchy, function(x) max(ranking$bm25[unlist(x)]))
    
    rf.sug <- data.frame(pruned.tag=names(pruned.suggests),
               bm25=pruned.suggests)
    
    rf.sug <- rf.sug[with(rf.sug, order(-bm25)),]
  } else{
    rf.sug <- data.frame(pruned.tag="None",
                         bm25=0)
  }
  
  return(rf.sug)
}




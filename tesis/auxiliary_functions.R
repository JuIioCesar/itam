suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(SnowballC))

##eliminate duplicated strings within the same query string
eliminateDuplicated <- function(x) {
  aux <- unlist(str_split(x, " "))
  if(sum(duplicated(aux)) > 0) {
    y <- aux[-which(duplicated(aux))]
  } else {
    y <- aux
  }
  
  query <- character()
  for(i in 1:length(y)) {
    if(i == 1){
      query <- paste0(query, y[i])
    } else {
      query <- paste(query, y[i], sep=" ") 
    }
  }
  
  query
}


##cleaning corpus to build a term document matrix
cleaningCorpus <- function(volatil.corpus){
  #eliminate punctuation
  without_punctuation <- tm_map(volatil.corpus, removePunctuation)
  #eliminate numbers
  without_numbers <- tm_map(without_punctuation, removeNumbers)
  #eliminate stopwords 
  without_stopwords <- tm_map(without_numbers, removeWords, stopwords("spanish"))
  #eliminate extra whitespaces 
  extra_whitespace <- tm_map(without_stopwords, stripWhitespace)
  #to lower case
  s_lower <- tm_map(extra_whitespace, content_transformer(tolower))
  #stem each token
  s_steam <- tm_map(s_lower, stemDocument)

  s_steam
}


###avg length of docs
avgdlValue <- function(doc.matrix){
##get the count of zeros on each doc (with the use of table an looking for label 0)
#   ptm <- proc.time()
#   lengths <- apply(doc.matrix[,2:dim(doc.matrix)[2]], 2, function(x) 
#   dim(doc.matrix)[1] - table(x)[grep("^0$", names(table(x)))])
#   
#   mean(lengths)
#   print(proc.time() - ptm)
  
  all.docs <- doc.matrix[,2:dim(doc.matrix)[2]]
  
  binary.all.docs <- as.data.frame(sapply(all.docs, function(x) ifelse(x>0,1,0)))
  lengths <- sapply(binary.all.docs, function(x) sum(x))

  mean(lengths)
}


#the length of a specific document
documentsLengths <- function(doc.matrix) {
#   ptm <- proc.time()
#   doc <- doc.matrix[grep(paste0("^",document,"$"), names(doc.matrix))]
#   length <- dim(doc.matrix)[1] - table(doc)[grep("^0$",names(table(doc)))]
#   print(proc.time()-ptm)
#   
#   length
  
  all.docs <- doc.matrix[,2:dim(doc.matrix)[2]]
  
  binary.all.docs <- as.data.frame(sapply(all.docs, function(x) ifelse(x>0,1,0)))
  lengths <- sapply(binary.all.docs, function(x) sum(x))
  
  lengths
}


#K value K=k1((1-b) +b *dl/avgdl)
KValues <- function(k1, b, dlValues, avgdl){
  sapply(dlValues, function(x) (k1*((1-b) + (b * x/avgdl))))
}

#get the frequency of the term within the query
qfValue <- function(term, query){
  freq <- table(str_split(query, " "))
  qf <- freq[grep(term,names(freq))]
  
  qf
}

#get the total number of documents in the set
NValue <- function(doc.matrix){
  N <- dim(doc.matrix)[2] -1 
  
  N
}

#frequency of the term in all documents
nValue <- function(doc.matrix, term){
  docs <- 0
  if(length(grep(paste0("^", term, "$"), doc.matrix$term)) > 0) {
    freq.all.docs <- doc.matrix[grep(paste0("^", term, "$"), doc.matrix$term),]
    freq.all.docs <- select(freq.all.docs, -term)
    gathered <- gather(freq.all.docs, doc, freq)
    docs <- dim(freq.all.docs)[2] - 
      table(gathered$freq)[grep("^0$", names(table(gathered$freq)))]
  }
  
  docs
  
  
  sum(ifelse(gather(doc.matrix) > 0,1,0))
}



#get the frequency of the term in the specific document
fValue <- function(document, doc.matrix, term){
  if(length(grep(paste0("^", term, "$"), doc.matrix$term)) > 0) { 
    n <- doc.matrix[grep(paste0("^", term, "$"), doc.matrix$term), eval(document)]
  } else {
    n <- 0
  }
  
  n
}


##first term of the bm25 formula 
t1Value <- function(n, r, R, N){
  t1_num <- (r+0.5)*(N - n - R + r +0.5)
  t1_denom <- (n - r +0.5)*(R - r +0.5)
  t1 <- log(t1_num/t1_denom)
  
  t1
}



##second term of the bm25 formula ((k1+1)*f)/(k1+f)
t2Value <- function(term, doc.matrix, K, avgdl, dlValues, k1){
  if(length(grep(paste0("^", term, "$"), doc.matrix$term)) > 0) {
    frequencies <- doc.matrix[grep(paste0("^",term,"$"), doc.matrix$term),]
    docs <- gather(frequencies, doc, freq, - term)
    
    docs$K <- K
    docs$dl <- dlValues
    
    t2s <- ifelse(docs$freq > 0, ((k1 +1)*docs$freq)/(docs$K + docs$freq) ,0)
  } else {
    t2s <- rep(0, (dim(doc.matrix)[2]-1))
  }
}


#third term of the bm25 formula  ((k2 +1)*qf)/(k2 + qf)
t3Value <- function(term, qf, k2) {
  qf.value <- qf[grep(paste0("^",term,"$"), names(qf))]
  t3 <- ((k2 + 1)*qf.value)/(k2 + qf.value)
    
  t3
}




########
####bm25 
bm25 <- function(r=0, R=0, k1=1.2, k2=100, b=0.75, 
                 N, dlValues, avgdl, doc.matrix, query, 
                 nValues){
  terms <- unique(unlist(str_split(query, " ")))
  
  nValues <- sapply(terms, function(x) 
    sum(ifelse(gather(doc.matrix[grep(
      paste0("^",x,"$"), doc.matrix$term),2:N])$value >0, 1,0)))
  #names(nValues) <- terms
  
  
  #t1 values
  t1.values <- sapply(nValues, function(x) t1Value(x, r, R, N))
  t1.values.df <- data.frame(
    #doc=paste0("doc",seq(1:N)),
    sapply(t1.values, function(x) rep(x, N)))
  
  Ks <- KValues(k1, b, dlValues, avgdl)
  
  #t2 values
  t2.values <- as.data.frame(sapply(terms, function(x) 
    t2Value(x, doc.matrix, Ks, avgdl, dlValues, k1)))
  
  qf.values <- unlist(table(str_split(query, " ")))
  
  #t3 values
  t3.values <- sapply(terms, function(x)
    t3Value(x, qf.values, k2)) 
  names(t3.values) <- terms
  t3.values.df <- data.frame(
    #doc=paste0("doc",seq(1:N)),
    sapply(t3.values, function(x) rep(x, N)))
  
  bms <- data.frame(doc=paste0("doc", seq(1:N)))
  for(i in 1:length(terms)){
     bms <- cbind(bms, t1.values.df[,i]*t2.values[,i]*t3.values.df[,i]) 
  }
  names(bms) <- c("doc", terms)

  bm25s <- data.frame(doc=paste0("doc", seq(1:N)),
                      bm25=rep(0, N))
  bm25s$bm25 <- apply(bms[,2:length(terms)], 1, function(x) sum(x))
  bm25s <- bm25s[with(bm25s, order(-bm25)),]
  
  bm25s[1:10,]
}


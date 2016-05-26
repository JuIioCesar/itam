library(dplyr)
library(RPostgreSQL)

source("../suggestedTags.R")
source("../tags_manipulation.R")

my_db <- src_postgres(dbname="itamtesis", host="localhost", user="liliana.millan")
noticias <- tbl(my_db, "noticias")

noticias_df <- collect(noticias)
noticias_df$marca <- ""
noticias_df[grep("financiero", noticias_df$origen), "marca"] <- "financiero"
noticias_df[grep("nyt", noticias_df$origen), "marca"] <- "nyt"
              
# s <- group_by(noticias_df, marca) %>%
#   summarise(n=n_distinct(title))

noticias_unique <- distinct(noticias_df, title)
  
content_df <- paste(noticias_unique$title, noticias_unique$description, 
                    noticias_unique$content, sep=" ")

#### digest
getRecommendations <- function(content, final_df){
  getTags(content)
  pruneHierarchy()
  
  load("../pruned_bm25.RData")
  load("../pruned_tfidf.RData")
  
  # top_suggestions_bm25 <- list(rf.sug[1:5,"pruned.tag"])
  # top_suggestions_tfidf <- list(rf_sug_tfidf[1:5,"pruned.tag"])
  
  s <- data.frame(contet=content, tag_bm25=rf.sug[1:5,"pruned.tag"],
                  bm25=rf.sug[1:5,"bm25"],
                  tags_tfidf=rf_sug_tfidf[1:5,"pruned.tag"],
                  tfidf=rf_sug_tfidf[1:5,"tfidf"])

  
  conn <- dbConnect("PostgreSQL", dbname="itamtesis", host="localhost" )
  dbWriteTable(conn, value=s, name="clasificaciones", append=T, row.names=F)
  dbDisconnect(conn)
}

pmd <- proc.time()
results <- sapply(content_df, function(x) getRecommendations(x))
proc.time() - pmd





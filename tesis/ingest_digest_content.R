library(dplyr)

source("../suggestedTags.R")

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


recommendations <- sapply(content_df, function(x) getTags(x))
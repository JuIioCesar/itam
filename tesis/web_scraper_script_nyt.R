library(RCurl)
library(XML)
library(lubridate)
library(dplyr)
library(RPostgreSQL)

###getting the actual news
getNews <- function(x){
  url_x <- getURL(x)
  txt_x <- htmlParse(url_x)
  
  node_property <- xpathApply(txt_x, "//meta", xmlGetAttr, "property")
  #description
  description_node <- grep("og:description", node_property)
  description <- xpathApply(txt_x,"//meta", xmlGetAttr, "content")[[description_node]]
  
  #title
  title_node <- grep("og:title", node_property)
  title <- xpathApply(txt_x,"//meta", xmlGetAttr, "content")[[title_node]]
  
  content_html_style <- xpathSApply(txt_x, "//div/p", xmlValue)
  for(i in 1:length(content_html_style)){
    if(i == 1){
      content <- content_html_style[i]
    } else {
      content <- paste(content, content_html_style[i])
    }
  }
  
  row.info <- c(x, title, description, content)
  
  row.info
}


extractNews <- function(specific_url){
  row.data <- getNews(specific_url)
  info <- c(row.data[1], row.data[2], row.data[3], row.data[4])
  
  return(info)
}



getUrls <- function(base_url) {
  url <- getURL(base_url)
  txt <- htmlParse(url)
  links <- unique(getHTMLLinks(txt))
  found <- links[grep("/2016/", links)]
  
  return(found)
}
##########

urls_look <- c("http://www.nytimes.com/es/collection/editorial/",
          "http://www.nytimes.com/es/collection/salud/",
          "http://www.nytimes.com/es/collection/internacional/",
          "http://www.nytimes.com/es/collection/tecnologia/",
          "http://www.nytimes.com/es/collection/america-latina/",
          "http://www.nytimes.com/es/collection/europa/",
          "http://www.nytimes.com/es/collection/estados-unidos/")

all_urls <- sapply(urls_look, function(x) getUrls(x)) %>% unlist()


contents.df <- sapply(all_urls, function(x) extractNews(x)) %>% t() %>%
  as.data.frame(row.names=NULL)
names(contents.df) <-  c("url","title","description","content")

day.time <- Sys.time()
label.day.time <- paste(year(day.time),
                        ifelse(month(day.time) < 10,
                               paste0("0", month(day.time)),
                               month(day.time)),
                        ifelse(day(day.time) < 10,
                               paste0("0", day(day.time)),
                               day(day.time)),
                        ifelse(hour(day.time) < 10,
                               paste0("0", hour(day.time)),
                               hour(day.time)), 
                        ifelse(minute(day.time) < 10,
                               paste0("0", minute(day.time)),
                               minute(day.time)), 
                        sep="_")


# write.csv(contents.df, file=paste0("noticias/",label.day.time,"_nyt"),
#           row.names=F)

contents.df$fecha <- Sys.Date()
contents.df$origen <- paste(label.day.time, "nyt", sep="_")

contents.df$seccion <- ""
contents.df$seccion[grep("/salud/", names(contents.df$url))] <- "salud"
contents.df$seccion[grep("/editorial/", names(contents.df$url))] <- "editorial"
contents.df$seccion[grep("/internacional/", names(contents.df$url))] <- "internacional"
contents.df$seccion[grep("/tecnologia/", names(contents.df$url))] <- "tecnologia"
contents.df$seccion[grep("/america-latina/", names(contents.df$url))] <- "america_latina"
contents.df$seccion[grep("/europa/", names(contents.df$url))] <- "europa"
contents.df$seccion[grep("/estados-unidos/", names(contents.df$url))] <- "estados_unidos"




s <- select(contents.df, fecha, url, title, description, content, origen, seccion)

### to postgres
conn <- dbConnect("PostgreSQL", dbname="itamtesis", host="localhost" )
#my_db <- src_postgres(dbname="itamtesis", host="localhost", user="liliana.millan")
dbWriteTable(conn, value=s, name="noticias", append=T, row.names=F)


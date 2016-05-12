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
  description_node <- grep("description", node_property)
  description <- xpathApply(txt_x,"//meta", xmlGetAttr, "content")[[description_node]]
  
  #title
  title_node <- grep("title", node_property)
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

##########

base_url <- "http://www.elfinanciero.com.mx"
urls_look <- c("/nacional/","/mundo/","/financial-times/","/tech/")

url <- getURL(base_url)
txt <- htmlParse(url)
links <- unique(getHTMLLinks(txt))
found <- links[grep(paste(urls_look, collapse="|"), links)]
found_urls  <- paste0(base_url, found)

contents.df <- sapply(found_urls, function(x) extractNews(x)) %>% t() %>% 
  as.data.frame(row.names=NULL)
names(contents.df) <- c("url","title","description","content")


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


#write.csv(contents.df, file=paste0("noticias/",label.day.time,"_financiero"),
#          row.names=F)

#financiero <- read_csv("/Users/liliana.millan/Documents/itam/itam/tesis/noticias/2016_04_30_23_51_financiero")
#financiero <- as.data.frame(financiero)
contents.df$fecha <- Sys.Date()
contents.df$origen <- paste(label.day.time, "financiero", sep="_")

s <- select(contents.df, fecha, url, title, description, content, origen)

### to postgres
conn <- dbConnect("PostgreSQL", dbname="itamtesis", host="localhost" )
#my_db <- src_postgres(dbname="itamtesis", host="localhost", user="liliana.millan")
dbWriteTable(conn, value=s, name="noticias", append=T, row.names=F)



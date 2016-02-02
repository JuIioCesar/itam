library(RCurl)
library(XML)
library(lubridate)

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

##########


root_url <- "http://www.elfinanciero.com.mx"
url <- getURL("http://www.elfinanciero.com.mx/nacional")
txt <- htmlParse(url)
links <- unique(getHTMLLinks(txt))
national_news <- links[grep("^/nacional/", links)]

national_urls <- paste0(root_url, national_news)

urls <- character()
titles <- character()
descriptions <- character()
contents <- character()

for(i in 1:length(national_urls)){
  row.data <- getNews(national_urls[i])
  urls <- rbind(urls, row.data[1])
  titles <- rbind(titles, row.data[2])
  descriptions <- rbind(descriptions, row.data[3])
  contents <- rbind(contents, row.data[4])
}

contents.df <- data.frame(url=urls, 
                          title=titles,
                          description=descriptions,
                          content=contents)

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


write.csv(contents.df, file=paste0("financiero/nacional/",label.day.time),
          row.names=F)



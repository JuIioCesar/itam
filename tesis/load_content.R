library(lubridate)

daytime <- Sys.Date()
daytime.pattern <- paste(year(daytime),
                     ifelse(month(daytime) < 10, 
                            paste0("0", month(daytime)),
                            month(daytime)),
                     ifelse(day(daytime) < 10,
                            paste0("0", day(daytime)),
                            day(daytime)),
                     sep="_")

#read content 
today.files <- list.files("financiero/nacional", pattern=daytime.pattern)

for(i in 1:length(today.files){
  file.content <- read.csv(paste0("financiero/nacional/", today.files[i]), 
                           header=T)
}

save(file.content, file="file_content.RData")
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(glue)

setwd('D:/Cruise/raw_data/met_data')
Sys.setenv(TZ = 'UTC')

files = list.files(pattern = "mvpos",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  
  datList[[index]] = read.csv(files[index],header=TRUE, na.strings= c('NA','missing')) %>%
    tibble()
  
}

mvpos = bind_rows(datList) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date,"1 sec")) %>% 
  select(-c(X))


files = list.files(pattern = "surfm",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  
  datList[[index]] = read.csv(files[index],header=TRUE, na.strings= c('NA','missing')) %>%
    tibble()
  
}

surfm = bind_rows(datList) %>% 
  mutate(date = glue("{date} {time}"),
         date = dmy_hms(date),
         date = round_date(date,"1 sec")) %>% 
  select(-c(X,time))


met_data = left_join(mvpos,surfm,by = "date")

write.csv(met_data,"D:/Cruise/raw_data/met_data.csv",row.names = FALSE)

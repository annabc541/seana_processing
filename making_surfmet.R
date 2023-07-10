library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)

setwd('D:/Cruise/raw_data/met_data/Surfmet')
Sys.setenv(TZ = 'UTC')

files = list.files("5",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  
  datList[[index]] = read.csv(files[index],header=FALSE, na.strings= c('NA','missing')) %>%
    tibble()
  
}

surfm = bind_rows(datList) %>% 
  rename(date = V2,
         time = V3,
         rel_ws = V11,
         rel_wd = V12,
         air_temp = V13,
         air_humidity = V14) %>% 
  select(c(date,time,rel_ws,rel_wd,air_temp,air_humidity))

write.csv(surfm,'D:/Cruise/raw_data/met_data/surfm5.csv',row.names = FALSE)

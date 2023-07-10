library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)

#ok so I think this code is for pulling together all the individual surfmet and mvpos data, which I had to 
#separate into 5 folders due to my laptop's inability to process such large files
#what is left here is just me pulling together the last set of surfmet files
#will rename and move to archive, I don't think I will need to do this again since this data will not change

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

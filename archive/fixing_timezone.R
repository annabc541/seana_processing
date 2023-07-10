library(dplyr)
library(openair)
library(lubridate)
library(tidyr)
library(ggplot2)
library(zoo)
library(viridis)

#just fixing timezone so it is all in UTC

setwd('D:/Cruise/raw_data')
Sys.setenv(TZ = 'UTC')

# Importing data ------------------------------------------------------------

#Importing data recorded in BST and adjusting it to UTC

files_BST = list.files('BST', full.names=TRUE)
datList_BST = list()
hrs = 1 * 60 *60
for(index in 1:length(files_BST)) {
  
  datList_BST[[index]] = read.csv(files_BST[index],header=TRUE, na.strings= c('NA','missing')) %>%
    tibble() %>% 
    mutate(TheTime = mdy_hms(TheTime)) %>%
    rename(date = TheTime)
  
}

BST = bind_rows(datList_BST) %>%
  mutate(date = date - hrs)

#Importing files after changing laptop to UTC
files_UTC = list.files('UTC', full.names=TRUE)
datList_UTC = list()
for(index in 1:length(files_UTC)) {
  
  datList_UTC[[index]] = read.csv(files_UTC[index],header=TRUE, na.strings= c('NA','missing'))%>%
    tibble() %>% 
    mutate(TheTime = mdy_hms(TheTime)) %>%
    rename(date = TheTime)
  
}
UTC = bind_rows(datList_UTC)

#Binding both BST and UTC dataframes and tidying raw dataframe
raw_data <- rbind(BST, UTC) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty() %>% 
  arrange(date) %>%
  rename(so2 = t101_022639_so2,
         co = al5002_inst_co_conc,
         noy = t200up_021842_no,
         no = t200up_023828_no,
         no2 = t200up_023828_no2)

write.csv(raw_data,"raw_data.csv",row.names = FALSE)
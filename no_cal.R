library(dplyr)
library(openair)
library(lubridate)
library(tidyr)
library(ggplot2)
library(zoo)
library(viridis)

setwd('D:/Cruise/processed_data')
Sys.setenv(TZ = 'UTC')

# Functions ---------------------------------------------------------------

tidy_rle = function(rleObj){
  
  require(dplyr)
  
  rleDf = tibble(lengths = rleObj$lengths,
                 values = rleObj$values) %>% 
    mutate(idxStart = cumsum(c(1,lengths))[1:(nrow(.))],
           idxEnd = cumsum(lengths))
  
  #
  rleDf
}


# Code --------------------------------------------------------------------

#need to edit some of the calibration timings

dat = read.csv("SEANA_data.csv") %>% 
  mutate(date = ymd_hms(date),
         nox_cal = ifelse(date < '2022-06-15 10:31' & date > '2022-06-15 10:00',0,nox_cal),
         nox_cal = ifelse(date > '2022-05-23 09:37' & date < '2022-05-23 09:44',0,nox_cal),
         nox_cal = ifelse(date > '2022-06-15 10:44' & date < '2022-06-15 11:40',0,nox_cal),
         nox_cal = ifelse(zero_box != 0,0,nox_cal))

calibrating = rle(dat$nox_cal) %>% 
  tidy_rle() %>%
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble()

dat2 = dat %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(calibrating, "idx") %>% 
  select(-idx) %>% 
  mutate(id = ifelse(is.na(id), 0, id)) %>% 
  group_by(id) %>% 
  mutate(idx = 1:n(),
         no = ifelse(idx < 7, NA, no),
         idx = n():1,
         no = ifelse(idx < 3, NA, no)) %>%
  filter(nox_cal == 1) %>% 
  select(date,no,id,nox_cal,zero_box) %>% 
  summarise(no = mean(no,na.rm =T))

ggplot(dat2,aes(id,no)) +
  geom_point()

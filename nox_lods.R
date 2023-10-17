library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(janitor)
library(openair)
library(ggplot2)

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

# Code currently in use ------------------------------------------------------------

#importing data

raw_data = read.csv("data/measured_data.csv") %>% 
  mutate(date = ymd_hms(date),
         zero_box = ifelse(is.na(zero_box),0,zero_box),
         nox_cal = case_when(date > '2022-05-23 09:22' & date < '2022-05-23 09:44' ~ 1,
                             date > '2022-06-03 09:02' & date < '2022-06-03 09:18' ~ 1,
                             date > '2022-06-15 10:00' & date < '2022-06-15 11:40' ~ 1,
                             date > '2022-06-19 14:08' & date < '2022-06-19 14:42' ~ 1,
                             date > '2022-06-22 09:33' & date < '2022-06-22 09:44' ~ 1,
                             TRUE ~ 0)) %>% 
  filter(nox_cal == 0) %>% 
  select(date,no,no2,zero_box,station)

#identifying rows when zeroing and id-ing each zero

zeroing = rle(raw_data$zero_box) %>% 
  tidy_rle() %>%
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble()

#getting length of each zero (> 60 indicates a longer zero)

zero_len = zeroing %>% group_by(id) %>% summarise(n = n())

#calculating 2sd for each longer zero, then dividing by the sqrt of the number of measurements
#6 when averaging to a minute (6 10 sec measurements in each minute)

sd = raw_data %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zeroing, "idx") %>% 
  select(-idx) %>% 
  filter(id == 4 | id == 16 | id == 28 | id == 37) %>% 
  group_by(id) %>% 
  summarise(no_sd = 2 * sd(no),
            no2_sd = 2 * sd(no2)) %>% 
  mutate(nox_sd = (no_sd^2+no2_sd^2)^0.5,
         no_lod = no_sd/6^0.5,
         no2_lod = no2_sd/6^0.5,
         nox_lod = nox_sd/6^0.5)

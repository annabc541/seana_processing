library(dplyr)
library(openair)
library(lubridate)
library(tidyr)
library(ggplot2)
library(zoo)
library(viridis)

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
                             TRUE ~ 0),
         station = case_when(date < '2022-05-20 20:15' ~ 'Reykjavik',
                             between(date,as.POSIXct('2022-05-20 20:15:00'),as.POSIXct('2022-05-23 06:15:00')) ~ 'Sailing 1',
                             between(date,as.POSIXct('2022-05-23 06:15:00'),as.POSIXct('2022-05-26 00:00')) ~ 'East Greenland',
                             between(date,as.POSIXct('2022-05-26 00:00'),as.POSIXct('2022-05-28 12:00')) ~ 'Sailing 2',
                             between(date,as.POSIXct('2022-05-28 12:00'),as.POSIXct('2022-05-31 08:00')) ~ 'Nuuk',
                             between(date,as.POSIXct('2022-05-31 08:00'),as.POSIXct('2022-06-01 08:00')) ~ 'Sailing 3',
                             between(date,as.POSIXct('2022-06-01 08:00'),as.POSIXct('2022-06-03 07:00')) ~ 'Maniitsoq 1',
                             between(date,as.POSIXct('2022-06-03 07:00'),as.POSIXct('2022-06-03 20:00')) ~ 'Sailing 4',
                             between(date,as.POSIXct('2022-06-03 20:00'),as.POSIXct('2022-06-04 20:00')) ~ 'Sisimiut',
                             between(date,as.POSIXct('2022-06-04 20:00'),as.POSIXct('2022-06-05 11:00')) ~ 'Sailing 5',
                             between(date,as.POSIXct('2022-06-05 11:00'),as.POSIXct('2022-06-06 10:30')) ~ 'Disko Bay',
                             between(date,as.POSIXct('2022-06-06 10:30'),as.POSIXct('2022-06-06 18:30')) ~ 'Sailing 6',
                             between(date,as.POSIXct('2022-06-06 18:30'),as.POSIXct('2022-06-12 23:55')) ~ 'Sea ice',
                             between(date,as.POSIXct('2022-06-12 23:55'),as.POSIXct('2022-06-13 21:00')) ~ 'Sailing 7',
                             between(date,as.POSIXct('2022-06-13 21:00'),as.POSIXct('2022-06-16 09:00')) ~ 'Maniitsoq 2',
                             date > '2022-06-16 09:00' ~ 'Sailing home')) %>% 
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

#joining identified zero dataframe

dat = raw_data %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zeroing, "idx") %>% 
  select(-idx) %>% 
  mutate(id = ifelse(is.na(id), 0, id))

#visualising

dat %>% 
  filter(zero_box == 1,
         station != "Sailing home",
         station != "Reykjavik",
         id != 12,
         id != 2,
         id != 25,
         id != 15,
         id != 3,
         id != 16,
         id != 7) %>%
  # mutate(no_med = ifelse(no < no_m + 2*no_sd & no > no_m - 2*no_sd,"yes","no")) %>% 
  # filter(no_med == "yes") %>% 
  pivot_longer(c(no,no2)) %>%
  ggplot(aes(date,value,col = id)) +
  facet_grid(rows = vars(name)) +
  geom_point()

#mean of no and no2 zeroes per station, filtering out odd cycles

zeroes = dat %>% 
  filter(zero_box == 1,
         station != "Sailing home",
         station != "Reykjavik",
         id != 12,
         id != 2,
         id != 25,
         id != 15,
         id != 3,
         id != 16,
         id != 7) %>%
  group_by(station) %>% 
  summarise(no = mean(no),
            no2 = mean(no2)) %>% 
  ungroup()

#below code is not currently in use but may be used in the future
#lord knows I have changed how I do the zeroes about a dozen times

# Flagging dataframe ------------------------------------------------------

raw_data1 = dat %>% 
  mutate(flag = case_when(date < '2022-05-18 20:21' ~ 1,
                          date > '2022-05-20 16:15' & date < '2022-05-20 18:15' ~ 1,
                          date > '2022-05-21 00:00' & date < '2022-05-22 06:00' ~ 1,
                          date > '2022-06-11 13:33' & date < '2022-06-11 13:37' ~ 1,
                          date > '2022-06-11 12:40' & date < '2022-06-11 12:50' ~ 1,
                          date > '2022-06-20 09:58' & date < '2022-06-20 10:17' ~ 1,
                          date > '2022-05-23 09:22' & date < '2022-05-23 09:44' ~ 1,
                          date > '2022-06-03 09:02' & date < '2022-06-03 09:18' ~ 1,
                          date > '2022-06-15 10:12' & date < '2022-06-15 11:00' ~ 1,
                          date > '2022-06-19 14:08' & date < '2022-06-19 14:42' ~ 1,
                          date > '2022-06-22 09:33' & date < '2022-06-22 09:44' ~ 1,
                          date > '2022-06-12 23:55' & date < '2022-06-13 21:00' ~ 1,
                          TRUE ~ 0),
         station = case_when(date < '2022-05-20 20:15' ~ 'Reykjavik',
                             between(date,as.POSIXct('2022-05-20 20:15:00'),as.POSIXct('2022-05-23 06:15:00')) ~ 'Sailing 1',
                             between(date,as.POSIXct('2022-05-23 06:15:00'),as.POSIXct('2022-05-26 00:00')) ~ 'East Greenland',
                             between(date,as.POSIXct('2022-05-26 00:00'),as.POSIXct('2022-05-28 12:00')) ~ 'Sailing 2',
                             between(date,as.POSIXct('2022-05-28 12:00'),as.POSIXct('2022-05-31 08:00')) ~ 'Nuuk',
                             between(date,as.POSIXct('2022-05-31 08:00'),as.POSIXct('2022-06-01 08:00')) ~ 'Sailing 3',
                             between(date,as.POSIXct('2022-06-01 08:00'),as.POSIXct('2022-06-03 07:00')) ~ 'Maniitsoq 1',
                             between(date,as.POSIXct('2022-06-03 07:00'),as.POSIXct('2022-06-03 20:00')) ~ 'Sailing 4',
                             between(date,as.POSIXct('2022-06-03 20:00'),as.POSIXct('2022-06-04 20:00')) ~ 'Sisimiut',
                             between(date,as.POSIXct('2022-06-04 20:00'),as.POSIXct('2022-06-05 11:00')) ~ 'Sailing 5',
                             between(date,as.POSIXct('2022-06-05 11:00'),as.POSIXct('2022-06-06 10:30')) ~ 'Disko Bay',
                             between(date,as.POSIXct('2022-06-06 10:30'),as.POSIXct('2022-06-06 18:30')) ~ 'Sailing 6',
                             between(date,as.POSIXct('2022-06-06 18:30'),as.POSIXct('2022-06-12 23:55')) ~ 'Sea ice',
                             between(date,as.POSIXct('2022-06-12 23:55'),as.POSIXct('2022-06-13 21:00')) ~ 'Sailing 7',
                             between(date,as.POSIXct('2022-06-13 21:00'),as.POSIXct('2022-06-16 09:00')) ~ 'Maniitsoq 2',
                             date > '2022-06-16 09:00' ~ 'Sailing home'))

# Applying NOx zeroes ---------------------------------------------------------

#identifying rows when zeroing and id-ing each zero

zeroing = rle(raw_data$zero_box) %>% 
  tidy_rle() %>%
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble()

#joining identified zero dataframe and calculating median of each zero

dat = raw_data %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zeroing, "idx") %>% 
  select(-idx) %>% 
  mutate(id = ifelse(is.na(id), 0, id)) %>% 
  group_by(station) %>% 
  group_by(id) %>% 
  mutate(idx = 1 :n(),
         no = ifelse(idx < 50, NA, no)) %>% 
  # filter(date < "2022-06-16"
         # ) %>%
  nest_by() %>%  
  mutate(zero_mean_no = ifelse(id == 0 | id == 14 | id == 1, NA,mean(data$no, na.rm = T))) %>% 
  unnest(data) %>%
  arrange(date) %>% 
  ungroup() %>% 
  mutate(zero_no = na.approx(zero_mean_no,na.rm = F),
         no_corr = no - zero_no)


  # group_by(station) %>% 
  # nest_by() %>% 
  # mutate(zero_no = median(data$no, na.rm = T),
  #        zero_no2 = median(data$no2, na.rm = T)) %>% 
  # ungroup() %>%
  # unnest(cols = c(data)) %>% 
  # arrange(date) %>% 
  # select(c(zero_no,zero_no2,idx))


dat2 = raw_data2 %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(dat, "idx") %>% 
  mutate(zero_no = na.approx(mean_no,na.rm = F),
         zero_no_med = na.approx(med_no,na.rm = F)
         # zero_no2 = na.approx(zero_no2,na.rm = F)
         )

dat2 %>% 
  # filter(id == 27) %>% 
  pivot_longer(c(zero_no,zero_no2)) %>% 
  ggplot(aes(date,value,col = station)) +
  geom_point() +
  theme_bw() +
  facet_grid(rows = vars(name),scales = "free_y")
  
corr_dat %>%
  mutate(id = as.character(id)) %>% 
  # filter(
  #   zero_box == 1
  #        ) %>%
  pivot_longer(c(zero_no,zero_no2)) %>% 
  ggplot(aes(date,value,col = id)) +
  geom_point() +
  theme_bw() +
  facet_grid(rows = vars(name),scales = "free_y") +
  # scale_colour_manual(values = viridis(10)) +
  # scale_x_datetime(date_labels = "%Y-%m-%d %H:%M") +
  NULL



raw_data2 = dat %>% 
  filter(flag_all == 0,
         nox_flag == 0,
         nox_cal == 0,
         poll == 0) %>% 
  mutate(zero_box = ifelse(is.na(zero_box),0,zero_box)) %>%  
  select(date,no,no2,zero_box,station)
#identifying rows when zeroing and id-ing each zero
zero_nox = rle(raw_data2$zero_box) %>% 
  tidy_rle() %>%
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble()
#joining identified zero dataframe and calculating median of each zero
corr_dat = raw_data2 %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zero_nox, "idx") %>% 
  mutate(id = ifelse(is.na(id), 0, id)) %>% 
  filter(id != 0) %>% 
  group_by(station) %>% 
  nest_by() %>% 
  mutate(zero_no = median(data$no, na.rm = T),
         zero_no2 = median(data$no2, na.rm = T)) %>% 
  ungroup() %>%
  unnest(cols = c(data)) %>% 
  arrange(date) %>% 
  select(c(zero_no,zero_no2,idx))


dat2 = dat %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(corr_dat, "idx") %>% 
  mutate(zero_no = na.approx(zero_no,na.rm = F),
         zero_no2 = na.approx(zero_no2,na.rm = F),
         corr_no = no - zero_no,
         corr_no2 = no2 - zero_no2) %>% 
  select(c(date,so2,noy,corr_no,corr_no2,o3,zero_box)) %>% 
  rename(no = corr_no,
         no2 = corr_no2) %>% 
  timeAverage(avg.time = '1 min')
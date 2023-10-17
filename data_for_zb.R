library(lubridate)
library(zoo)
library(janitor)
library(openair)
library(tidyverse)

Sys.setenv(TZ = 'UTC')

#creating a minute dataset for ZB

# Read in data ------------------------------------------------------------

# Reading in NOx, NOy, O3 and SO2 data
raw_dat = read.csv("data/measured_data.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  mutate(date = round_date(date, "1 sec")) %>% 
  select(c(date,so2,noy,no,no2,zero_box,so2_zero_channel,o3))

# Reading in CO data
co_dat = read.csv('data/co_data.csv') %>%
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  filter(date < '2022-06-22 10:13') %>% 
  select(c(date,co = co_calibrated))

# Reading in met data
met_data = read.csv('data/met_data_processed.csv') %>% 
  mutate(date = ymd_hms(date))

# Wind data ------------------------------------------------------

#averaging true and rel wind using openair's vectorised wind calculation
true_wind = met_data[,c("date","lat","long","speed","heading","air_temp","air_humidity","true_wd","ws","u","v")]
rel_wind = met_data[,c('date','rel_ws','rel_wd')]

true_wind = true_wind %>% 
  rename(wd = true_wd) %>% 
  timeAverage(avg.time = '1 min',vector.ws = TRUE)

rel_wind = rel_wind %>% 
  rename(wd = rel_wd,
         ws = rel_ws) %>% 
  timeAverage(avg.time = '1 min',vector.ws = TRUE) %>% 
  rename(rel_wd = wd,
         rel_ws = ws)

# NOx zeroes --------------------------------------------------------------

raw_dat_corr = raw_dat %>% 
  mutate(station = case_when(date < '2022-05-20 20:15' ~ 'Reykjavik',
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
                             date > '2022-06-16 09:00' ~ 'Sailing home'),
         no_corr = case_when(station == "Disko Bay" ~ no - -0.029,
                             station == "East Greenland" ~ no - -0.050,
                             station == "Maniitsoq 1" ~ no - -0.028,
                             station == "Maniitsoq 2" ~ no - -0.021,
                             station == "Nuuk" ~ no - -0.044,
                             station == "Reykjavik" ~ no - - 0.50,
                             station == "Sailing 1" ~ no - -0.050,
                             station == "Sailing 2" ~ no - -0.066,
                             station == "Sailing 3" ~ no - -0.044,
                             station == "Sailing 4" ~ no - -0.028,
                             station == "Sailing 5" ~ no - -0.049,
                             station == "Sailing 6" ~ no - -0.029,
                             station == "Sailing 7" ~ no - 0.033,
                             station == "Sailing home" ~ no - -0.021,
                             station == "Sea ice" ~ no - -0.033,
                             station == "Sisimiut" ~ no - -0.027),
         no2_corr = case_when(station == "Disko Bay" ~ no2 - -0.128,
                              station == "East Greenland" ~ no2 - -0.121,
                              station == "Maniitsoq 1" ~ no2 - -0.129,
                              station == "Maniitsoq 2" ~ no2 - -0.141,
                              station == "Nuuk" ~ no2 - -0.144,
                              station == "Sailing 1" ~ no2 - -0.121,
                              station == "Sailing 2" ~ no2 - -0.130,
                              station == "Sailing 3" ~ no - -0.144,
                              station == "Sailing 4" ~ no - -0.129,
                              station == "Sailing 5" ~ no2 - -0.139,
                              station == "Sailing 6" ~ no - -0.128,
                              station == "Sailing 7" ~ no - 0.139,
                              station == "Sailing home" ~ no - -0.141,
                              station == "Sea ice" ~ no2 - -0.139,
                              station == "Sisimiut" ~ no2 - -0.119)) %>%
  timeAverage(avg.time = '1 min') %>%
  select(-c(no,no2)) %>%
  rename(no = no_corr,
         no2 = no2_corr)


# Joining data ------------------------------------------------------------

df_list = list(rel_wind,true_wind,co_dat,raw_dat_corr)

dat = df_list %>% reduce(full_join,by = "date")

# Instrumental flags ------------------------------------------------------

dat_instrumental_flags = dat %>% 
  mutate(nox = no + no2,
         flag_all = case_when(date < '2022-05-18 20:21' ~ 1,
                              date > '2022-05-20 16:15' & date < '2022-05-20 18:15' ~ 1,
                              date > '2022-05-21 00:00' & date < '2022-05-22 06:00' ~ 1,
                              date > '2022-06-11 13:33' & date < '2022-06-11 13:37' ~ 1,
                              TRUE ~ 0),
         o3_flag = case_when (date > '2022-05-19 15:51:30' & date < '2022-05-19 17:51:30' ~ 1,
                              date > '2022-06-11 12:56' & date < '2022-06-11 15:03' ~ 1,
                              date > '2022-06-20 09:58' & date < '2022-06-20 10:17' ~ 1),
         o3_flag = na.fill(o3_flag,0),
         co_flag = case_when(date < '2022-05-22 19:30' ~ 1,
                             date > '2022-06-11 10:30' & date < '2022-06-11 13:32' ~ 1,
                             date > '2022-06-22 10:13' ~ 1,
                             co > 250 ~ 1,
                             TRUE ~ 0),
         so2_flag = case_when(date > '2022-06-11 12:49' & date < '2022-06-11 14:56' ~ 1,
                              TRUE ~ 0),
         nox_flag = case_when(date > '2022-06-11 12:40' & date < '2022-06-11 13:08' ~ 1,
                              date > '2022-06-20 09:58' & date < '2022-06-20 10:17' ~ 1,
                              TRUE ~ 0),
         nox_cal = case_when(date > '2022-05-23 08:30' & date < '2022-05-23 09:44' ~ 1,
                             date > '2022-06-03 09:02' & date < '2022-06-03 09:18' ~ 1,
                             date > '2022-06-15 10:00' & date < '2022-06-15 11:40' ~ 1,
                             date > '2022-06-19 14:08' & date < '2022-06-19 14:42' ~ 1,
                             date > '2022-06-22 09:33' & date < '2022-06-22 09:44' ~ 1,
                             TRUE ~ 0),
         noy_flag = case_when(date > '2022-06-16 17:00' & date < '2022-06-19 14:50' ~ 1,
                              noy < nox ~ 1,
                              TRUE ~ 0),
         noy_cal = case_when(date > '2022-05-23 09:39' & date < '2022-05-23 10:25' ~ 1,
                             date > '2022-06-03 09:18' & date < '2022-06-03 10:48' ~ 1,
                             date > '2022-06-05 09:27' & date < '2022-06-05 09:57' ~ 1,
                             date > '2022-06-05 13:16' & date < '2022-06-05 14:15' ~ 1,
                             date > '2022-06-15 10:44' & date < '2022-06-15 11:05' ~ 1,
                             date > '2022-06-19 14:19' & date < '2022-06-17 14:30' ~ 1,
                             date > '2022-06-19 14:30' & date < '2022-06-17 14:41' ~ 1,
                             date > '2022-06-22 09:42' & date < '2022-06-22 09:54' ~ 1,
                             TRUE ~ 0),
         noy_zero = case_when(date > '2022-06-05 13:51' & date < '2022-06-05 14:03' ~ 1,
                              date > '2022-06-22 10:00' ~ 1,
                              TRUE ~ 0))


# Ship flags --------------------------------------------------------------

dat_ship_flags = dat_instrumental_flags %>% 
  mutate(ws = ifelse(between(date,as.POSIXct('2022-06-10 06:00'),as.POSIXct('2022-06-10 12:30')),NA,ws),
         stack_flag = case_when(rel_ws < 2.5 ~ "ws",
                                rel_wd > 157.5 & rel_wd <  202.5 ~ "wd",
                                between(date,as.POSIXct('2022-05-20 19:30'),as.POSIXct('2022-05-20 20:35')) ~ "ws",
                                between(date,as.POSIXct('2022-05-22 19:50'),as.POSIXct('2022-05-22 23:45')) ~ "other_ship",
                                between(date,as.POSIXct('2022-05-23 00:55'),as.POSIXct('2022-05-23 01:37')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-05-25 15:15'),as.POSIXct('2022-05-25 17:17')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-05-28 07:45'),as.POSIXct('2022-05-28 12:00')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-05-29 01:09'),as.POSIXct('2022-05-29 01:42')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-05-29 03:32'),as.POSIXct('2022-05-29 03:46')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-05-29 04:00'),as.POSIXct('2022-05-29 04:25')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-05-29 14:41'),as.POSIXct('2022-05-29 15:00')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-05-29 15:30'),as.POSIXct('2022-05-29 16:45')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-05-29 20:05'),as.POSIXct('2022-05-29 20:45')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-05-29 23:20'),as.POSIXct('2022-05-29 23:40')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-05-30 06:25'),as.POSIXct('2022-05-30 08:05')) ~ "other_ship",
                                between(date,as.POSIXct('2022-05-30 10:45'),as.POSIXct('2022-05-30 11:45')) ~ "wd",
                                between(date,as.POSIXct('2022-05-30 13:30'),as.POSIXct('2022-05-30 19:30')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-05-31 07:57'),as.POSIXct('2022-05-31 09:35')) ~ "ws",
                                between(date,as.POSIXct('2022-06-01 00:57'),as.POSIXct('2022-06-01 01:22')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-01 06:00'),as.POSIXct('2022-06-01 18:00')) ~ "ws",
                                between(date,as.POSIXct('2022-06-02 07:15'),as.POSIXct('2022-06-03 04:00')) ~ "ws",
                                between(date,as.POSIXct('2022-06-03 06:00'),as.POSIXct('2022-06-03 09:00')) ~ "ws",
                                between(date,as.POSIXct('2022-06-03 19:00'),as.POSIXct('2022-06-03 21:00')) ~ "wd",
                                between(date,as.POSIXct('2022-06-03 15:55'),as.POSIXct('2022-06-03 16:20')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-03 17:00'),as.POSIXct('2022-06-03 17:26')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-03 23:15'),as.POSIXct('2022-06-04 02:00')) ~ "ws",
                                between(date,as.POSIXct('2022-06-04 07:30'),as.POSIXct('2022-06-04 08:00')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-04 11:45'),as.POSIXct('2022-06-04 11:53')) ~ "ws",
                                between(date,as.POSIXct('2022-06-04 13:15'),as.POSIXct('2022-06-04 14:00')) ~ "ws",
                                between(date,as.POSIXct('2022-06-04 13:00'),as.POSIXct('2022-06-04 22:23')) ~ "ws",
                                between(date,as.POSIXct('2022-06-05 03:10'),as.POSIXct('2022-06-05 03:23')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-05 08:00'),as.POSIXct('2022-06-05 10:31')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-06 03:15'),as.POSIXct('2022-06-06 03:35')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-06 11:24'),as.POSIXct('2022-06-06 11:34')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-06 16:29'),as.POSIXct('2022-06-06 16:39')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-06 17:05'),as.POSIXct('2022-06-06 17:28')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-06 23:30'),as.POSIXct('2022-06-07 02:30')) ~ "other_ship",
                                between(date,as.POSIXct('2022-06-07 04:02'),as.POSIXct('2022-06-07 04:25')) ~ "other_ship",
                                between(date,as.POSIXct('2022-06-07 10:30'),as.POSIXct('2022-06-07 10:58')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-07 11:11'),as.POSIXct('2022-06-07 11:30')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-07 11:40'),as.POSIXct('2022-06-07 11:50')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-07 21:05'),as.POSIXct('2022-06-07 23:00')) ~ "other_ship",
                                between(date,as.POSIXct('2022-06-08 01:07'),as.POSIXct('2022-06-08 01:27')) ~ "other_ship",
                                between(date,as.POSIXct('2022-06-08 02:23'),as.POSIXct('2022-06-08 02:45')) ~ "other_ship",
                                between(date,as.POSIXct('2022-06-08 10:30'),as.POSIXct('2022-06-08 17:15')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-09 07:40'),as.POSIXct('2022-06-09 08:40')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-09 13:35'),as.POSIXct('2022-06-09 14:35')) ~ "other_ship",
                                between(date,as.POSIXct('2022-06-09 15:22'),as.POSIXct('2022-06-09 15:40')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-10 19:10'),as.POSIXct('2022-06-10 20:15')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-10 20:35'),as.POSIXct('2022-06-10 20:41')) ~ "other_ship",
                                between(date,as.POSIXct('2022-06-10 21:10'),as.POSIXct('2022-06-10 23:05')) ~ "other_ship",
                                between(date,as.POSIXct('2022-06-12 23:55'),as.POSIXct('2022-06-13 21:00')) ~ "wd",
                                between(date,as.POSIXct('2022-06-14 18:40'),as.POSIXct('2022-06-14 21:00')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-15 05:10'),as.POSIXct('2022-06-15 05:48')) ~ "nox_spike",
                                between(date,as.POSIXct('2022-06-15 13:05'),as.POSIXct('2022-06-15 14:35')) ~ "other_ship",
                                TRUE ~ "no"),
         stack_flag = case_when(stack_flag == "no" ~ 0,
                                stack_flag == "ws" ~ 1,
                                stack_flag == "wd" ~ 2,
                                stack_flag == "other_ship" ~ 3,
                                stack_flag == "nox_spike" ~ 4))


# Final data --------------------------------------------------------------

#lods from nox_lods script

final_dat = dat_ship_flags %>% 
  filter(flag_all == 0) %>% 
  mutate(co = ifelse(co_flag != 0,NA_real_,co),
         no = case_when(nox_flag != 0 ~ NA_real_,
                        nox_cal != 0 ~ NA_real_,
                        zero_box != 0 ~ NA_real_,
                        TRUE ~ no),
         no2 = case_when(nox_flag != 0 ~ NA_real_,
                         nox_cal != 0 ~ NA_real_,
                         zero_box != 0 ~ NA_real_,
                         TRUE ~ no2),
         nox = case_when(nox_flag != 0 ~ NA_real_,
                         nox_cal != 0 ~ NA_real_,
                         zero_box != 0 ~ NA_real_,
                         TRUE ~ nox),
         noy = case_when(noy_flag != 0 ~ NA_real_,
                         noy_cal != 0 ~ NA_real_,
                         noy_zero != 0 ~ NA_real_,
                         TRUE ~ noy),
         o3 = ifelse(o3_flag != 0,NA_real_,o3)) %>% 
  select(c(date,co,so2,noy,o3,no,no2,nox,stack_flag)) %>% 
  mutate(no_lod = 0.00687,
         noy_lod = 0.00687,
         no2_lod = 0.00988,
         nox_lod = 0.012,
         no_flag = ifelse(no < no_lod,1,0),
         no2_flag = ifelse(no2 < no2_lod,1,0),
         noy_flag = ifelse(noy < noy_lod,1,0),
         nox_flag = ifelse(nox < nox_lod,1,0)) %>% 
  rename(co_ppb = co, so2_ppb = so2,noy_ppb = noy,o3_ppb = o3,no_ppb = no,no2_ppb = no2,nox_ppb = nox)

final_dat %>% 
  filter(stack_flag == 0) %>% 
  ggplot(aes(date,noy_ppb,col = as.character(noy_flag))) +
  geom_point()

write.csv(final_dat,'output/seana_minute_data.csv',row.names = FALSE)

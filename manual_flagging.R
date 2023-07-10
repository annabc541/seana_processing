library(ggplot2)
library(viridis)
library(tidyverse)

Sys.setenv(TZ = 'UTC')
setwd('D:/Cruise/processed_data')

#flagging done in data processing, this code is to check and plot up when has been flagged
#variables in stack_flag column indicate why period has been flagged
#ws for RWS
#wd for RWD
#other_ship for spike in NOx, CO (and sometimes SO2 as well)
#nox_spike for spike in NOx alone (NOx spikes above 1ppb flagged)

# Importing data ----------------------------------------------------------

dat = read.csv("SEANA_data.csv") %>% 
  mutate(date = ymd_hms(date))

# Check and modify flagged periods ----------------------------------------

dat %>% 
  mutate(stack_flag = case_when(rel_ws < 2.5 ~ "ws",
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
         nox = no + no2,
         nox = case_when(nox_flag == 1 ~ NA_real_,
                                        flag_all == 1 ~ NA_real_,
                                        nox_cal == 1 ~ NA_real_,
                                        zero_box == 1 ~ NA_real_,
                                        TRUE ~ nox),
         no = case_when(nox_flag == 1 ~ NA_real_,
                        flag_all == 1 ~ NA_real_,
                        nox_cal == 1 ~ NA_real_,
                        zero_box == 1 ~ NA_real_,
                        TRUE ~ no),
         no2 = case_when(nox_flag == 1 ~ NA_real_,
                         flag_all == 1 ~ NA_real_,
                         nox_cal == 1 ~ NA_real_,
                         zero_box == 1 ~ NA_real_,
                         TRUE ~ no2),
         noy = case_when(noy_flag == 1 ~ NA_real_,
                         flag_all == 1 ~ NA_real_,
                         noy_cal == 1 ~ NA_real_,
                         noy_zero == 1 ~ NA_real_,
                         TRUE ~ noy),
         o3 = case_when(o3_flag == 1 ~ NA_real_,
                        flag_all == 1 ~ NA_real_,
                        TRUE ~ o3),
         co = case_when(co_flag == 1 ~ NA_real_,
                        flag_all == 1 ~ NA_real_,
                        TRUE ~ co),
         so2 = case_when(so2_flag == 1 ~ NA_real_,
                         flag_all == 1 ~ NA_real_,
                         so2_zero_channel == 1 ~ NA_real_,
                         TRUE ~ so2)) %>% 
  filter(
    co < 250,
    date > "2022-05-28" & date < "2022-05-29",
    station == "Sea ice",
    stack_flag == "no",
         ) %>%
  pivot_longer(c(nox,co,so2)) %>% 
  ggplot(aes(date,value, col = stack_flag)) +
  geom_point() +
  facet_grid(rows = vars(name), scales = "free_y") +
  scale_colour_manual(values = c("black",'#EE9B00','#0A9396','#9B2226','#94D2BD')) +
  # scale_x_datetime(breaks = "1 hour",date_labels = "%H:%M") +
  NULL

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(zoo)
library(viridis)
library(openair)
  
setwd('D:/Cruise/data/raw_data')
Sys.setenv(TZ = 'UTC')


# Importing data ------------------------------------------------------------

#Importing data recorded in BST and adjusting it to UTC

files_BST = list.files('BST', full.names=TRUE)
datList_BST = list()
hrs = 1 * 60 *60
for(index in 1:length(files_BST)) {
  
  datList_BST[[index]] = read.csv(files_BST[index],header=TRUE, na.strings= c('NA','missing')) %>%
    mutate(TheTime = mdy_hms(TheTime)) %>%
    rename(date = TheTime) %>% 
    tibble()
  
}

BST = bind_rows(datList_BST) %>%
  mutate(date = date - hrs)

#Importing files after changing laptop to UTC
files_UTC = list.files('UTC', full.names=TRUE)
datList_UTC = list()
for(index in 1:length(files_UTC)) {
  
  datList_UTC[[index]] = read.csv(files_UTC[index],header=TRUE, na.strings= c('NA','missing'))%>%
    mutate(TheTime = mdy_hms(TheTime)) %>%
    rename(date = TheTime) %>% 
    tibble()
  
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
         no2 = t200up_023828_no2) %>% 
  mutate(date = round_date(date, "1 sec"))

remove(UTC,BST,datList_BST,datList_UTC,files_BST,files_UTC,hrs,index)


# Flagging dataframe ------------------------------------------------------

raw_data1 = raw_data %>% 
  mutate(flag_all = case_when(date < '2022-05-18 20:21' ~ 1,
                              date > '2022-05-20 16:15' & date < '2022-05-20 18:15' ~ 1,
                              date > '2022-05-21 00:00' & date < '2022-05-22 06:00' ~ 1,
                              date > '2022-06-11 13:33' & date < '2022-06-11 13:37' ~ 1),         flag_all = na.fill(flag_all,0),
         o3_flag = case_when (date > '2022-05-19 15:51:30' & date < '2022-05-19 17:51:30' ~ 1,
                              date > '2022-06-11 12:56' & date < '2022-06-11 15:03' ~ 1,
                              date > '2022-06-20 09:58' & date < '2022-06-20 10:17' ~ 2),
         o3_flag = na.fill(o3_flag,0),
         co_flag = case_when(date < '2022-05-22 19:30' ~ 1,
                             date > '2022-06-11 13:18' & date < '2022-06-11 13:32' ~ 1,
                             date > '2022-06-22 10:13' ~ 1),
         co_flag = na.fill(co_flag,0),
         so2_flag = case_when(date > '2022-06-11 12:49' & date < '2022-06-11 14:56' ~ 1),
         so2_flag = na.fill(so2_flag,0),
         nox_flag = case_when(date > '2022-06-11 12:40' & date < '2022-06-11 12:50' ~ 1,
                              date > '2022-06-20 09:58' & date < '2022-06-20 10:17' ~ 2),
         nox_flag = na.fill(nox_flag,0),
         nox_cal = case_when(date > '2022-05-23 09:22' & date < '2022-05-23 09:44' ~ 1,
                             date  > '2022-06-03 09:02' & date < '2022-06-03 09:18' ~ 1,
                             date > '2022-06-15 10:12' & date < '2022-06-15 11:00' ~ 1,
                             date > '2022-06-19 14:08' & date < '2022-06-19 14:42' ~ 1,
                             date > '2022-06-22 09:33' & date < '2022-06-22 09:44' ~ 1),
         nox_cal = na.fill(nox_cal,0),
         noy_flag = case_when(date > '2022-06-16 17:00' & date < '2022-06-19 14:50' ~ 1),
         noy_flag = na.fill(noy_flag,0),
         noy_cal = case_when(date > '2022-05-23 09:39' & date < '2022-05-23 10:25' ~ 1,
                             date > '2022-06-03 09:18' & date < '2022-06-03 10:48' ~ 1,
                             date > '2022-06-05 09:27' & date < '2022-06-05 09:57' ~ 1,
                             date > '2022-06-05 13:16' & date < '2022-06-05 14:15' ~ 1,
                             date > '2022-06-15 10:44' & date < '2022-06-15 11:05' ~ 1,
                             date > '2022-06-19 14:19' & date < '2022-06-17 14:30' ~ 1,
                             date > '2022-06-19 14:30' & date < '2022-06-17 14:41' ~ 1,
                             date > '2022-06-22 09:42' & date < '2022-06-22 09:54' ~ 1),
         noy_cal = na.fill(noy_cal,0),
         noy_zero = case_when(date > '2022-06-05 13:51' & date < '2022-06-05 14:03' ~ 1,
                              date > '2022-06-22 10:00' ~ 1),
         noy_zero = na.fill(noy_zero,0),
         pollution = case_when(date > '2022-06-12 23:55' & date < '2022-06-13 21:00' ~ 1),
         pollution = na.fill(pollution,0))



# Correcting data -----------------------------------------------------

dat1 = raw_data1 %>% 
  mutate(no_corr = no - -0.0363139655120056,
         no2_corr = no2 - -0.134759009068831)

dat2 = dat1 %>% 
  select(c(date,o3,no,no2,no_corr,no2_corr,noy,zero_box,flag_all,o3_flag,nox_cal,nox_flag,noy_cal,noy_flag,noy_zero,so2,so2_zero_channel,so2_flag,pollution)) %>% 
  mutate(o3 = ifelse(o3_flag == 0,o3,NA),
         no_corr = ifelse(nox_flag == 0,no_corr,NA),
         no_corr = ifelse(zero_box == 0,no_corr,NA),
         no2_corr = ifelse(nox_flag == 0,no2_corr,NA),
         no2_corr = ifelse(zero_box == 0,no2_corr,NA),
         no_corr = ifelse(nox_cal == 0,no_corr,NA),
         no2_corr = ifelse(nox_cal == 0,no2_corr,NA),
         noy = ifelse(noy_cal == 0,noy,NA),
         noy = ifelse(noy_flag == 0,noy,NA),
         noy = ifelse(noy_zero == 0,noy,NA),
         so2 = ifelse(so2_zero_channel == 0,so2,NA),
         so2 = ifelse(so2_flag == 0,so2,NA),
         o3 = ifelse(flag_all == 0,o3,NA),
         no_corr = ifelse(flag_all == 0,no_corr,NA),
         no2_corr = ifelse(flag_all == 0,no2_corr,NA),
         noy = ifelse(flag_all == 0,noy,NA),
         so2 = ifelse(flag_all == 0,so2,NA)) %>% 
  mutate(date = round_date(date, "1 sec"))

dattwo = dat2 %>% 
  timeAverage(avg.time = '1 min')



# CO data -----------------------------------------------------------------

co_dat = read.csv('SEANA_CO.csv') %>%
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  filter(date < '2022-06-22 10:13') %>% 
  ungroup()

co_dat1 = co_dat %>% 
  select(c(date,co_calibrated)) %>% 
  mutate(flag_all = case_when(date < '2022-05-18 20:21' ~ 1,
                              date > '2022-05-20 16:15' & date < '2022-05-20 18:15' ~ 1,
                              date > '2022-05-21 00:00' & date < '2022-05-22 06:00' ~ 1,
                              date > '2022-06-11 13:33' & date < '2022-06-11 13:37' ~ 1),
         flag_all = na.fill(flag_all,0),
         co_flag = case_when(date < '2022-05-22 19:30' ~ 1,
                             date > '2022-06-11 13:18' & date < '2022-06-11 13:32' ~ 1,
                             date > '2022-06-22 10:13' ~ 1),
         co_flag = na.fill(co_flag,0))

co_dat2 = co_dat1 %>% 
  mutate(co_calibrated = ifelse(flag_all == 0,co_calibrated,NA),
         co_calibrated = ifelse(co_flag == 0,co_calibrated,NA))

dat3 = bind_rows(select(dat2,date,value = o3) %>% mutate(poll = 'O3'),
                 select(dat2,date,value = no_corr) %>% mutate(poll = 'NO'),
                 select(dat2,date,value = no2_corr) %>% mutate(poll = 'NO2'),
                 select(dat2,date,value = noy) %>% mutate(poll = 'NOy'),
                 select(co_dat2,date,value = co_calibrated) %>% mutate(poll = 'CO'))
                 # select(dat2,date,value = so2) %>% mutate(poll = 'SO2'))

dat3 = left_join(dattwo,co_dat2, by = 'date') %>%
  rename(NO = no_corr,
         NO2 = no2_corr,
         CO = co_calibrated) %>% 
  filter(date)

timePlot(dat3,pollutant = c('o3','NO','NO2','CO','so2'),
         y.relation = 'free')


dat3 %>% 
  ggplot(aes(date,value,col = poll)) + 
  geom_line() +
  facet_grid(rows = vars(poll),scales = 'free_y') +
  labs(x = 'Date',
       y = 'Mixing ratios / ppb') +
  theme_bw() +
  scale_color_manual(values = (plasma(6))) +
  theme(legend.position = 'None')

ggsave('SEANA_for_pres.png',
       path = '~/PhD work/Cruise/Plots',
       width = 33,
       height = 15,
       units = 'cm')

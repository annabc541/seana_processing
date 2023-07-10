library(dplyr)
library(openair)
library(lubridate)
library(tidyr)
library(ggplot2)
library(zoo)
library(viridis)

setwd('D:/Cruise/raw_data')

# Importing data ------------------------------------------------------------

#Importing data recorded in BST and adjusting it to UTC
files_BST = list.files('BST', full.names=TRUE)
datList_BST = list()
hrs = 1 * 60 *60
for(index in 1:length(files_BST)) {
  
  datList_BST[[index]] = read.csv(files_BST[index],header=TRUE, na.strings= c('NA','missing'))%>%
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

remove(UTC,BST,datList_BST,datList_UTC,files_BST,files_UTC,hrs,index)

# CO calibration ----------------------------------------------------------

raw_data1 = raw_data %>% 
  select(date,no,no2,noy,co,o3,so2,zero_box,so2_zero_channel,al5002_inst_co_sens) %>% 
  mutate(id = 1:nrow(.))
#Using change in CO sensitivity as a calibration flag
co_sens = raw_data1 %>% 
  select(al5002_inst_co_sens) %>% 
  rename(diff_in_sens = al5002_inst_co_sens)

sens_diff = diff(as.matrix(co_sens))
sens_diff = data.frame(sens_diff)
sens_diff = rbind(NA,sens_diff) %>% 
  mutate(id = 1:nrow(.))

raw_data2 = raw_data1 %>% 
  left_join(sens_diff,by = 'id') %>% 
  select(-id)

calibrating = rle(raw_data2$diff_in_sens) %>% 
  wsdmiscr::tidy_rle() %>% 
  filter(values == 0) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% #df with row numbers and corresponding groups
  tibble()

dat = raw_data2 %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(calibrating, "idx") %>% #joins two dfs by their row number
  mutate(id = ifelse(is.na(id), 0, id), #makes id (group) = 0 when cal running
         idx_2 = 1:nrow(.)) %>% 
  group_by(id) %>% 
  mutate(idx = 1:n(), #numbers each row based on group
         co_cal1 = ifelse(idx < 200, 1, 0), #removes first x values of group
         idx = n():1,
         co_cal2 = ifelse(idx < 200, 1,0),
         co_cal = co_cal1 + co_cal2) %>% #removes last x values of group
  ungroup() %>% 
  mutate(ID = 1:nrow(.)) %>%
  select(date,co,no,no2,noy,o3,so2,zero_box,so2_zero_channel,co_cal) %>% 
  mutate(idx_2 = 1:nrow(.)) %>% 
  mutate(co_cal = ifelse(co_cal == 2,1,co_cal))

remove(calibrating, co_sens,sens_diff,raw_data2)

# Applying zeroes ---------------------------------------------------------

nox_zeroing = rle(dat$zero_box) %>% 
  wsdmiscr::tidy_rle() %>% 
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble() 

zeroes_no_no2 = dat %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(nox_zeroing, "idx") %>% #joins two dfs by their row number
  select(-idx) %>% 
  mutate(id = ifelse(is.na(id), 0, id),
         idx_2 = 1:nrow(.)) %>% #makes id (group) = 0 when cal running
  filter(zero_box == 1) %>% 
  group_by(id) %>% 
  summarise(no = mean(no),
            no2 = mean(no2),
            idx_2 = idx_2,
            date = date) %>%
  rename(zero_no = no,
         zero_no2 = no2) %>% 
  ungroup()

zeroes_no_no2 %>% 
  # mutate(zero_no = ifelse(zero_no > 0.6,NA,zero_no),
  #        zero_no2 = ifelse(zero_no2 > 0.4,NA,zero_no2)) %>%
  pivot_longer(c(zero_no,zero_no2)) %>% 
  # filter(value > 0 & value < 2) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point() + 
  scale_x_datetime(breaks = '1 day', date_labels = '%d', minor_breaks = '1 day')

#as above with so2
so2_zeroing = rle(dat$so2_zero_channel) %>%   
  wsdmiscr::tidy_rle() %>% 
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble() 

zeroes_so2 = dat %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(so2_zeroing, "idx") %>% #joins two dfs by their row number
  select(-idx) %>% 
  mutate(id = ifelse(is.na(id), 0, id),
         idx_2 = 1:nrow(.)) %>% #makes id (group) = 0 when cal running
  filter(so2_zero_channel == 1) %>% 
  group_by(id) %>% 
  summarise(so2 = mean(so2),
            idx_2 = idx_2,
            date = date) %>%
  rename(zero_so2 = so2) %>% 
  ungroup()

zeroes_so2 %>% 
  mutate(date = as.POSIXct(date)) %>% 
  ggplot(aes(date,zero_so2)) +
  geom_point() +
  scale_x_datetime(date_labels = '%b %d',date_minor_breaks = '1 day')

#add zeroes and interpolate, then correct nox and so2 data
dat_corr = dat %>% 
  left_join(zeroes_no_no2,by = 'idx_2') %>% 
  left_join(zeroes_so2,by = 'idx_2') %>% 
  select(-c(idx_2,date.y,date,id.y,id.x)) %>% 
  rename(date = date.x) %>% 
  # mutate(zero_no = ifelse(zero_no > 0.6,NA,zero_no),
  # zero_no2 = ifelse(zero_no2 > 0.05,NA,zero_no2)) %>%
  mutate(zero_no = na.approx(zero_no,na.rm = F),
         zero_no2 = na.approx(zero_no2,na.rm = F),
         zero_so2 = na.approx(zero_so2,na.rm = F),
         no_corr = no-zero_no,
         no2_corr = no2-zero_no2,
         so2_corr = so2 - zero_so2)

# Proper dataframe --------------------------------------------------------

dat_corr = dat_corr %>% 
  mutate(flag_all = case_when(date < '2022-05-18 20:21' ~ 1,
                              date > '2022-05-20 16:15' & date < '2022-05-20 18:15' ~ 1,
                              date > '2022-05-21 00:00' & date < '2022-05-22 06:00' ~ 1,
                              date > '2022-06-11 13:33' & date < '2022-06-11 13:37' ~ 1),
         flag_all = na.fill(flag_all,0),
         o3_flag = case_when (date > '2022-05-19 15:51:30' & date < '2022-05-19 17:51:30' ~ 1,
                              date > '2022-06-11 12:56' & date < '2022-06-11 15:03' ~ 1,
                              date > '2022-06-20 09:58' & date < '2022-06-20 10:17' ~ 2),
         o3_flag = na.fill(o3_flag,0),
         co_flag = case_when(date < '2022-05-22 18:30' ~ 1,
                             date > '2022-06-11 13:18' & date < '2022-06-11 13:32' ~ 1),
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
         noy_zero = na.fill(noy_zero,0))
  
# Plotting data -----------------------------------------------------------

data_for_plotting = dat_corr %>% 
  #column to filter and remove measurements from when instruments turned on and warming up
  mutate(o3 = ifelse(o3_flag == 0,o3,NA),
         co = ifelse(co_flag == 0,co,NA),
         no_corr = ifelse(nox_flag == 0,no_corr,NA),
         no_corr = ifelse(zero_box == 0,no_corr,NA),
         no2_corr = ifelse(nox_flag == 0,no2_corr,NA),
         no2_corr = ifelse(zero_box == 0,no_corr,NA),
         so2_corr = ifelse(so2_flag == 0,so2_corr,NA),
         so2_corr = ifelse(so2_zero_channel == 0,so2_corr,NA),
         no_corr = ifelse(nox_cal == 0,no_corr,NA),
         no2_corr = ifelse(nox_cal == 0,no2_corr,NA),
         noy = ifelse(noy_cal == 0,noy,NA),
         noy = ifelse(noy_flag == 0,noy,NA),
         noy = ifelse(noy_zero == 0,noy,NA),
         o3 = ifelse(flag_all == 0,o3,NA),
         no_corr = ifelse(flag_all == 0,no_corr,NA),
         no2_corr = ifelse(flag_all == 0,no2_corr,NA),
         co = ifelse(flag_all == 0,co,NA),
         so2_corr = ifelse(flag_all == 0,so2_corr,NA),
         noy = ifelse(flag_all == 0,noy,NA),
         co = ifelse(co_cal == 1,NA,co))

data_for_plotting %>% 
  filter(date > '2022-05-25 00:00' & date < '2022-05-30') %>%
  pivot_longer(c(co,no_corr,no2_corr,noy,so2_corr,o3)) %>% 
ggplot(aes(date,value,col = name)) +
  geom_line() +
  facet_grid(rows = vars(name),scale = 'free_y') +
  theme_minimal() +
  labs(x = 'Date',
       y = 'Mixing ratios / ppb',
       color = 'Measured species')+
  scale_color_manual(labels = c('CO','NO',expression(NO[2]),expression(NO[y]),expression(O[3]),expression(SO[2])),
                     values = (plasma(6))) +
  theme(legend.position = 'bottom')
  # scale_x_datetime(date_labels = '%b %d %R',date_breaks = '1 day', date_minor_breaks = '12 hour')
  # geom_hline(data = data_hline,
  #            aes(yintercept = hline))

ggsave('o3_co.png',
       path = 'D:/Cruise/Plots',
       width = 33,
       height = 12.5,
       units = 'cm')

data_hline = data.frame(name = unique(data_for_plotting$name),
                        hline = (c(NA,0.2,0.2,0.2,0.4,NA)))

# One minute average ------------------------------------------------------

one_min = dat_corr %>%
  filter(date > '2022-06-16') %>% 
  group_by(date = cut(date,breaks = '1 min')) %>% 
  summarise(co = mean(co),
            no = mean(no),
            no_corr = mean(no_corr),
            no2 = mean(no2),
            no2_corr = mean(no2_corr),
            noy = mean(noy),
            o3 = mean(o3),
            so2 = mean(so2),
            so2_corr = mean(so2_corr),
            zero_box = mean(zero_box),
            so2_zero_channel = mean(so2_zero_channel),
            co_cal = mean(co_cal)) %>% 
  ungroup() %>% 
  mutate(date = as.POSIXct(date),
         flag_all = case_when(date < '2022-05-18 20:21' ~ 1,
                              date > '2022-05-20 16:15' & date < '2022-05-20 18:15' ~ 1,
                              date > '2022-05-21 00:00' & date < '2022-05-22 06:00' ~ 1,
                              date > '2022-06-11 13:33' & date < '2022-06-11 13:37' ~ 1),
         flag_all = na.fill(flag_all,0),
         o3_flag = case_when (date > '2022-05-19 15:51:30' & date < '2022-05-19 17:51:30' ~ 1,
                              date > '2022-06-11 12:56' & date < '2022-06-11 15:03' ~ 1,
                              date > '2022-06-20 09:58' & date < '2022-06-20 10:17' ~ 2),
         o3_flag = na.fill(o3_flag,0),
         co_flag = case_when(date < '2022-05-22 18:30' ~ 1,
                             date > '2022-06-11 13:18' & date < '2022-06-11 13:32' ~ 1),
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
         co_cal = ifelse(date < '2022-05-19',0,co_cal))

write.csv(one_min,'D:/Cruise/SEANA_one_min_5.csv')

one_min_no_poll = dat_corr %>%
  # group_by(date = cut(date,breaks = '1 min')) %>% 
  # summarise(co = mean(co),
  #           no = mean(no),
  #           no_corr = mean(no_corr),
  #           no2 = mean(no2),
  #           no2_corr = mean(no2_corr),
  #           noy = mean(noy),
  #           o3 = mean(o3),
  #           so2 = mean(so2),
  #           so2_corr = mean(so2_corr),
  #           zero_box = mean(zero_box),
  #           so2_zero_channel = mean(so2_zero_channel),
  #           co_cal = mean(co_cal)) %>% 
  # ungroup() %>% 
  mutate(date = as.POSIXct(date),
         flag_all = case_when(date < '2022-05-18 20:21' ~ 1,
                              date > '2022-05-20 16:15' & date < '2022-05-20 18:15' ~ 1,
                              date > '2022-05-21 00:00' & date < '2022-05-22 06:00' ~ 1,
                              date > '2022-06-11 13:33' & date < '2022-06-11 13:37' ~ 1,
                              date > '2022-06-12 23:55' & date < '2022-06-13 21:00' ~ 1),
                              # date > '2022-06-04 20:00' & date < '2022-06-04 23:55' ~ 1),
         flag_all = na.fill(flag_all,0),
         o3_flag = case_when (date > '2022-05-19 15:51:30' & date < '2022-05-19 17:51:30' ~ 1,
                              date > '2022-06-11 12:56' & date < '2022-06-11 15:03' ~ 1,
                              date > '2022-06-20 09:58' & date < '2022-06-20 10:17' ~ 2),
         o3_flag = na.fill(o3_flag,0),
         co_flag = case_when(date < '2022-05-22 18:30' ~ 1,
                             date > '2022-06-11 13:18' & date < '2022-06-11 13:32' ~ 1),
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
         co_cal = ifelse(date < '2022-05-19',0,co_cal))
  

one_min_no_poll %>% 
  filter(date > '2022-05-29 00:00' & date <'2022-06-04 20:00') %>% 
  mutate(o3 = ifelse(o3_flag == 0,o3,NA),
         co = ifelse(co_flag == 0,co,NA),
         no_corr = ifelse(nox_flag == 0,no_corr,NA),
         no2_corr = ifelse(nox_flag == 0,no2_corr,NA),
         so2_corr = ifelse(so2_flag == 0,so2_corr,NA),
         no_corr = ifelse(nox_cal == 0,no_corr,NA),
         no2_corr = ifelse(nox_cal == 0,no2_corr,NA),
         noy = ifelse(noy_cal == 0,noy,NA),
         o3 = ifelse(flag_all == 0,o3,NA),
         no_corr = ifelse(flag_all == 0,no_corr,NA),
         no2_corr = ifelse(flag_all == 0,no2_corr,NA),
         co = ifelse(flag_all == 0,co,NA),
         so2_corr = ifelse(flag_all == 0,so2_corr,NA),
         noy = ifelse(flag_all == 0,noy,NA),
         noy = ifelse(noy_flag == 0,noy,NA),
         co = ifelse(co_cal == 1,NA,co)) %>% 
pivot_longer(c(co,no_corr,no2_corr,noy,o3,so2_corr)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_line() +
  facet_grid(rows = vars(name),scale = 'free_y') +
  # geom_hline(mapping = aes(value = 'no',yintercept = 0.2)) +
  theme_minimal() +
  labs(x = 'Date',
       y = 'Mixing ratios / ppb',
       color = 'Measured species')+
  scale_color_manual(na.translate = FALSE,
                     labels = c('CO','NO',expression(NO[2]),expression(NO[y]),expression(O[3]),expression(SO[2])),
                     values = (plasma(6))) +
  theme(legend.position = 'bottom')+
  geom_vline(xintercept = as.POSIXct('2022-05-20 20:15:00')) +
  geom_vline(xintercept = as.POSIXct('2022-05-23 06:15:00'), col = 'green') +
  geom_vline(xintercept = as.POSIXct('2022-05-26 00:00'), col = 'green') +
  geom_vline(xintercept = as.POSIXct('2022-05-29 00:00'),col = 'blue') +
  geom_vline(xintercept = as.POSIXct('2022-05-31 08:00'), col = 'blue') +
  geom_vline(xintercept = as.POSIXct('2022-06-01 08:00'), col = 'navy blue') +
  geom_vline(xintercept = as.POSIXct('2022-06-03 07:00'), col = 'navy blue') +
  geom_vline(xintercept = as.POSIXct('2022-06-03 20:00'), col = 'maroon') +
  geom_vline(xintercept = as.POSIXct('2022-06-04 20:00'), col = 'maroon') +
  geom_vline(xintercept = as.POSIXct('2022-06-05 11:00')) +
  geom_vline(xintercept = as.POSIXct('2022-06-06 10:30')) +
  geom_vline(xintercept = as.POSIXct('2022-06-06 18:30'), col = 'turquoise') +
  geom_vline(xintercept = as.POSIXct('2022-06-08 09:00'),col = 'red') +
  geom_vline(xintercept = as.POSIXct('2022-06-08 17:00'),col = 'red') +
  geom_vline(xintercept = as.POSIXct('2022-06-12 23:55'),col = 'turquoise') +
  geom_vline(xintercept = as.POSIXct('2022-06-13 20:40'),col = 'navy blue') +
  geom_vline(xintercept = as.POSIXct('2022-06-16 09:00'),col = 'navy blue')
  # scale_x_datetime(date_labels = '%b %d',date_breaks = '4 days', date_minor_breaks = '1 day')

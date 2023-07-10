library(dplyr)
library(openair)
library(lubridate)
library(tidyr)
library(ggplot2)
library(zoo)
library(viridis)

setwd('D:/Cruise/raw_data')
Sys.setenv(TZ = 'UTC')

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

# raw_data = raw_data %>% 
#   mutate(idx_2 = 1:nrow(.))

remove(UTC,BST,datList_BST,datList_UTC,files_BST,files_UTC,hrs,index)


# CO cal ------------------------------------------------------------------

#create df with only necessary cols
#df with a row number, id
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

#df with change in sens: when change = 0, not calibrating
raw_data2 = raw_data1 %>% 
  left_join(sens_diff,by = 'id') %>% 
  select(-id)

not_cal = rle(raw_data2$diff_in_sens) %>%
  tidy_rle() %>% 
  filter(values == 0) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble()

dat = raw_data2 %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(not_cal, "idx") %>% #joins two dfs by their row number
  select(-idx) %>%
  fill(id) %>% 
  mutate(idx_2 = 1:nrow(.)) %>% 
  group_by(id) %>% 
  mutate(idx = 1:n(), #numbers each row based on group
         co_cal1 = ifelse(idx < 50, 1, 0), #removes first x values of group
         idx = n():1,
         co_cal2 = ifelse(idx < 50,1,0),
         co_cal = co_cal1 + co_cal2) %>% #removes last x values of group
  ungroup() %>% 
  mutate(co_cal = ifelse(is.na(co),0,co_cal),
         co_cal = ifelse(co_cal == 2,1,co_cal)) %>% 
  select(date,co,no,no2,noy,o3,so2,zero_box,so2_zero_channel,co_cal,idx_2)

remove(co_sens,not_cal,sens_diff,raw_data1,raw_data2)

# Applying zeroes ---------------------------------------------------------

nox_zeroing = rle(dat$zero_box) %>% 
  tidy_rle() %>% 
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

# zeroes_no_no2 %>% 
  # mutate(zero_no = ifelse(zero_no > 0.6,NA,zero_no),
  #        zero_no2 = ifelse(zero_no2 > 0.4,NA,zero_no2)) %>%
  # pivot_longer(c(zero_no,zero_no2)) %>% 
  # filter(value > 0 & value < 2) %>% 
  # ggplot(aes(date,value,col = name)) +
  # geom_point() + 
  # scale_x_datetime(breaks = '1 day', date_labels = '%d', minor_breaks = '1 day')

#as above with so2
so2_zeroing = rle(dat$so2_zero_channel) %>%   
  tidy_rle() %>% 
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

# zeroes_so2 %>% 
#   mutate(date = as.POSIXct(date)) %>% 
#   ggplot(aes(date,zero_so2)) +
#   geom_point() +
#   scale_x_datetime(date_labels = '%b %d',date_minor_breaks = '1 day')

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
         no_corr = no - zero_no,
         no2_corr = no2 - zero_no2,
         so2_corr = so2 - zero_so2)

remove(nox_zeroing,so2_zeroing,zeroes_no_no2,zeroes_so2)

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
         noy_zero = na.fill(noy_zero,0))

# Plotting data -----------------------------------------------------------

data_for_plotting = dat_corr %>% 
  #column to filter and remove measurements from when instruments turned on and warming up
  mutate(o3 = ifelse(o3_flag == 0,o3,NA),
         co = ifelse(co_flag == 0,co,NA),
         no_corr = ifelse(nox_flag == 0,no_corr,NA),
         no_corr = ifelse(zero_box == 0,no_corr,NA),
         no2_corr = ifelse(nox_flag == 0,no2_corr,NA),
         no2_corr = ifelse(zero_box == 0,no2_corr,NA),
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

with_colours = data_for_plotting %>% 
  pivot_longer(c(co,no_corr,no2_corr,noy,o3,so2_corr)) %>% 
  filter(date > '2022-05-29 00:00' & date < '2022-06-04 20:00')
  
rects <- data.frame(xstart = c('2022-05-20 20:15:00','2022-05-23 06:15:00','2022-05-26 00:00','2022-05-29 00:00','2022-05-31 08:00','2022-06-01 08:00','2022-06-03 07:00','2022-06-03 20:00','2022-06-04 20:00','2022-06-05 11:00','2022-06-06 10:30','2022-06-06 18:30','2022-06-12 23:55','2022-06-13 20:40','2022-06-16 09:00'),
                    xend = c('2022-05-23 06:15:00','2022-05-26 00:00','2022-05-29 00:00','2022-05-31 08:00','2022-06-01 08:00','2022-06-03 07:00','2022-06-03 20:00','2022-06-04 20:00','2022-06-05 11:00','2022-06-06 10:30','2022-06-06 18:30','2022-06-12 23:55','2022-06-13 20:40','2022-06-16 09:00','2022-06-23 12:00'),
                    cols = c('Sailing','Eastern coast of Greenland','Sailing','Nuuk','Sailing','Maniitsoq','Sailing','Sisimut','Sailing','Disko Bay','Sailing','Sea ice','Sailing','Maniitsoq','Sailing'))

ggplot() +
  geom_rect(data = rects1, aes(xmin = as.POSIXct(xstart), xmax = as.POSIXct(xend), ymin = -Inf, ymax = Inf, fill = cols), alpha = 0.4) +
  geom_line(data = with_colours,aes(date,value)) +
  facet_grid(rows = vars(name),scale = 'free_y') +
  theme_bw() +
  labs(x = 'Date',
       y = 'Mixing ratios / ppb',
       cols = 'Location') +
  theme(legend.position = 'bottom')

ggsave('west_greenland.png',
       path = 'D:/Cruise/Plots',
       width = 33,
       height = 12.5,
       units = 'cm')

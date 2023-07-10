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

remove(UTC,BST,datList_BST,datList_UTC,files_BST,files_UTC,hrs,index)


# Flagging dataframe ------------------------------------------------------

raw_data1 = raw_data %>% 
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
         noy_zero = na.fill(noy_zero,0),
         poll = case_when(date > '2022-06-12 23:55' & date < '2022-06-13 21:00' ~ 1),
         poll = na.fill(poll,0))

# Applying NOx zeroes ---------------------------------------------------------

raw_data2 = raw_data1 %>% 
  filter(flag_all == 0) %>% 
  mutate(zero_box = ifelse(is.na(zero_box),0,zero_box),
         so2_zero_channel = ifelse(is.na(so2_zero_channel),0,so2_zero_channel),
         no = ifelse(nox_flag == 1,NA,no),
         no2 = ifelse(nox_flag == 1,NA,no2),
         so2 = ifelse(so2_flag ==1,NA,so2)) 
  select(date,no,no2,so2,zero_box,so2_zero_channel,poll,o3)
  
zero_nox = rle(raw_data2$zero_box) %>% 
  wsdmiscr::tidy_rle() %>% 
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble()

dat = raw_data2 %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zero_nox, "idx") %>% 
  mutate(id = ifelse(is.na(id), 0, id)) %>% 
  group_by(id) %>% 
  nest_by() %>%
  mutate(zeroMed_no = ifelse(id == 0, NA, median(data$no, na.rm = T)),
         zeroMed_no = ifelse(zeroMed_no > 0.25 | zeroMed_no < -0.25, NA, zeroMed_no),
         zeroMed_no2 = ifelse(id == 0, NA, median(data$no2, na.rm = T)),      
         zeroMed_no2 = ifelse(zeroMed_no2 > 0, NA, zeroMed_no2)) %>%
  unnest(data) %>% 
  arrange(date) %>% 
  ungroup() %>%
  mutate(zero_no = na.approx(zeroMed_no,na.rm = F),
         zero_no2 = na.approx(zeroMed_no2,na.rm = F),
         no_corr = no - zero_no,
         no2_corr = no2-zero_no2)
  # select(-id)

dat %>% 
  filter(poll == 0, 
         date > '2022-05-25' & date < '2022-05-30') %>%
  pivot_longer(c(no,no_corr,zero_no)) %>%
  ggplot() +
  geom_point(aes(date,zeroMed_no)) +
  geom_line(aes(date,value,col = name))
 


# Applying SO2 zeroes -----------------------------------------------------

zero_so2 = rle(raw_data2$so2_zero_channel) %>% 
  wsdmiscr::tidy_rle() %>% 
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble()

dat1 = dat %>% 
  left_join(zero_so2, "idx") %>% 
  mutate(id = ifelse(is.na(id), 0, id)) %>% 
  group_by(id) %>% 
  nest_by() %>%
  mutate(zeroMed_so2 = ifelse(id == 0, NA, median(data$so2, na.rm = T)),     
         zeroMed_so2 = ifelse(zeroMed_so2 < 0.01, NA, zeroMed_so2)) %>%
  unnest(data) %>% 
  arrange(date) %>% 
  ungroup() %>%
  mutate(zero_so2 = na.approx(zeroMed_so2,na.rm = F),
         so2_corr = so2-zero_so2)

dat1 %>% 
  filter(poll == 0) %>%
  mutate(hod = hour(date)) %>%
  # pivot_longer(c(so2,so2_corr,zero_so2)) %>%
  ggplot(aes(date,zero_so2)) +
  geom_line() 
  # scale_x_datetime(date_labels = '%d',date_breaks = '1 day',date_minor_breaks = '1 day')
  



plots = dat %>% 
  mutate(nox = no_corr + no2_corr)

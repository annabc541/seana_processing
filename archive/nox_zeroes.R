no2 = raw_data2 %>% 
  select(c(date,no2,samppress = t200up_023828_samppress,sampflow = t200up_023828_sampflow,ozonflow = t200up_023828_ozonflow,hvps = t200up_023828_hvps,rcelltemp = t200up_023828_rcelltemp,boxtemp = t200up_023828_boxtemp,pmttemp = t200up_023828_pmttemp,convtemp = t200up_023828_convtemp,rcellpress = t200up_023828_rcellpress,t200up_023828_noslope,t200up_023828_nooffset,zero_box,flag_all,nox_flag,nox_cal,poll,flag_all,nox_flag,nox_cal)) %>% 
  mutate(flag = flag_all + nox_flag,
           flag = ifelse(flag == 2,1,flag),
         zero_flag = case_when(date > '2022-06-19' ~ 1,
                               date < '2022-05-23' ~ 1,
                               date > '2022-06-13' & date < '2022-06-14' ~ 1,
                               date > '2022-05-31' & date < '2022-06-01' ~ 1,
                               date > '2022-06-03' & date < '2022-06-04' ~ 1,
                               # date > '2022-06-15 11:00' & date < '2022-06-16 12:00' ~ 1,
                               # date > '2022-05-26' & date < '2022-05-27' ~ 1,
                               TRUE ~ 0),
         no2 = ifelse(zero_box == 1 & zero_flag == 1,NA,no2))


no2 %>% 
  filter(zero_box == 1,
         flag == 0) %>%
  pivot_longer(c(rcelltemp,boxtemp,pmttemp,convtemp,no2)) %>%
  ggplot(aes(date,value,col = name)) +
  geom_point() +
  facet_grid(rows = vars(name), scales = "free") +
  scale_x_datetime(date_breaks = '3 day', date_minor_breaks = '1 day')


zero_no2 = rle(no2$zero_box) %>% 
  wsdmiscr::tidy_rle() %>% 
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble()

dat = no2 %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zero_no2, "idx") %>% 
  mutate(id = ifelse(is.na(id), 0, id)) %>% 
  group_by(id) %>% 
  nest_by() %>%
  mutate(
         zeroMed_no2 = ifelse(id == 0, NA, median(data$no2, na.rm = T)),
         zeroMean_no2 = ifelse(id == 0, NA, mean(data$no2, na.rm = T))) %>% 
  # zeroMed_no = ifelse(zeroMed_no > 0.25 | zeroMed_no < -0.25, NA, zeroMed_no),
  # zeroMed_no2 = ifelse(id == 0, NA, median(data$no2, na.rm = T))) %>%       
  # zeroMed_no2 = ifelse(zeroMed_no2 > 0, NA, zeroMed_no2)) %>%
  unnest(data) %>% 
  arrange(date) %>% 
  ungroup() %>%
  mutate(
         zero_no2_med = na.approx(zeroMed_no2,na.rm = F),
         zero_no2_mean = na.approx(zeroMean_no2,na.rm = F))
# zero_no2 = na.approx(zeroMed_no2,na.rm = F),
# no_corr = no - zero_no)
# no2_corr = no2-zero_no2)

test = dat %>% 
  filter(is.na(zeroMean_no2) == FALSE)
  # mutate(zeroMean_no1 = zeroMean_no - no_mean_mean)

no2_mean_mean = mean(unique(test$zeroMean_no2))
sd_mean = sd(unique(test$zeroMean_no2))
no2_med_mean = mean(unique(test$zeroMed_no2))
sd_med = sd(unique(test$zeroMed_no2))

dat %>% 
  filter(zero_box == 1,
  flag == 0) %>%
  pivot_longer(c(no2,zeroMed_no2,zeroMean_no2)) %>%
  ggplot(aes(date,value,col = name)) +
  geom_point() +
  # geom_abline(intercept = no_mean_mean,slope = 0,colour = 'red') +
  # geom_abline(slope = 0, intercept = no_med_mean, colour = 'blue') +
  # facet_grid(rows = vars(name), scales = "free") +
  scale_x_datetime(date_breaks = '3 day', date_minor_breaks = '1 day')

library(ncdf4)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(janitor)

setwd("D:/Cruise/Sea ice and snow/Sea ice and snow depth")


# Reading in data ---------------------------------------------------------

var = c("HDFEOS/GRIDS/NpPolarGrid12km/lat","HDFEOS/GRIDS/NpPolarGrid12km/lon","HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_ICECON_DAY","HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_SNOWDEPTH_5DAY")

june5 = nc_open("AMSR_U2_L3_SeaIce12km_B04_20220605.he5")

for (i in 1:length(var)) {
  vname <- var[i]
  raw <- as.vector(ncdf4::ncvar_get(june5,vname,collapse_degen=FALSE))
  if(i==1){
    dat <- data.frame(raw)
    names(dat) <- vname
  } 
  else {
    dat <- cbind(dat,raw)
    names(dat)[ncol(dat)] <- vname
  }
}

dat5 = dat %>% 
  rename(lat = `HDFEOS/GRIDS/NpPolarGrid12km/lat`,
         lon = `HDFEOS/GRIDS/NpPolarGrid12km/lon`,
         sea_ice = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_ICECON_DAY`,
         snow_depth = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_SNOWDEPTH_5DAY`) %>% 
  # filter(lon < -56 & lon > -57.5,
    # lat > 68 & lat < 69         ) %>% 
  mutate(date = as.POSIXct("2022-06-05"))

june6 = nc_open("AMSR_U2_L3_SeaIce12km_B04_20220606.he5")

for (i in 1:length(var)) {
  vname <- var[i]
  raw <- as.vector(ncdf4::ncvar_get(june6,vname,collapse_degen=FALSE))
  if(i==1){
    dat <- data.frame(raw)
    names(dat) <- vname
  } 
  else {
    dat <- cbind(dat,raw)
    names(dat)[ncol(dat)] <- vname
  }
}

dat6 = dat %>% 
  rename(lat = `HDFEOS/GRIDS/NpPolarGrid12km/lat`,
         lon = `HDFEOS/GRIDS/NpPolarGrid12km/lon`,
         sea_ice = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_ICECON_DAY`,
         snow_depth = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_SNOWDEPTH_5DAY`) %>% 
  # filter(lon < -56 & lon > -57.5,
         # lat > 68 & lat < 69         ) %>% 
  mutate(date = as.POSIXct("2022-06-06"))

june7 = nc_open("AMSR_U2_L3_SeaIce12km_B04_20220607.he5")

for (i in 1:length(var)) {
  vname <- var[i]
  raw <- as.vector(ncdf4::ncvar_get(june7,vname,collapse_degen=FALSE))
  if(i==1){
    dat <- data.frame(raw)
    names(dat) <- vname
  } 
  else {
    dat <- cbind(dat,raw)
    names(dat)[ncol(dat)] <- vname
  }
}

dat7 = dat %>% 
  rename(lat = `HDFEOS/GRIDS/NpPolarGrid12km/lat`,
         lon = `HDFEOS/GRIDS/NpPolarGrid12km/lon`,
         sea_ice = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_ICECON_DAY`,
         snow_depth = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_SNOWDEPTH_5DAY`) %>% 
  # filter(lon < -56 & lon > -57.5,
  # lat > 68 & lat < 69         ) %>% 
  mutate(date = as.POSIXct("2022-06-07"))

june8 = nc_open("AMSR_U2_L3_SeaIce12km_B04_20220608.he5")

for (i in 1:length(var)) {
  vname <- var[i]
  raw <- as.vector(ncdf4::ncvar_get(june8,vname,collapse_degen=FALSE))
  if(i==1){
    dat <- data.frame(raw)
    names(dat) <- vname
  } 
  else {
    dat <- cbind(dat,raw)
    names(dat)[ncol(dat)] <- vname
  }
}

dat8 = dat %>% 
  rename(lat = `HDFEOS/GRIDS/NpPolarGrid12km/lat`,
         lon = `HDFEOS/GRIDS/NpPolarGrid12km/lon`,
         sea_ice = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_ICECON_DAY`,
         snow_depth = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_SNOWDEPTH_5DAY`) %>% 
  # filter(lon < -56 & lon > -57.5,
  # lat > 68 & lat < 69         ) %>% 
  mutate(date = as.POSIXct("2022-06-08"))

june9 = nc_open("AMSR_U2_L3_SeaIce12km_B04_20220609.he5")

for (i in 1:length(var)) {
  vname <- var[i]
  raw <- as.vector(ncdf4::ncvar_get(june9,vname,collapse_degen=FALSE))
  if(i==1){
    dat <- data.frame(raw)
    names(dat) <- vname
  } 
  else {
    dat <- cbind(dat,raw)
    names(dat)[ncol(dat)] <- vname
  }
}

dat9 = dat %>% 
  rename(lat = `HDFEOS/GRIDS/NpPolarGrid12km/lat`,
         lon = `HDFEOS/GRIDS/NpPolarGrid12km/lon`,
         sea_ice = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_ICECON_DAY`,
         snow_depth = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_SNOWDEPTH_5DAY`) %>% 
  # filter(lon < -56 & lon > -57.5,
  # lat > 68 & lat < 69         ) %>% 
  mutate(date = as.POSIXct("2022-06-09"))

june10 = nc_open("AMSR_U2_L3_SeaIce12km_B04_20220610.he5")

for (i in 1:length(var)) {
  vname <- var[i]
  raw <- as.vector(ncdf4::ncvar_get(june10,vname,collapse_degen=FALSE))
  if(i==1){
    dat <- data.frame(raw)
    names(dat) <- vname
  } 
  else {
    dat <- cbind(dat,raw)
    names(dat)[ncol(dat)] <- vname
  }
}

dat10 = dat %>% 
  rename(lat = `HDFEOS/GRIDS/NpPolarGrid12km/lat`,
         lon = `HDFEOS/GRIDS/NpPolarGrid12km/lon`,
         sea_ice = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_ICECON_DAY`,
         snow_depth = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_SNOWDEPTH_5DAY`) %>% 
  # filter(lon < -56 & lon > -57.5,
  # lat > 68 & lat < 69         ) %>% 
  mutate(date = as.POSIXct("2022-06-10"))

june11 = nc_open("AMSR_U2_L3_SeaIce12km_B04_20220611.he5")

for (i in 1:length(var)) {
  vname <- var[i]
  raw <- as.vector(ncdf4::ncvar_get(june11,vname,collapse_degen=FALSE))
  if(i==1){
    dat <- data.frame(raw)
    names(dat) <- vname
  } 
  else {
    dat <- cbind(dat,raw)
    names(dat)[ncol(dat)] <- vname
  }
}

dat11 = dat %>% 
  rename(lat = `HDFEOS/GRIDS/NpPolarGrid12km/lat`,
         lon = `HDFEOS/GRIDS/NpPolarGrid12km/lon`,
         sea_ice = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_ICECON_DAY`,
         snow_depth = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_SNOWDEPTH_5DAY`) %>% 
  # filter(lon < -56 & lon > -57.5,
  # lat > 68 & lat < 69         ) %>% 
  mutate(date = as.POSIXct("2022-06-11"))

june12 = nc_open("AMSR_U2_L3_SeaIce12km_B04_20220612.he5")

for (i in 1:length(var)) {
  vname <- var[i]
  raw <- as.vector(ncdf4::ncvar_get(june12,vname,collapse_degen=FALSE))
  if(i==1){
    dat <- data.frame(raw)
    names(dat) <- vname
  } 
  else {
    dat <- cbind(dat,raw)
    names(dat)[ncol(dat)] <- vname
  }
}

dat12 = dat %>% 
  rename(lat = `HDFEOS/GRIDS/NpPolarGrid12km/lat`,
         lon = `HDFEOS/GRIDS/NpPolarGrid12km/lon`,
         sea_ice = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_ICECON_DAY`,
         snow_depth = `HDFEOS/GRIDS/NpPolarGrid12km/Data Fields/SI_12km_NH_SNOWDEPTH_5DAY`) %>% 
  # filter(lon < -56 & lon > -57.5,
  # lat > 68 & lat < 69         ) %>% 
  mutate(date = as.POSIXct("2022-06-12"))

dat = rbind(dat6,dat7,dat8,dat9,dat10,dat11,dat12)

write.csv(dat,"D:/Cruise/processed_data/SEANA_seaice_snow.csv")

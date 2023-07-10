#SEANA absolute winds from relative

#read in
met_data <- read.csv('D:/Cruise/raw_data/met_data.csv')
met_data$...1 <-  NULL #remove surplus row names
met_data <- na.omit(met_data) #remove rows with NAs

#rel_wd is 0 for a headwind, 180 for a a tailwind

#true wind direction
met_data$true_wd <- met_data$heading+met_data$rel_wd
met_data$true_wd[met_data$true_wd >= 360] <- met_data$true_wd[met_data$true_wd >=360] -360

#get relative wind vectors ('Earth frame': u +ve towards east, v +ve towards north)
#first define wd as 'wind towards', opposite to the 'from' convention
met_data$true_wd_towards <- met_data$true_wd+180
met_data$true_wd_towards[met_data$true_wd_towards >= 360] <- met_data$true_wd_towards[met_data$true_wd_towards >=360] -360
met_data$u_rel <- met_data$rel_ws*sin(met_data$true_wd_towards*pi/180)
met_data$v_rel <- met_data$rel_ws*cos(met_data$true_wd_towards*pi/180)

#get boat movement as u and v vectors
#/1.9438 for knots -> m s-1
met_data$ship_motion_u <- (met_data$speed/1.9438)*sin(met_data$heading*pi/180)
met_data$ship_motion_v <- (met_data$speed/1.9438)*cos(met_data$heading*pi/180)

#add ship movement to relative wind vectors to get true wind:
#(if the boat is moving north, the anemometer will see less southerly wind etc)
met_data$u <- met_data$u_rel+met_data$ship_motion_u
met_data$v <- met_data$v_rel+met_data$ship_motion_v

#final speed:
met_data$ws <- sqrt(met_data$u^2+met_data$v^2)

write.csv(met_data, "D:/Cruise/processed_data/met_data_processed.csv", row.names = F)

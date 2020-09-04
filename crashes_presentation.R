# setting the workind directory: ####
setwd("D:/Work/Fall_2019/Co-Op/20191114_presentation")

#loading relevant packages: ####
library(tidyverse)
library(ggmap)
library(lubridate)
library(dplyr)
library(ggplot2)
library(DescTools)

# loading required data: ####
pedcrash <- read.csv("ped_crash_select.csv", na.strings=c("","NA"))
biccrash <- read.csv("Bic_Crash_CY17_IMPACT.csv", na.strings=c("","NA"))

# Getting data on schools: ####
school_ped <- read.csv("school_zone_ped.csv", na.strings=c("","NA"))
school_ped$schoolzone <- rep("Within School Zone",nrow(school_ped))
pedcrash <- merge(pedcrash, school_ped[,c(2,117)], by.y = "crash_numb", by.x = "Crash.Number", all.x = T, all.y = F)
pedcrash$schoolzone[is.na(pedcrash$schoolzone)] <- "Not a school zone"

school_bic <- read.csv("school_zone_bike.csv", na.strings=c("","NA"))
school_bic$schoolzone <- rep("Within School Zone",nrow(school_bic))
biccrash <- merge(biccrash, school_bic[,c(2,117)], by = "crash_numb", all.x = T, all.y = F)
biccrash$schoolzone[is.na(biccrash$schoolzone)] <- "Not a school zone"

# Parsing Action of Vehicle before crash: ####
biccrash$veh_act <- ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Travelling straight ahead"), "Travelling straight ahead",
                           ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Other"), "Other",
                                  ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "right"), "Turning right",
                                         ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "left"), "Turning left",
                                                ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Slowing or stopped in traffic"), "Slowing or stopped in traffic",
                                                       ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Parked"), "Parked",
                                                              ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Entering traffic lane"), "Entering traffic lane",
                                                                     ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Overtaking/passing"), "Overtaking/passing",
                                                                            ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Changing lanes"), "Changing lanes",
                                                                                   ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Not reported"), "Not reported",
                                                                                          ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Unknown"), "Unknown",
                                                                                                 ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Leaving traffic lane"), "Leaving traffic lane",
                                                                                                        ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Backing"), "Backing",
                                                                                                               ifelse(str_detect(biccrash$vehc_mnvr_actn_cl, pattern = "Making U-turn"), "Making U-turn",
                                                                                                                      NA))))))))))))))

pedcrash$veh_act <- ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Travelling straight ahead"), "Travelling straight ahead",
                           ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Other"), "Other",
                                  ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "right"), "Turning right",
                                         ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "left"), "Turning left",
                                                ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Slowing or stopped in traffic"), "Slowing or stopped in traffic",
                                                       ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Parked"), "Parked",
                                                              ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Entering traffic lane"), "Entering traffic lane",
                                                                     ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Overtaking/passing"), "Overtaking/passing",
                                                                            ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Changing lanes"), "Changing lanes",
                                                                                   ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Not reported"), "Not reported",
                                                                                          ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Unknown"), "Unknown",
                                                                                                 ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Leaving traffic lane"), "Leaving traffic lane",
                                                                                                        ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Backing"), "Backing",
                                                                                                               ifelse(str_detect(pedcrash$Vehicle.Actions.Prior.to.Crash..All.Vehicles., pattern = "Making U-turn"), "Making U-turn",
                                                                                                                      NA))))))))))))))
# Parsing Time of Day for crash: ####

pedcrash$timeofday <- parse_time((as.character(pedcrash$Crash.Time)), "")
biccrash$timeofday <- parse_time((as.character(biccrash$crash_time_2)), "")

# Marking Age Factor Levels correctly:

pedcrash$Age.of.Non.Motorist...Youngest.Known <- str_replace(pedcrash$Age.of.Non.Motorist...Youngest.Known, "<6", "Younger than 6")
pedcrash$Age.of.Non.Motorist...Youngest.Known <- str_replace(pedcrash$Age.of.Non.Motorist...Youngest.Known, ">84", "Older than 74")
pedcrash$Age.of.Non.Motorist...Youngest.Known <- str_replace(pedcrash$Age.of.Non.Motorist...Youngest.Known, "6-15", "6 to 15")
pedcrash$Age.of.Non.Motorist...Youngest.Known <- str_replace(pedcrash$Age.of.Non.Motorist...Youngest.Known, "16-20", "16 to 20")
pedcrash$Age.of.Non.Motorist...Youngest.Known <- str_replace(pedcrash$Age.of.Non.Motorist...Youngest.Known, "21-24", "21 to 34")
pedcrash$Age.of.Non.Motorist...Youngest.Known <- str_replace(pedcrash$Age.of.Non.Motorist...Youngest.Known, "25-34", "21 to 34")
pedcrash$Age.of.Non.Motorist...Youngest.Known <- str_replace(pedcrash$Age.of.Non.Motorist...Youngest.Known, "35-44", "35 to 54")
pedcrash$Age.of.Non.Motorist...Youngest.Known <- str_replace(pedcrash$Age.of.Non.Motorist...Youngest.Known, "45-54", "35 to 54")
pedcrash$Age.of.Non.Motorist...Youngest.Known <- str_replace(pedcrash$Age.of.Non.Motorist...Youngest.Known, "55-64", "55 to 74")
pedcrash$Age.of.Non.Motorist...Youngest.Known <- str_replace(pedcrash$Age.of.Non.Motorist...Youngest.Known, "65-74", "55 to 74")
pedcrash$Age.of.Non.Motorist...Youngest.Known <- str_replace(pedcrash$Age.of.Non.Motorist...Youngest.Known, "75-84", "Older than 74")
pedcrash$Age.of.Non.Motorist...Youngest.Known <- factor(pedcrash$Age.of.Non.Motorist...Youngest.Known, levels = c("Younger than 6",
                                                                                                                  "6 to 15",
                                                                                                                  "16 to 20",
                                                                                                                  "21 to 34",
                                                                                                                  "35 to 54",
                                                                                                                  "55 to 74",
                                                                                                                  "Older than 74"))
biccrash$age_nonmtrst_yngst <- str_replace(biccrash$age_nonmtrst_yngst, "<6", "Younger than 6")
biccrash$age_nonmtrst_yngst <- str_replace(biccrash$age_nonmtrst_yngst, ">84", "Older than 74")
biccrash$age_nonmtrst_yngst <- str_replace(biccrash$age_nonmtrst_yngst, "15-Jun", "6 to 15")
biccrash$age_nonmtrst_yngst <- str_replace(biccrash$age_nonmtrst_yngst, "16-20", "16 to 20")
biccrash$age_nonmtrst_yngst <- str_replace(biccrash$age_nonmtrst_yngst, "21-24", "21 to 34")
biccrash$age_nonmtrst_yngst <- str_replace(biccrash$age_nonmtrst_yngst, "25-34", "21 to 34")
biccrash$age_nonmtrst_yngst <- str_replace(biccrash$age_nonmtrst_yngst, "35-44", "35 to 54")
biccrash$age_nonmtrst_yngst <- str_replace(biccrash$age_nonmtrst_yngst, "45-54", "35 to 54")
biccrash$age_nonmtrst_yngst <- str_replace(biccrash$age_nonmtrst_yngst, "55-64", "55 to 74")
biccrash$age_nonmtrst_yngst <- str_replace(biccrash$age_nonmtrst_yngst, "65-74", "55 to 74")
biccrash$age_nonmtrst_yngst <- str_replace(biccrash$age_nonmtrst_yngst, "75-84", "Older than 74")
biccrash$age_nonmtrst_yngst <- factor(biccrash$age_nonmtrst_yngst, levels = c("Younger than 6",
                                                                              "6 to 15",
                                                                              "16 to 20",
                                                                              "21 to 34",
                                                                              "35 to 54",
                                                                              "55 to 74",
                                                                              "Older than 74"))

# Getting a subset of severe crashes for both bike and ped: ####
pedcrash$ifsevere <- ifelse(str_detect(pedcrash$Max.Injury.Severity.Reported ,pattern ="Non-fatal injury - Incapacitating"),"Severe",NA)
pedcrash_severe <- subset(pedcrash, pedcrash$ifsevere == "Severe")

biccrash$ifsevere <- ifelse(str_detect(biccrash$max_injr_svrty_cl ,pattern ="Non-fatal injury - Incapacitating"),"Severe",NA)
biccrash_severe <- subset(biccrash, biccrash$ifsevere == "Severe")

# Bike and Ped Vehicle Configuration: ####

ped_vehc <- Freq(pedcrash$Vehicle.Configuration..All.Vehicles.)
write_csv(ped_vehc, 
          path = paste("ped_vehc", '.csv'))

bike_vehc <- Freq(biccrash$vehc_config_cl)
write_csv(bike_vehc, 
          path = paste("bike_vehc", '.csv'))


ped_vehc_sv <- Freq(pedcrash_severe$Vehicle.Configuration..All.Vehicles.)
write_csv(ped_vehc_sv, 
          path = paste("ped_vehc_sv", '.csv'))

bike_vehc_sv <- Freq(biccrash_severe$vehc_config_cl)
write_csv(bike_vehc_sv, 
          path = paste("bike_vehc_sv", '.csv'))

# Hit and Run: ####

ped_hitrun <- Freq(pedcrash$Hit.and.Run)
bike_hitrun <- Freq(biccrash$hit_run_descr)
ped_hitrun_sv <- Freq(pedcrash_severe$Hit.and.Run)
bike_hitrun_sv <- Freq(biccrash_severe$hit_run_descr)

# Road Intersections: ####

ped_rdway <- Freq(pedcrash$Roadway.Junction.Type)
write_csv(ped_rdway, 
          path = paste("ped_rdway", '.csv'))

bike_rdway <- Freq(biccrash$rdwy_jnct_type_descr)
write_csv(bike_rdway, 
          path = paste("bike_rdway", '.csv'))


ped_rdway_sv <- Freq(pedcrash_severe$Roadway.Junction.Type)
write_csv(ped_rdway_sv, 
          path = paste("ped_rdway_sv", '.csv'))

bike_rdway_sv <- Freq(biccrash_severe$rdwy_jnct_type_descr)
write_csv(bike_rdway_sv, 
          path = paste("bike_rdway_sv", '.csv'))

# Getting a subset of T-intersections, and understanding the manner of collision: ####

ped_t_junc <- subset(pedcrash, pedcrash$Roadway.Junction.Type == "T-intersection")
ped_t_junc_sv <- subset(pedcrash_severe, pedcrash_severe$Roadway.Junction.Type == "T-intersection")

bic_t_junc <- subset(biccrash, biccrash$rdwy_jnct_type_descr == "T-intersection")
bic_t_junc_sv <- subset(biccrash_severe, biccrash_severe$rdwy_jnct_type_descr == "T-intersection")

# Getting a subset all Intersections, and understanding the manner of collision: ####

ped_junc <- subset(pedcrash, str_detect(pedcrash$Roadway.Junction.Type, "intersection"))
ped_junc_sv <- subset(pedcrash_severe, str_detect(pedcrash$Roadway.Junction.Type, "intersection"))

bic_junc <- subset(biccrash, str_detect(biccrash$rdwy_jnct_type_descr, "intersection"))
bic_junc_sv <- subset(biccrash_severe, str_detect(biccrash$rdwy_jnct_type_descr, "intersection"))

ped_junc_act <- Freq(ped_junc$veh_act)
write_csv(ped_junc_act, 
          path = paste("ped_junc_act", '.csv'))

ped_junc_act_sv <- Freq(ped_junc_sv$veh_act)
write_csv(ped_junc_act_sv, 
          path = paste("ped_junc_act_sv", '.csv'))


bic_junc_act <- Freq(bic_junc$veh_act)
write_csv(bic_junc_act, 
          path = paste("bic_junc_act", '.csv'))

bic_junc_act_sv <- Freq(bic_junc_sv$veh_act)
write_csv(bic_junc_act_sv, 
          path = paste("bic_junc_act_sv", '.csv'))

##

ped_veh_act <- Freq(pedcrash$veh_act)
write_csv(ped_veh_act, 
          path = paste("ped_veh_act", '.csv'))

bike_veh_act <- Freq(biccrash$veh_act)
write_csv(bike_veh_act, 
          path = paste("bike_veh_act", '.csv'))


ped_veh_act_sv <- Freq(pedcrash_severe$veh_act)
write_csv(ped_veh_act_sv, 
          path = paste("ped_veh_act_sv", '.csv'))

bike_veh_act_sv <- Freq(biccrash_severe$veh_act)
write_csv(bike_veh_act_sv, 
          path = paste("bike_veh_act_sv", '.csv'))

##

ped_veh_act_tjunc <- Freq(ped_t_junc$veh_act)
write_csv(ped_veh_act_tjunc, 
          path = paste("ped_veh_act_tjunc", '.csv'))

bike_veh_act_tjunc <- Freq(bic_t_junc$veh_act)
write_csv(bike_veh_act_tjunc, 
          path = paste("bike_veh_act_tjunc", '.csv'))


ped_veh_act_tjunc_sv <- Freq(ped_t_junc_sv$veh_act)
write_csv(ped_veh_act_tjunc_sv, 
          path = paste("ped_veh_act_tjunc_sv", '.csv'))

bike_veh_act_tjunc_sv <- Freq(bic_t_junc_sv$veh_act)
write_csv(bike_veh_act_tjunc_sv, 
          path = paste("bike_veh_act_tjunc_sv", '.csv'))

# Age of Crash Victim and Time of Day: ####


# Percentage Density:

pedcrash_ages <- pedcrash[!is.na(pedcrash$Age.of.Non.Motorist...Youngest.Known),]
pedcrash_ages_sv <- pedcrash_severe[!is.na(pedcrash_severe$Age.of.Non.Motorist...Youngest.Known),]

ggplot()+
  geom_histogram(data = pedcrash_ages, 
                 aes(x = timeofday, fill = Age.of.Non.Motorist...Youngest.Known), 
                 position = "fill",bins = 48 )+
  theme_minimal()+
  scale_fill_brewer(palette = "RdYlGn")+
  xlab("Time of the Day")+ylab("Percentage")+ggtitle("Pedestrian Crashes: Age of Youngest non-Motorist")+
  scale_x_time(breaks = c(0,7200,14400,21600,28800,36000,43200,50400,57600,64800,72000,79200,86400))+
  scale_y_continuous(labels = scales::percent_format())+
  ggsave("pedcrash_age_timeofday.svg", h = 10, w = 15)

##

biccrash_ages <- biccrash[!is.na(biccrash$age_nonmtrst_yngst),]
biccrash_ages_sv <- biccrash_severe[!is.na(biccrash_severe$age_nonmtrst_yngst),]

ggplot()+
  geom_histogram(data = biccrash_ages, 
                 aes(x = timeofday, fill = age_nonmtrst_yngst), 
                 position = "fill",bins = 48 )+
  theme_minimal()+
  scale_fill_brewer(palette = "RdYlGn")+
  xlab("Time of the Day")+ylab("Percentage")+ggtitle("Bike Crashes: Age of Youngest non-Motorist")+
  scale_x_time(breaks = c(0,7200,14400,21600,28800,36000,43200,50400,57600,64800,72000,79200,86400))+
  scale_y_continuous(labels = scales::percent_format())+
  ggsave("biccrash_age_timeofday.svg", h = 10, w = 15)

# Volume Density:

ggplot()+
  geom_histogram(data = pedcrash_ages, 
                 aes(x = timeofday),
                 bins = 48 )+
  theme_minimal()+
  xlab("Time of the Day")+ylab("Count")+ggtitle("Pedestrian Crashes: Age of Youngest non-Motorist")+
  scale_x_time(breaks = c(0,7200,14400,21600,28800,36000,43200,50400,57600,64800,72000,79200,86400))+
  ggsave("pedcrash_count_timeofday.svg", h = 10, w = 15)

ggplot()+
  geom_histogram(data = biccrash_ages, 
                 aes(x = timeofday),
                 bins = 48 )+
  theme_minimal()+
  xlab("Time of the Day")+ylab("Count")+ggtitle("Bike Crashes: Age of Youngest non-Motorist")+
  scale_x_time(breaks = c(0,7200,14400,21600,28800,36000,43200,50400,57600,64800,72000,79200,86400))+
  ggsave("biccrash_count_timeofday.svg", h = 10, w = 15)

# Checking for school zone or not:

ped_schlzn <- Freq(pedcrash$schoolzone)
write_csv(ped_schlzn, 
          path = paste("ped_schlzn", '.csv'))

ped_schlzn_sv <- Freq(pedcrash_severe$schoolzone)
write_csv(ped_schlzn_sv, 
          path = paste("ped_schlzn_sv", '.csv'))

pedcrash_schlzone <- subset(pedcrash, pedcrash$schoolzone == "Within School Zone")
pedcrash_schlzone <- pedcrash_schlzone[!is.na(pedcrash_schlzone$Age.of.Non.Motorist...Youngest.Known),]

ggplot()+
  geom_histogram(data = pedcrash_schlzone, 
                 aes(x = schoolzone, fill = Age.of.Non.Motorist...Youngest.Known),
                 stat = "count", position = "fill",
                 bins = 48 )+
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal()+
  scale_fill_brewer(palette = "RdYlGn")+
  ylab("Count")+ggtitle("Pedestrian Crashes in School Zones: Age of Crash Victim")+
  ggsave("schoolzone_peds_age.svg", h = 10, w = 15)

pedcrash_nonschlzone <- subset(pedcrash, pedcrash$schoolzone == "Not a school zone")

pedcrash_nonschlzone <- pedcrash_nonschlzone[!is.na(pedcrash_nonschlzone$Age.of.Non.Motorist...Youngest.Known),]

ggplot()+
  geom_histogram(data = pedcrash_nonschlzone, 
                 aes(x = schoolzone, fill = Age.of.Non.Motorist...Youngest.Known),
                 stat = "count", position = "fill",
                 bins = 48 )+
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal()+
  scale_fill_brewer(palette = "RdYlGn")+
  ylab("Count")+ggtitle("Pedestrian Crashes outside School Zones: Age of Crash Victim")+
  ggsave("nonschoolzone_ped_age.svg", h = 10, w = 15)

##

bike_schlzn <- Freq(biccrash$schoolzone)
write_csv(bike_schlzn, 
          path = paste("bike_schlzn", '.csv'))

bike_schlzn_sv <- Freq(biccrash_severe$schoolzone)
write_csv(bike_schlzn_sv, 
          path = paste("bike_schlzn_sv", '.csv'))

biccrash_schlzone <- subset(biccrash, biccrash$schoolzone == "Within School Zone")

biccrash_schlzone <- biccrash_schlzone[!is.na(biccrash_schlzone$age_nonmtrst_yngst),]

ggplot()+
  geom_histogram(data = biccrash_schlzone, 
                 aes(x = schoolzone, fill = age_nonmtrst_yngst),
                 stat = "count", position = "fill",
                 bins = 48 )+
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal()+
  scale_fill_brewer(palette = "RdYlGn")+
  ylab("Count")+ggtitle("Bike Crashes in School Zones: Age of Crash Victim")+
  ggsave("schoolzone_bike_age.svg", h = 10, w = 15)
##
biccrash_nonschlzone <- subset(biccrash, biccrash$schoolzone == "Not a school zone")

biccrash_nonschlzone <- biccrash_nonschlzone[!is.na(biccrash_nonschlzone$age_nonmtrst_yngst),]

ggplot()+
  geom_histogram(data = biccrash_nonschlzone, 
                 aes(x = schoolzone, fill = age_nonmtrst_yngst),
                 stat = "count", position = "fill",
                 bins = 48 )+
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal()+
  scale_fill_brewer(palette = "RdYlGn")+
  ylab("Count")+ggtitle("Bike Crashes outside School Zones: Age of Crash Victim")+
  ggsave("nonschoolzone_bike_age.svg", h = 10, w = 15)

#####################################################################################################################################################


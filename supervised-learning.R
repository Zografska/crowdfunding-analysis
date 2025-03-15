# supervised-learning on the car accident data
# hypothesis - younger drivers cause more severe car accidents
# https://www.gov.uk/government/collections/road-accidents-and-safety-statistics
# the goal is to predict the severity of the car accident

#unfiltered_casualty_data <- read.csv("data/casualty.csv")
unfiltered_vehicle <- read.csv("data/vehicle.csv")
unfiltered_accident <- read.csv("data/road-data.csv")

all_data <- merge(unfiltered_casualty_data, unfiltered_vehicle, by = "accident_reference")
all_data <- merge(all_data, unfiltered_accident, by = "accident_reference")


# get only the accidents where there is only one vehicle to ensure independance
current <- all_data[all_data$number_of_vehicles == 1,]
dim(current)

# let's choose variables
# accident_ref_no
# accident_severity: 1 (Fatal), 2 (Serious), 3 (Slight)
# number_of_vehicles
# number_of_casualties
# date - to transform it into month
# time - to transform it into hour
# day_of_week: 1 (Sunday), 2 (Monday), 3 (Tuesday), 4 (Wednesday), 5 (Thursday), 6 (Friday), 7 (Saturday)
# enhanced_collision_severity: 1 (Fatal), 5 (Very Serious), 6 (Moderately Serious), 7 (Less Serious), 3 (Slight), -1 (Data missing or out of range)
# road_type: 1 (Roundabout), 2 (One way street), 3 (Dual carriageway), 6 (Single carriageway), 7 (Slip road), 9 (Unknown), 12 (One way street/Slip road), -1 (Data missing or out of range)
# speed_limit: 20, 30, 40, 50, 60, 70, 99(self-reported), -1 (Data missing or out of range)
# weather_conditions: 1 (Fine no high winds), 2 (Raining no high winds), 3 (Snowing no high winds), 4 (Fine + high winds), 5 (Raining + high winds), 6 (Snowing + high winds), 7 (Fog or mist), 8 (Other), 9 (Unknown), -1 (Data missing or out of range)
# road_surface_conditions: 1 (Dry), 2 (Wet or damp), 3 (Snow), 4 (Frost or ice), 5 (Flood over 3cm. deep), 6 (Oil or diesel), 7 (Mud), -1 (Data missing or out of range), 9 (unknown (self reported))
# vehicle_type: 1 (Pedal cycle), 2 (Motorcycle 50cc and under), 3 (Motorcycle 125cc and under), 4 (Motorcycle over 125cc and up to 500cc), 5 (Motorcycle over 500cc), 8 (Taxi/Private hire car), 9 (Car), 10 (Minibus (8 - 16 passenger seats)), 11 (Bus or coach (17 or more pass seats)), 16 (Ridden horse), 17 (Agricultural vehicle), 18 (Tram), 19 (Van / Goods 3.5 tonnes mgw or under), 20 (Goods over 3.5t. and under 7.5t), 21 (Goods 7.5 tonnes mgw and over), 22 (Mobility scooter), 23 (Electric motorcycle), 90 (Other vehicle), 97 (Motorcycle - unknown cc), 98 (Goods vehicle - unknown weight), 99 (Unknown vehicle type (self rep only)), 103 (Motorcycle - Scooter (1979-1998)), 104 (Motorcycle (1979-1998)), 105 (Motorcycle - Combination (1979-1998)), 106 (Motorcycle over 125cc (1999-2004)), 108 (Taxi (excluding private hire cars) (1979-2004)), 109 (Car (including private hire cars) (1979-2004)), 110 (Minibus/Motor caravan (1979-1998)), 113 (Goods over 3.5 tonnes (1979-1998)), -1 (Data missing or out of range)
# urban_or_rural_area: 1 (Urban), 2 (Rural), 3 (Unallocated), -1 (Data missing or out of range)
# sex_of_driver: 1 (Male), 2 (Female), 3 (Not known), -1 (Data missing or out of range)
# age_of_driver: -1 (Data missing or out of range)
# escooter_flag: 1 (Vehicle was an e-scooter), 0 (Vehicle was not an e-scooter)
# age_of_vehicle: -1 (Data missing or out of range)
# car_passenger: 0 (Not car passenger), 1 (Front seat passenger), 2 (Rear seat passenger), 9 (unknown (self reported)), -1 (Data missing or out of range)

# !! | current$enhanced_collision_severity == -1 
#remove data with -1 values for all columns
# look at age of vehicle also
current <- all_data[all_data$number_of_vehicles == 1,]
current <- current[!(current$accident_severity == -1 | current$number_of_casualties == -1 | current$day_of_week == -1 | current$speed_limit == -1 | current$weather_conditions == -1 | current$road_surface_conditions == -1 | current$urban_or_rural_area == -1 | current$engine_capacity_cc == -1 | current$age_of_driver == -1),]
current <- current[!(current$sex_of_driver == 3),]

# is iid?
length(unique(current$accident_reference)) == nrow(current)


# merge columns with same accident referance
library(dplyr)
current <- current %>% group_by(accident_reference) %>% summarise(number_of_casualties = sum(number_of_casualties), accident_severity = mean(accident_severity), number_of_vehicles = mean(number_of_vehicles), day_of_week = mean(day_of_week), speed_limit = mean(speed_limit), weather_conditions = mean(weather_conditions), road_surface_conditions = mean(road_surface_conditions), urban_or_rural_area = mean(urban_or_rural_area), engine_capacity_cc = mean(engine_capacity_cc), age_of_driver = mean(age_of_driver), sex_of_driver = mean(sex_of_driver))
summary(current)

# us iid after processing
length(unique(current$accident_reference)) == nrow(current)

# were road conditions good?
# speed_limit good_surface_conditions
current$good_surface_conditions <- ifelse(current$road_surface_conditions == 1, 1, 0)
current$good_weather_conditions <- ifelse(current$weather_conditions == 1, 1, 0)

#think about it
#split string by delimiter
#is_at_night <- function (time) {(stringr::str_split(time, "[:]"))[[1]][1] %in% c("20","21","22","23","00","01","02","03")}
#is_at_night(current$time)

current$is_male <- ifelse(current$sex_of_driver == 1, 1, 0)
current$is_weekend <- ifelse(current$day_of_week %in% c(1,6,7),1,0)
current$is_urban <- ifelse(current$urban_or_rural_area == 1, 1, 0)

set <- current %>% select(accident_severity, speed_limit, good_surface_conditions, good_weather_conditions, number_of_casualties, is_weekend, is_male, age_of_driver, is_urban, engine_capacity_cc)


dataset <- set[sample(nrow(set), 1000),]


summary(dataset)

summary(dataset$accident_severity)
qqnorm(dataset$age_of_driver)
log(dataset$age_of_driver)
shapiro.test(log(dataset$age_of_driver))

# normality??


library(ggpubr)

ggqqplot(dataset$age_of_driver)
ggdensity(dataset, x = "age_of_driver", fill = "lightgray", title = "age_of_driver") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

dataset$log_age = log(dataset$age_of_driver)

ggqqplot(dataset$age_of_driver)
ggdensity(dataset, x = "log_age", fill = "lightgray", title = "log_age") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# bad in both cases


# let's try a box-cox transformation,

#library(MASS)
#y = dataset$age_of_driver
#hist(y,breaks = 12)
#result = boxcox(y~1, lambda = seq(-5,5,0.5))
#mylambda = result$x[which.max(result$y)]
#mylambda
#y2 = (y^mylambda-1)/mylambda
#hist(y2)
#shapiro.test(y2) # stil bad

res<-cor(dataset) 
round(res, 2)
symnum(res, abbr.colnames = FALSE)



unfiltered_data <- read.csv("data/players_22.csv")
summary(unfiltered_data)
head(unfiltered_data)
as.factor(unfiltered_data$club_position)
# get by club position ST, LW or RW
striker_stat = unfiltered_data[unfiltered_data$club_position %in% c("ST", "LW", "RW"),]
dim(striker_stat)
# drop non-numerical columns
cstat = striker_stat[, c("overall", "potential", "value_eur", "wage_eur", "age")]

dim(cstat)

rownames(cstat) <- unfiltered_data$country
summary(cstat)

library(FactoMineR)
library(factoextra)
library(mclust)
library(cluster)
library(factoextra) 
library(caret)
library(lattice)

# drop missing values (for now)
cstat <- na.omit(cstat)
summary(cstat)

pre <- preProcess(as.data.frame(cstat), method = 'knnImpute', k = 4)
imputed <- predict(pre, cstat)
imputed
summary(imputed)

# plot the data

mc_fit <- Mclust(imputed)
summary(mc_fit)
plot(mc_fit)
# from the plot we can see some strong correlation between the overall and the potential
# we can also see that the wage and the value are not correlated with the other variables
# we can also see that the age is not correlated with the other variables


# since we're analyzing striker data, let's add some more variables
# in relation to that position
# we can add the finishing, the shot power, the long shots, the volleys, the penalties, the composure, the vision, the crossing, the short passing, the long passing, the curve, the agility, the balance, the reactions, the ball control, the dribbling, the composure, the interceptions, the heading accuracy, the marking, the standing tackle, the sliding tackle, the jumping, the stamina, the strength, the aggression, the positioning, the acceleration, the sprint speed, the agility, the balance, the reactions, the ball control, the dribbling, the composure, the interceptions, the heading accuracy, the marking, the standing tackle, the sliding tackle, the jumping, the stamina, the strength, the aggression, the positioning, the acceleration, the sprint speed, the agility, the balance, the reactions, the ball control, the dribbling, the composure, the interceptions, the heading accuracy, the marking, the standing tackle, the sliding tackle, the jumping, the stamina, the strength, the aggression, the positioning, the acceleration, the sprint speed, the agility, the balance, the reactions, the ball control, the dribbling, the composure, the interceptions, the heading accuracy, the marking, the standing tackle, the sliding tackle, the jumping, the stamina, the strength, the aggression, the positioning, the acceleration, the sprint speed, the agility, the balance, the reactions, the ball control, the dribbling, the composure, the interceptions, the heading accuracy, the marking, the standing tackle, the sliding tackle, the jumping, the stamina, the strength, the aggression, the positioning, the acceleration, the sprint speed, the agility, the balance, the reactions, the ball control, the dribbling, the composure, the interceptions, the heading accuracy, the marking, the standing tackle, the sliding tackle, the jumping, the stamina, the strength, the aggression, the positioning, the acceleration, the sprint speed, the agility, the balance, the reactions, the ball control, the dribbling, the composure, the interceptions, the heading accuracy, the marking, the standing tackle, the sliding tackle, the jumping, the stamina, the strength, the aggression, the positioning, the acceleration, the sprint speed, the agility, the balance, the reactions, the ball control, the dribbling, the composure, the interceptions, the heading accuracy, the
cstat = striker_stat[, c("overall", "potential", "wage_eur", "age", "shooting", "dribbling",  "attacking_finishing")]


d <- dist(imputed) # Euclidean distance by default
sil_mclust <- silhouette(3 , d)


summary(sil_mclust)
# Plot the silhouette
plot(sil_mclust, main = "Silhouette Plot for Mclust Clusters")

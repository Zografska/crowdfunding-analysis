library("dplyr")

#sqrt_age, height_cm_capped, weight_kg_capped,
#club_contract_valid_until, sqrt_pace_dribbling,
#defending_physic, work_rate_attack, work_rate_defense

dataset<-new_fifa %>% dplyr::select(wage_eur, potential, preferred_foot_right, weak_foot, age, defending_physic, work_rate_attack, work_rate_defense, work_rate_defense, height_cm_capped, weight_kg_capped, pace_dribbling)

summary(dataset$wage_eur)
boxplot(dataset$wage_eur)

library("rpart")
library("rpart.plot")
library("party") 
library("partykit")

# splitting
set.seed(101)
train.idx <- sample(nrow(dataset), 2/3 * nrow(dataset)) 
data.train <- dataset[train.idx, ]
data.test <- dataset[-train.idx, ]

pptree<-ctree(wage_eur ~ ., data=data.train, control = ctree_control(mincriterion=0.99999, minsplit=0, minbucket=100))
plot(ptree)
# tree is chaotic, there is a lot of splits
# need a smaller tree


dim(data.train)
rtree<-rpart(wage_eur ~ ., data=data.train, cp=0.005)
printcp(rtree)

# rel error
# As splits are added, 
# the relative error decreases, meaning the model is explaining more variance.

# xerror
# This is the error measured using cross-validation,
# which helps estimate how well the tree generalizes to unseen data.

# it looks like it stabilazes around 8 splits, so I prune at that cp value
pruned_tree <- prune(rtree, cp = 0.0117)
rpart.plot(pruned_tree)

# we can see that we have a pretty unbalanced three
# since a lot of the data is focused on the left side

library(randomForest)

rf = randomForest(wage_eur ~ ., data=data.train, ntree=50, mtry=3, importance=TRUE)
varImpPlot(rf)

pred <- predict(rf,data.train[,-1])
plot(data.train[,1],pred)

cor(data.train[,1],pred)

# check performance on the test data
prediction <- predict(rf,data.test[,-1])
plot(data.test[,1],prediction)

cor(data.test[,1],prediction)

# let's see if we can make it better
# find the best value for ntree

ntree_values = c(50, 75, 100, 125, 150, 200)
correlation_values = c()

for (ntree in ntree_values){
  rf = randomForest(wage_eur ~ ., data=data.train, ntree=ntree, mtry=3, importance=TRUE)
  pred <- predict(rf,data.test[,-1])
  correlation_values = c(correlation_values, cor(data.test[,1],pred))
}

plot(ntree_values, correlation_values, type="l", xlab="ntree", ylab="correlation")
# best value
print(correlation_values[4])

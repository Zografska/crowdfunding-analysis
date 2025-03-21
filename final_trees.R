library("rpart")
library("rpart.plot")
library("party") 
library("partykit")
library("dplyr")
# splitting
set.seed(101)
for_tree <- dataset %>% dplyr::select(success_status, is_donation, is_all_or_nothing,project_duration_days,
                                                  promo_video_length,frequently_asked_questions_numb,has_website, has_social_media, have_followers, 
                                                  log_social_media_followers, description_word_count,is_fongogo, in_big_city, has_female_owner, log_funding_target)

train.idx <- sample(nrow(for_tree), 2/3 * nrow(for_tree)) 
data.train <- for_tree[train.idx, ]
data.test <- for_tree[-train.idx, ]

pptree<-ctree(success_status ~ ., data=data.train, control = ctree_control(mincriterion=0.5, minsplit=0, minbucket=100))
pptree
plot(pptree)

dim(data.train)
rtree<-rpart(success_status ~ ., data=data.train, cp=0.005)
printcp(rtree)
plotcp(rtree)
summary(rtree)


## from group proj


set.seed(101)
# after a couple of tries, best split for the balance between the validation error and relational error
train.idx <- sample(nrow(for_tree), 4/5 * nrow(for_tree)) 
data.train <- for_tree[train.idx, ]
data.test <- for_tree[-train.idx, ]

pptree<-ctree(success_status ~ ., data=data.train, control = ctree_control(mincriterion=0.01, minsplit=0, minbucket=10))
plot(pptree)
# tree is chaotic, there is a lot of splits
# need a smaller tree


dim(data.train)
rtree<-rpart(success_status ~ ., data=data.train, cp=0.0001)
printcp(rtree)

# rel error
# As splits are added, 
# the relative error decreases, but at a point, the cv error increases also
# we need to stop at 17 where cv error is one of the lowest, 
# and rel error seems to stabilize and also the split is still a smaller number than the others


# xerror
# This is the error measured using cross-validation,
# which helps estimate how well the tree generalizes to unseen data.

# it looks like it stabilazes around 17 splits, so I prune at that cp value
pruned_tree <- prune(rtree, cp = 0.0065574)
rpart.plot(pruned_tree)

# do predictions with pruned tree
tree_predictions <- predict(pruned_tree, data.test, type = "class")
tree_confusion_matrix <- table(Predicted = tree_predictions, Actual = data.test$success_status)
tree_test_results <- diagnose_confusion_matrix(tree_confusion_matrix)

glm_model <- fit_glm(data.train, step_direction = "forward")
glm_predictions <- predict_glm(glm_model$model, data.test, is_youden = FALSE)
glm_confusion_matrix <- table(Predicted = glm_predictions$predicitons, Actual = data.test$success_status)
glm_results <- diagnose_confusion_matrix(glm_confusion_matrix)
glm_confusion_matrix

comparison_table <- rbind(tree_test_results, glm_results)
colnames(comparison_table) <- c("Sensitivity", "Specificity", "Percision")
rownames(comparison_table) <- c("Tree", "GLM")
comparison_table
# the GLM has higher sensitivity, but the tree has higher specificity and percision


# use AUC to compare models
library(dplyr)
library(MASS)
library(pROC)
summary(dataset)

regression_variables <- dataset %>% dplyr::select(success_status, is_donation, is_all_or_nothing,project_duration_days,
                                           promo_video_length,frequently_asked_questions_numb,has_website, has_social_media, have_followers, 
                                           log_social_media_followers, description_word_count,is_fongogo, in_big_city, has_female_owner, log_funding_target,
                                           generalized_category)
dim(regression_variables)
mod<-glm(success_status ~., data = regression_variables, family=binomial(link='logit'))
summary(mod)

# fit logistic regression model with all the variables
full_model <- glm(success_status ~ ., data = regression_variables, family = binomial(link='logit'))

# function that does stepwise selection and returns the best model,
# along with statistics for accuracy specificity and percision
# and the ROC curve
# params
# variables - the dataset
# is_youden - if true, calculate youden index, else euclidian index

diagnose_confusion_matrix <- function(confusion_matrix){
  sensitivity <- confusion_matrix[1,1] / (confusion_matrix[1,1] + confusion_matrix[2,1])
  specificity <- confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[1,2])
  percision <- confusion_matrix[1,1] / (confusion_matrix[1,1] + confusion_matrix[1,2])
  return(c(sensitivity, specificity, percision))
}

fit_glm <- function(variables, step_direction = "both"){
  model <- glm(success_status ~ ., data = variables, family = binomial(link='logit'))
  best_model <- MASS::stepAIC(model, direction = step_direction, trace = FALSE)
  return(list(model=best_model))
}

predict_glm <- function(model, data, is_youden = TRUE){
  pred_probabilities <- predict(model, newdata=data, type = "response")
  roc_curve <- roc(data$success_status, pred_probabilities)
  plot(roc_curve, col = "blue", main = "ROC Curve")
  index <- ifelse(is_youden, which.max(roc_curve$sensitivities + roc_curve$specificities - 1),
                  which.min(sqrt((1 - roc_curve$sensitivities)^2 + (1 - roc_curve$specificities)^2)) ) 
  cutoff <- roc_curve$thresholds[index]
  pred_classes <- ifelse(pred_probabilities <= cutoff, 1, 0)
  pred_classes <- factor(pred_classes, levels=c(1,0))
  
  return(list(cutoff=cutoff, predicitons=pred_classes, roc_curve=roc_curve))
}

set.seed(101)
train.idx <- sample(nrow(regression_variables), 4/5 * nrow(regression_variables)) 
data.train <- regression_variables[train.idx, ]
data.test <- regression_variables[-train.idx, ]

youden_model <- fit_glm(data.train)
youden_predictions <- predict_glm(youden_model$model, data.test, is_youden = TRUE)
youden_confusion_matrix <- table(Predicted = youden_predictions$predicitons, Actual = data.test$success_status)
youden_results <- diagnose_confusion_matrix(youden_confusion_matrix)
youden_confusion_matrix
youden_results

euclidean_model <- fit_glm(data.train)
euclidean_predictions <- predict_glm(euclidean_model$model, data.test, is_youden = FALSE)
euclidean_confusion_matrix <- table(Predicted = euclidean_predictions$predicitons, Actual = data.test$success_status)
euclidean_results <- diagnose_confusion_matrix(euclidean_confusion_matrix)
euclidean_confusion_matrix
euclidean_results

comparison_table <- rbind(youden_results, euclidean_results)
colnames(comparison_table) <- c("Sensitivity", "Specificity", "Percision")
rownames(comparison_table) <- c("Youden", "Euclidian")
comparison_table

# what about the vif? does multicolinearity affect the model?

best_model <- youden_model$model
sqrt(car::vif(best_model)) > 2

# there is variables that have higher vif, let's remove them and see if the model improves
# remove the variables with high vif
sqrt(car::vif(best_model))
high_vif <- which(sqrt(car::vif(best_model)) > 2)
high_vif

cleaned.data.train <- data.train %>% dplyr::select(-c(has_social_media))
cleaned.data.test <- data.test %>% dplyr::select(-c(has_social_media))

cleaned_youden_model <- fit_glm(cleaned.data.train)
cleaned_youden_predictions <- predict_glm(cleaned_youden_model$model, cleaned.data.test, is_youden = FALSE)
cleaned_youden_confusion_matrix <- table(Predicted = cleaned_youden_predictions$predicitons, Actual = cleaned.data.test$success_status)
cleaned_youden_results <- diagnose_confusion_matrix(cleaned_youden_confusion_matrix)
cleaned_youden_confusion_matrix
cleaned_youden_results

cleaned_euclidean_model <- fit_glm(cleaned.data.train)
cleaned_euclidean_predictions <- predict_glm(cleaned_euclidean_model$model, cleaned.data.test, is_youden = FALSE)
cleaned_euclidean_confusion_matrix <- table(Predicted = cleaned_euclidean_predictions$predicitons, Actual = cleaned.data.test$success_status)
cleaned_euclidean_results <- diagnose_confusion_matrix(cleaned_euclidean_confusion_matrix)
cleaned_euclidean_confusion_matrix
cleaned_euclidian_results


comparison_table <- rbind(youden_results, cleaned_youden_results, cleaned_euclidian_results)
colnames(comparison_table) <- c("Sensitivity", "Specificity", "Percision")
rownames(comparison_table) <- c("Youden", "Cleaned Youden", "Cleaned Euclidian")
comparison_table
# the specificity and percision improved for the cleaned model, but not the sensitity 

# compare auc
auc(euclidean_predictions$roc_curve)
auc(cleaned_euclidean_predictions$roc_curve)
# the auc is the same

# what if we change the step direction?
backward_model <- fit_glm(cleaned.data.train, step_direction = "backward")
backward_predictions <- predict_glm(backward_model$model, cleaned.data.test, is_youden = FALSE)
backward_confusion_matrix <- table(Predicted = backward_predictions$predicitons, Actual = cleaned.data.test$success_status)
backward_results <- diagnose_confusion_matrix(backward_confusion_matrix)
backward_confusion_matrix

forward_model <- fit_glm(cleaned.data.train, step_direction = "forward")
forward_predictions <- predict_glm(forward_model$model, cleaned.data.test, is_youden = FALSE)
forward_confusion_matrix <- table(Predicted = forward_predictions$predicitons, Actual = cleaned.data.test$success_status)
forward_results <- diagnose_confusion_matrix(forward_confusion_matrix)
forward_confusion_matrix

table <- rbind(youden_results, forward_results, backward_results)
colnames(table) <- c("Sensitivity", "Specificity", "Percision")
rownames(table) <- c("Backward", "Forward", "Both")
table
# the backward selection gives better results


# use AUC to compare models
library(dplyr)
library(MASS)
library(pROC)
summary(dataset)

regression_variables <- dataset %>% dplyr::select(success_status, is_donation, is_all_or_nothing,project_duration_days,
                                           promo_video_length,has_faq,has_website, has_social_media, have_followers, 
                                           log_social_media_followers, description_word_count,is_fongogo, in_big_city, has_female_owner, log_funding_target)
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

fit_and_diagnose <- function(variables, is_youden = TRUE, step_direction = "both"){
  model <- glm(success_status ~ ., data = variables, family = binomial(link='logit'))
  best_model <- MASS::stepAIC(model, direction = step_direction, trace = FALSE)
  pred_probs <- predict(best_model, type = "response")
  roc_curve <- roc(variables$success_status, pred_probs)
  plot(roc_curve, col = "blue", main = "ROC Curve")
  index <- ifelse(is_youden, which.max(roc_curve$sensitivities + roc_curve$specificities - 1),
                  which.min(sqrt((1 - roc_curve$sensitivities)^2 + (1 - roc_curve$specificities)^2)) ) 
  cutoff <- roc_curve$thresholds[index]
  pred_classes <- ifelse(pred_probs <= cutoff, 1, 0)
  pred_classes <- factor(pred_classes, levels=c(1,0))
  confusion_matrix <- table(Predicted = pred_classes, Actual = variables$success_status)
  sensitivity <- confusion_matrix[1,1] / (confusion_matrix[1,1] + confusion_matrix[2,1])
  specificity <- confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[1,2])
  percision <- confusion_matrix[1,1] / (confusion_matrix[1,1] + confusion_matrix[1,2])
  results <- c(sensitivity, specificity, percision)
  
  return(list(results = results, model = best_model, roc_curve = roc_curve))
}

youden_model <- fit_and_diagnose(regression_variables, is_youden = TRUE)
euclidian_model <- fit_and_diagnose(regression_variables, is_youden = FALSE)

comparison_table <- rbind(youden_model$results, euclidian_model$results)
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

cleaned_variables <- regression_variables %>% dplyr::select(-c(has_social_media))
cleaned_model_youden <- fit_and_diagnose(cleaned_variables, is_youden = TRUE)
cleaned_model_euclidian <- fit_and_diagnose(cleaned_variables, is_youden = FALSE)


table <- rbind(youden_model$results, cleaned_model_youden$results, cleaned_model_euclidian$results)
colnames(table) <- c("Sensitivity", "Specificity", "Percision")
rownames(table) <- c("Youden", "Cleaned Youden", "Cleaned Euclidian")
table
# the sensitivity improved for the cleaned model, but not the specificity and percision

# compare auc
auc(youden_model$roc_curve)
auc(cleaned_model_youden$roc_curve)
# the auc is a bit lower for the cleaned model

plot(youden_model$roc_curve, col = "blue", main = "ROC Curve", lwd = 4)
lines(cleaned_model_youden$roc_curve, col = "red")
legend("bottomright", legend = c("Full Model", "Cleaned Model"), col = c("blue", "red"), lwd = 4)

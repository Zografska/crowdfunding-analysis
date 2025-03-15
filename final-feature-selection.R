# use AUC to compare models
library(dplyr)
library(MASS)
summary(dataset)

regression_variables <- dataset %>% dplyr::select(success_status, is_donation, is_all_or_nothing,project_duration_days,
                                           promo_video_length,has_faq,has_website, has_social_media, have_followers, 
                                           log_social_media_followers, description_word_count,is_fongogo, in_big_city, has_female_owner, log_funding_target)
dim(regression_variables)
mod<-glm(success_status ~., data = regression_variables, family=binomial(link='logit'))
summary(mod)

library(MASS)
library(pROC)

# fit logistic regression model with all the variables
full_model <- glm(success_status ~ ., data = regression_variables, family = binomial(link='logit'))

# performs stepwise model selection by AIC.
best_model <- MASS::stepAIC(full_model, direction = "both", trace = FALSE)

# forward selection: Starts with an empty model and adds the most significant variables one by one.
# backward elimination: Starts with the full model and removes the least significant variables step by step.
# both: This combines both approaches, adding or removing variables at each step

# Summary of the best model
summary(best_model)
best_model$anova

# predict probabilities using the best model
pred_probs <- predict(best_model, type = "response")
# Compute ROC curve
roc_curve <- roc(regression_variables$success_status, pred_probs)

# plot ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve")
# ROC Curve helps visualize model performance to determiine best treshold

# find the optimal threshold using the roc_curve$thresholds Index
youden_index <- which.max(roc_curve$sensitivities + roc_curve$specificities - 1)

# determine the cutoff value by euclidian index
euclidian_index <- which.min(sqrt((1 - roc_curve$sensitivities)^2 + (1 - roc_curve$specificities)^2))
euclidian_cutoff <- roc_curve$thresholds[euclidian_index]

best_cutoff <- roc_curve$thresholds[youden_index]

# Print the best cutoff value
print(paste("Optimal Cutoff:", best_cutoff))

# classify based on the best cutoff
pred_classes <- ifelse(pred_probs <= best_cutoff, 1, 0)

pred_classes_eucludian <- ifelse(pred_probs <= euclidian_cutoff, 1, 0)

# reorder for the true pos and neg to be in the main diagonal
pred_classes <- factor(pred_classes, levels=c(1,0))
pred_classes_eucludian <- factor(pred_classes_eucludian, levels=c(1,0))


# Confusion Matrix
confusion_matrix <- table(Predicted = pred_classes, Actual = regression_variables$success_status)
# false neg - those that succeed but we predict they fail
# false pos - those that fail but we predict they succeed

# sensitivity = true pos / (true pos + false neg)
# what perc. of firms that will succeed were correctly predicted to succeed
sensitivity <- confusion_matrix[1,1] / (confusion_matrix[1,1] + confusion_matrix[2,1])
# 0.7393617 - 73.9% of the firms that will succeed were correctly predicted to succeed

# specificity - hom many of the firms that will fail were correctly predicted to fail
specificity <- confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[1,2])
# 0.6570743 - 65.7% of the firms that will fail were correctly predicted to fail

# accuracy

# precision - proportion of the positive results that were correctly classified
# tp/tp+fp
percision <- confusion_matrix[1,1] / (confusion_matrix[1,1] + confusion_matrix[1,2])
percision
# 0.3932107 - 39.3% of the projects that were predicted to succeed actually succeeded
# there are lot's of projects that didn't succeed compared to those that did
true_positives_proportion <- summary(regression_variables$success_status)[[1]]/ nrow(regression_variables) 
true_positives_proportion
# percision is not effected by the imbalance of true negatives
# percision is a good measure of the model's ability to predict the positive class

youden_results <- c(sensitivity, specificity, percision)

## for euclidian
confusion_matrix_euclidian <- table(Predicted = pred_classes, Actual = regression_variables$success_status)
sensitivity_euclidian <- confusion_matrix_euclidian[1,1] / (confusion_matrix_euclidian[1,1] + confusion_matrix_euclidian[2,1])
specificity_euclidian <- confusion_matrix_euclidian[2,2] / (confusion_matrix_euclidian[2,2] + confusion_matrix_euclidian[1,2])
percision_euclidian <- confusion_matrix_euclidian[1,1] / (confusion_matrix_euclidian[1,1] + confusion_matrix_euclidian[1,2])

euclidian_results <- c(sensitivity_euclidian, specificity_euclidian, percision_euclidian)

# compare the two models
table <- rbind(youden_results, euclidian_results)
colnames(table) <- c("Sensitivity", "Specificity", "Percision")
rownames(table) <- c("Youden", "Euclidian")
table

# what about the vif? does multicolinearity affect the model?

sqrt(car::vif(best_model)) > 2
# there is variables that have higher vif, let's remove them and see if the model improves

# remove the variables with high vif
sqrt(car::vif(best_model))
high_vif <- which(sqrt(car::vif(best_model)) > 2)
high_vif
cleaned_variables <- regression_variables %>% select(-has_social_media)
cleaned_model <- glm(success_status ~ ., data = cleaned_variables, family = binomial(link='logit'))
cleaned_best_model <- MASS::stepAIC(cleaned_model, direction = "both", trace = FALSE)
cleaned_pred_probs <- predict(cleaned_best_model, type = "response")
cleaned_roc_curve <- roc(cleaned_variables$success_status, cleaned_pred_probs)
plot(cleaned_roc_curve, col = "blue", main = "ROC Curve")
cleaned_youden_index <- which.max(cleaned_roc_curve$sensitivities + cleaned_roc_curve$specificities - 1)
cleaned_best_cutoff <- cleaned_roc_curve$thresholds[cleaned_youden_index]
cleaned_pred_classes <- ifelse(cleaned_pred_probs <= cleaned_best_cutoff, 1, 0)
cleaned_pred_classes <- factor(cleaned_pred_classes, levels=c(1,0))
cleaned_confusion_matrix <- table(Predicted = cleaned_pred_classes, Actual = cleaned_variables$success_status)
cleaned_sensitivity <- cleaned_confusion_matrix[1,1] / (cleaned_confusion_matrix[1,1] + cleaned_confusion_matrix[2,1])
cleaned_specificity <- cleaned_confusion_matrix[2,2] / (cleaned_confusion_matrix[2,2] + cleaned_confusion_matrix[1,2])
cleaned_percision <- cleaned_confusion_matrix[1,1] / (cleaned_confusion_matrix[1,1] + cleaned_confusion_matrix[1,2])
cleaned_results <- c(cleaned_sensitivity, cleaned_specificity, cleaned_percision)
cleaned_results

# compare the two models
table <- rbind(youden_results, cleaned_results)
colnames(table) <- c("Sensitivity", "Specificity", "Percision")
rownames(table) <- c("Youden", "Cleaned")
table
# the sensitivity improved for the cleaned model, but not the specificity and percision

# compare auc
auc(roc_curve)
auc(cleaned_roc_curve)
# the auc is a bit lower for the cleaned model

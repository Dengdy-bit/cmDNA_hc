library(randomForest)
library(pROC)
library(caret)
library(ggplot2)
library(dplyr)
library(parallel)
library(doParallel)

All_inter_sample<-c(sample_train,sample_inter_test)
sample_names<-All_inter_sample
set.seed(123)  # Set random seeds to ensure repeatable results
shuffled_indices <- sample(1:length(sample_names))

# Divide it into two parts in 1:1 
split_point <- floor(length(sample_names) / 2)
group1_indices <- shuffled_indices[1:split_point]
group2_indices <- shuffled_indices[(split_point + 1):length(sample_names)]

# Extract sample names for both groups
group1_samples <- sample_names[group1_indices]
group2_samples <- sample_names[group2_indices]

# print results
print(paste("Group 1 :", length(group1_samples)))
print(paste("Group 2 :", length(group2_samples)))



train_sample<-group1_samples
test_sample<-group2_samples
info_train<-df[train_sample,]
info_train$Group<-group_type[train_sample,]
info_train$Group <- as.factor(info_train$Group)

info_inter_test<-df[test_sample,]
info_inter_test$Group<-group_type[test_sample,]
info_inter_test$Group <- as.factor(info_inter_test$Group)

sample_exter_test<-exter_test_train$Sample
select_sample_ex<-intersect(rownames(group_type), sample_exter_test)
info_exter_test<-df[select_sample_ex,]
info_exter_test$Group<-group_type[select_sample_ex,]
info_exter_test$Group <- as.factor(info_exter_test$Group)

X_train <- info_train[, -ncol(info_train)]
Y_train <- info_train$Group
X_test_in <- info_inter_test[, -ncol(info_inter_test)]
Y_test_in <- info_inter_test$Group
X_test_ex <- info_exter_test[, -ncol(info_exter_test)]
Y_test_ex <- info_exter_test$Group


model_info<-"rf"         ###you can be replaced with other models
roc_list <- list()
model_list<-list()

cl<-makePSOCKcluster(20)

registerDoParallel(cl)

for (i in 1:100) {
  set.seed(123 + i)
  model_random <- train(
    x = X_train, y = Y_train,
    method = model_info,
    metric = "ROC",
    trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary,savePredictions="final"),
    tuneLength = 15,    # Automatically adjusts the hyperparameters
    #tuneGrid = xgb_grid
  )

  rf_probs <- predict(model_random, X_test_in, type = "prob")[,1]
  roc_L <- roc(Y_test_in, rf_probs)
  #Save ROC results
  model_list[[i]]<-model_random
  roc_list[[i]] <-roc_L
}


table(Y_train)
best_index <- which.max(sapply(roc_list, function(x) x$auc))
roc_rf_in <- roc_list[[best_index]]
best_model_rf_random<-model_list[[best_index]]
saveRDS(best_model_rf_random,"rf_model.rds")
rf_probs <- predict(best_model_rf_random, X_test_ex, type = "prob")[,1]
roc_rf_ex <- roc(Y_test_ex, rf_probs)

predictions<-best_model_rf_random$pred
roc_rf_train<-roc(predictions$obs,predictions$Malignant)

model_info<-"glmnet"         ###you can be replaced with other models
roc_list <- list()
model_list<-list()

for (i in 1:100) {
  set.seed(123 + i)
  model_random <- train(
    x = X_train, y = Y_train,
    method = model_info,
    metric = "ROC",
    trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary,savePredictions="final"),
    tuneLength = 5,    # Automatically adjusts the hyperparameters
    #tuneGrid = xgb_grid
  )

  rf_probs <- predict(model_random, X_test_in, type = "prob")[,1]
  roc_L <- roc(Y_test_in, rf_probs)
  #Save ROC results
  model_list[[i]]<-model_random
  roc_list[[i]] <-roc_L
}
best_index <- which.max(sapply(roc_list, function(x) x$auc))
roc_glm_in <- roc_list[[best_index]]
best_model_glm_random<-model_list[[best_index]]
saveRDS(best_model_glm_random,"glm_model.rds")
rf_probs <- predict(best_model_glm_random, X_test_ex, type = "prob")[,1]
roc_glm_ex <- roc(Y_test_ex, rf_probs)

predictions<-best_model_glm_random$pred
roc_glm_train<-roc(predictions$obs,predictions$Malignant)


model_info<-"nnet"         ###you can be replaced with other models
roc_list <- list()
model_list<-list()

for (i in 1:100) {
  set.seed(123 + i)
  rf_model_random <- train(
    x = X_train, y = Y_train,
    method = model_info,
    metric = "ROC",
    trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary,savePredictions="final"),
    tuneLength = 10,    # Automatically adjusts the hyperparameters
    #tuneGrid = xgb_grid
  )

  rf_probs <- predict(rf_model_random, X_test_in, type = "prob")[,1]
  roc_L <- roc(Y_test_in, rf_probs)
  #Save ROC results
  model_list[[i]]<-rf_model_random
  roc_list[[i]] <-roc_L
}
best_index <- which.max(sapply(roc_list, function(x) x$auc))
roc_mlp_in <- roc_list[[best_index]]
best_model_mlp_random<-model_list[[best_index]]
saveRDS(best_model_mlp_random,"mlp_model.rds")
rf_probs <- predict(best_model_mlp_random, X_test_ex, type = "prob")[,1]
roc_mlp_ex <- roc(Y_test_ex, rf_probs)

predictions<-best_model_mlp_random$pred
roc_mlp_train<-roc(predictions$obs,predictions$Malignant)

xgb_grid <- expand.grid(
 nrounds = c(100,200),
 max_depth = c(3, 5),
 eta = c(0.01,0.1, 0.3),
 gamma = c(0, 0.1),
 colsample_bytree = c(0.8),
 min_child_weight = c(1),
 subsample = c(0.6,0.8)
)
model_info<-"xgbTree"         ###you can be replaced with other models
roc_list <- list()
model_list<-list()

for (i in 1:100) {
  set.seed(123 + i)
  rf_model_random <- train(
    x = X_train, y = Y_train,
    method = model_info,
    metric = "ROC",
    trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary,savePredictions="final"),
    tuneLength = 10,    # Automatically adjusts the hyperparameters
    #tuneGrid = xgb_grid
  )

  rf_probs <- predict(rf_model_random, X_test_in, type = "prob")[,1]
  roc_L <- roc(Y_test_in, rf_probs)
  #Save ROC results
  model_list[[i]]<-rf_model_random
  roc_list[[i]] <-roc_L
}
best_index <- which.max(sapply(roc_list, function(x) x$auc))
roc_xgb_in <- roc_list[[best_index]]
best_model_xgb_random<-model_list[[best_index]]
saveRDS(best_model_xgb_random,"xgb_model.rds")
rf_probs <- predict(best_model_xgb_random, X_test_ex, type = "prob")[,1]
roc_xgb_ex <- roc(Y_test_ex, rf_probs)

predictions<-best_model_xgb_random$pred
roc_xgb_train<-roc(predictions$obs,predictions$Malignant)

pdf("model_all_random_train.pdf")
plot(roc_rf_train, col = "blue", main = "ROC Curve Comparison", print.auc = F, lwd = 2)
plot(roc_glm_train, col = "red", add = TRUE, print.auc = F, lwd = 2)
plot(roc_mlp_train, col = "green", add = TRUE, print.auc = F, lwd = 2)
plot(roc_xgb_train,col = "purple", add = TRUE, print.auc = F, lwd = 2)
ci_rf_train <- ci.auc(roc_rf_train)
ci_glm_train <- ci.auc(roc_glm_train)
ci_mlp_train <- ci.auc(roc_mlp_train)
ci_xgb_train <- ci.auc(roc_xgb_train)


auc_text_rf <- paste0("Training Cohort AUC: ", round(auc(roc_rf_train), 3), 
                      " (95% CI: ", round(ci_rf_train[1], 3), "-", round(ci_rf_train[3], 3), ")")


auc_text_glm <- paste0("Inter Cohort AUC: ", round(auc(roc_glm_train), 3), 
                      " (95% CI: ", round(ci_glm_train[1], 3), "-", round(ci_glm_train[3], 3), ")")


auc_text_mlp <- paste0("Exter Cohort AUC: ", round(auc(roc_mlp_train), 3), 
                      " (95% CI: ", round(ci_mlp_train[1], 3), "-", round(ci_mlp_train[3], 3), ")")

auc_text_xgb <- paste0("Exter Cohort AUC: ", round(auc(roc_xgb_train), 3), 
                      " (95% CI: ", round(ci_xgb_train[1], 3), "-", round(ci_xgb_train[3], 3), ")")

legend("bottomright", 
       legend = c(auc_text_rf, auc_text_glm , auc_text_mlp ,auc_text_xgb),
       col = c("blue", "red", "green","purple"), lwd = 2, cex = 0.8)
dev.off()

pdf("model_all_random_in.pdf")
plot(roc_rf_in, col = "blue", main = "ROC Curve Comparison", print.auc = F, lwd = 2)
plot(roc_glm_in, col = "red", add = TRUE, print.auc = F, lwd = 2)
plot(roc_mlp_in, col = "green", add = TRUE, print.auc = F, lwd = 2)
plot(roc_xgb_in,col = "purple", add = TRUE, print.auc = F, lwd = 2)
ci_rf_in <- ci.auc(roc_rf_in)
ci_glm_in <- ci.auc(roc_glm_in)
ci_mlp_in <- ci.auc(roc_mlp_in)
ci_xgb_in <- ci.auc(roc_xgb_in)

auc_int_rf <- paste0("Random Forest AUC: ", round(auc(roc_rf_in), 3), 
                      " (95% CI: ", round(ci_rf_in[1], 3), "-", round(ci_rf_in[3], 3), ")")


auc_int_glm <- paste0("GLM AUC: ", round(auc(roc_glm_in), 3), 
                      " (95% CI: ", round(ci_glm_in[1], 3), "-", round(ci_glm_in[3], 3), ")")


auc_int_mlp <- paste0("MLP AUC: ", round(auc(roc_mlp_in), 3), 
                      " (95% CI: ", round(ci_mlp_in[1], 3), "-", round(ci_mlp_in[3], 3), ")")

auc_int_xgb <- paste0("XGBoost AUC: ", round(auc(roc_xgb_in), 3), 
                      " (95% CI: ", round(ci_xgb_in[1], 3), "-", round(ci_xgb_in[3], 3), ")")

legend("bottomright", 
       legend = c(auc_int_rf, auc_int_glm , auc_int_mlp ,auc_int_xgb),
       col = c("blue", "red", "green","purple"), lwd = 2, cex = 0.8)
dev.off()

pdf("model_all_random_ex.pdf")
plot(roc_rf_ex, col = "blue", maex = "ROC Curve Comparison", prext.auc = F, lwd = 2)
plot(roc_glm_ex, col = "red", add = TRUE, prext.auc = F, lwd = 2)
plot(roc_mlp_ex, col = "green", add = TRUE, prext.auc = F, lwd = 2)
plot(roc_xgb_ex,col = "purple", add = TRUE, prext.auc = F, lwd = 2)
ci_rf_ex <- ci.auc(roc_rf_ex)
ci_glm_ex <- ci.auc(roc_glm_ex)
ci_mlp_ex <- ci.auc(roc_mlp_ex)
ci_xgb_ex <- ci.auc(roc_xgb_ex)

auc_ext_rf <- paste0("Random Forest AUC: ", round(auc(roc_rf_ex), 3), 
                      " (95% CI: ", round(ci_rf_ex[1], 3), "-", round(ci_rf_ex[3], 3), ")")


auc_ext_glm <- paste0("GLM AUC: ", round(auc(roc_glm_ex), 3), 
                      " (95% CI: ", round(ci_glm_ex[1], 3), "-", round(ci_glm_ex[3], 3), ")")

auc_ext_mlp <- paste0("MLP AUC: ", round(auc(roc_mlp_ex), 3), 
                      " (95% CI: ", round(ci_mlp_ex[1], 3), "-", round(ci_mlp_ex[3], 3), ")")

auc_ext_xgb <- paste0("XGBoost AUC: ", round(auc(roc_xgb_ex), 3), 
                      " (95% CI: ", round(ci_xgb_ex[1], 3), "-", round(ci_xgb_ex[3], 3), ")")

legend("bottomright", 
       legend = c(auc_ext_rf, auc_ext_glm , auc_ext_mlp ,auc_ext_xgb),
       col = c("blue", "red", "green","purple"), lwd = 2, cex = 0.8)
dev.off()


predictions<-best_model_rf_random$pred
cm_rf_train <- confusionMatrix(predictions$pred, predictions$obs,positive = "Malignant")

y_pred_in <- predict(best_model_rf_random, X_test_in)
cm_rf_in <- confusionMatrix(y_pred_in, Y_test_in,positive = "Malignant")

y_pred_ex <- predict(best_model_rf_random, X_test_ex)
cm_rf_ex <- confusionMatrix(y_pred_ex, Y_test_ex,positive = "Malignant")

predictions<-best_model_glm_random$pred
cm_glm_train <- confusionMatrix(predictions$pred, predictions$obs,positive = "Malignant")

y_pred_in <- predict(best_model_glm_random, X_test_in)
cm_glm_in <- confusionMatrix(y_pred_in, Y_test_in,positive = "Malignant")

y_pred_ex <- predict(best_model_glm_random, X_test_ex)
cm_glm_ex <- confusionMatrix(y_pred_ex, Y_test_ex,positive = "Malignant")

predictions<-best_model_mlp_random$pred
cm_mlp_train <- confusionMatrix(predictions$pred, predictions$obs,positive = "Malignant")

y_pred_in <- predict(best_model_mlp_random, X_test_in)
cm_mlp_in <- confusionMatrix(y_pred_in, Y_test_in,positive = "Malignant")

y_pred_ex <- predict(best_model_mlp_random, X_test_ex)
cm_mlp_ex <- confusionMatrix(y_pred_ex, Y_test_ex,positive = "Malignant")

predictions<-best_model_xgb_random$pred
cm_xgb_train <- confusionMatrix(predictions$pred, predictions$obs,positive = "Malignant")

y_pred_in <- predict(best_model_xgb_random, X_test_in)
cm_xgb_in <- confusionMatrix(y_pred_in, Y_test_in,positive = "Malignant")

y_pred_ex <- predict(best_model_xgb_random, X_test_ex)
cm_xgb_ex <- confusionMatrix(y_pred_ex, Y_test_ex,positive = "Malignant")

get_ci <- function(value, n, conf.level = 0.95) {
  ci <- binom.test(round(value * n), n, conf.level = conf.level)$conf.int
  return(sprintf("%.3f - %.3f", ci[1], ci[2]))
}

# # Create formatting data frame extraction
create_results_df <- function() {
  data.frame(
    Samples = character(),
    Model = character(),
    AUC_95_CI = character(),
    Sensitivity_95_CI = character(),
    Specificity_95_CI = character(),
    Pos_Pred_Value = numeric(),
    Neg_Pred_Value = numeric(),
    Accuracy=character(),
    stringsAsFactors = FALSE
  )
}

results_df <- create_results_df()
add_row_to_df <- function(df, sample_type, model_name, 
                         auc_val, auc_ci_low, auc_ci_high,
                         cm_object, n_samples) {
  ####Extract the confusion matrix index
  sensitivity <- cm_object$byClass["Sensitivity"]
  specificity <- cm_object$byClass["Specificity"]
  ppv <- cm_object$byClass["Pos Pred Value"]
  npv <- cm_object$byClass["Neg Pred Value"]
  Acu <- cm_object$overall["Accuracy"]
  ACU_low<-cm_object$overall["AccuracyLower"]
  ACU_hign<-cm_object$overall["AccuracyUpper"]
  # Calculate the confidence interval
  sens_ci <- get_ci(sensitivity, n_samples)
  spec_ci <- get_ci(specificity, n_samples)
  
  new_row <- data.frame(
    Samples = sample_type,
    Model = model_name,
    AUC_95_CI = sprintf("%.3f(%.3f - %.3f)", auc_val, auc_ci_low, auc_ci_high),
    Sensitivity_95_CI = sprintf("%.3f(%s)", sensitivity, sens_ci),
    Specificity_95_CI = sprintf("%.3f(%s)", specificity, spec_ci),
    Pos_Pred_Value = round(ppv, 3),
    Neg_Pred_Value = round(npv, 3),
    Accuracy=sprintf("%.3f(%.3f - %.3f)", Acu,  ACU_low, ACU_hign),
    stringsAsFactors = FALSE
  )
  
  rbind(df, new_row)
}

n_train <- sum(cm_rf_train$table)  
 
n_in <- sum(cm_rf_in$table)        

n_ex <- sum(cm_rf_ex$table)        

results_df <- add_row_to_df(results_df, "Training Cohort", "Random Forest",
                          auc(roc_rf_train)[1], ci_rf_train[1], ci_rf_train[3],
                          cm_rf_train, n_train)

results_df <- add_row_to_df(results_df, "Internal Cohort", "Random Forest",
                          auc(roc_rf_in)[1], ci_rf_in[1], ci_rf_in[3],
                          cm_rf_in, n_in)

results_df <- add_row_to_df(results_df, "External Cohort", "Random Forest",
                          auc(roc_rf_ex)[1],  ci_rf_ex[1], ci_rf_ex[3],
                          cm_rf_ex, n_ex)
results_df <- add_row_to_df(results_df, "Training Cohort", "GLM",
                          auc(roc_glm_train)[1], ci_glm_train[1], ci_glm_train[3],
                          cm_glm_train, n_train)

results_df <- add_row_to_df(results_df, "Internal Cohort", "GLM",
                          auc(roc_glm_in)[1], ci_glm_in[1], ci_glm_in[3],
                          cm_glm_in, n_in)

results_df <- add_row_to_df(results_df, "External Cohort", "GLM",
                          auc(roc_glm_ex)[1],  ci_glm_ex[1], ci_glm_ex[3],
                          cm_glm_ex, n_ex)
results_df <- add_row_to_df(results_df, "Training Cohort", "MLP",
                          auc(roc_mlp_train)[1], ci_mlp_train[1], ci_mlp_train[3],
                          cm_mlp_train, n_train)

results_df <- add_row_to_df(results_df, "Internal Cohort", "MLP",
                          auc(roc_mlp_in)[1], ci_mlp_in[1], ci_mlp_in[3],
                          cm_mlp_in, n_in)

results_df <- add_row_to_df(results_df, "External Cohort", "MLP",
                          auc(roc_mlp_ex)[1],  ci_mlp_ex[1], ci_mlp_ex[3],
                          cm_mlp_ex, n_ex)
results_df <- add_row_to_df(results_df, "Training Cohort", "Xgbtree",
                          auc(roc_xgb_train)[1], ci_xgb_train[1], ci_xgb_train[3],
                          cm_xgb_train, n_train)

results_df <- add_row_to_df(results_df, "Internal Cohort", "Xgbtree",
                          auc(roc_xgb_in)[1], ci_xgb_in[1], ci_xgb_in[3],
                          cm_xgb_in, n_in)

results_df <- add_row_to_df(results_df, "External Cohort", "Xgbtree",
                          auc(roc_xgb_ex)[1],  ci_xgb_ex[1], ci_xgb_ex[3],
                          cm_xgb_ex, n_ex)
formatted_results <- results_df %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  arrange(Samples, factor(Model, levels = c("Random Forest", "GLM", "MLP", "XGBoost")))

print(formatted_results)


write.csv(formatted_results, "AUC_model_performance_summary.csv", row.names = FALSE)


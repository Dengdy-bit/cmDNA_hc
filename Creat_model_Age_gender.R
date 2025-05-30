library(randomForest)
library(pROC)
library(caret)
library(ggplot2)
library(dplyr)
library(parallel)
library(doParallel)


data_train <-read.table("../410.Cohort.txt",sep = "\t",header = T,check.names = F)
info_train$Age<-data_train$Age
info_train$Gender<-data_train$Gender
info_inter_test$Age<-inter_test_train$Age
info_inter_test$Gender<-inter_test_train$Gender
info_exter_test$Age<-exter_test_train$Age
info_exter_test$Gender<-exter_test_train$Gender


X_train <- info_train[, names(info_train) != "Group"]
Y_train <- info_train$Group
X_test_in <- info_inter_test[, names(info_inter_test) != "Group"]
Y_test_in <- info_inter_test$Group
X_test_ex <- info_exter_test[, names(info_inter_test) != "Group"]
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
    tuneLength = 20,    # Automatically adjusts the hyperparameters
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
saveRDS(best_model_rf_random,"rf_model_Age_gender.rds")
rf_probs <- predict(best_model_rf_random, X_test_ex, type = "prob")[,1]
roc_rf_ex <- roc(Y_test_ex, rf_probs)

predictions<-best_model_rf_random$pred
roc_rf_train<-roc(predictions$obs,predictions$Malignant)


pdf("rf_model_all_Age_gender.pdf")
plot(roc_rf_train, col = "blue", main = "ROC Curve Comparison", print.auc = F, lwd = 2)
plot(roc_rf_in, col = "red", add = TRUE, print.auc = F, lwd = 2)
plot(roc_rf_ex, col = "green", add = TRUE, print.auc = F, lwd = 2)

ci_auc_train <- ci.auc(roc_rf_train)
ci_auc_in <- ci.auc(roc_rf_in)
ci_auc_ex <- ci.auc(roc_rf_ex)

auc_text_rf <- paste0("Training Cohort AUC: ", round(auc(roc_rf_train), 3), 
                      " (95% CI: ", round(ci_auc_train[1], 3), "-", round(ci_auc_train[3], 3), ")")


auc_text_in <- paste0("Internal Cohort AUC: ", round(auc(roc_rf_in), 3), 
                      " (95% CI: ", round(ci_auc_in[1], 3), "-", round(ci_auc_in[3], 3), ")")


auc_text_ex <- paste0("External Cohort AUC: ", round(auc(roc_rf_ex), 3), 
                      " (95% CI: ", round(ci_auc_ex[1], 3), "-", round(ci_auc_ex[3], 3), ")")
legend("bottomright", 
       legend = c(auc_text_rf, auc_text_in, auc_text_ex),
       col = c("blue", "red", "green"), lwd = 2, cex = 0.8)
dev.off()

predictions<-best_model_rf_random$pred
cm_rf_train <- confusionMatrix(predictions$pred, predictions$obs,positive = "Malignant")

y_pred_in <- predict(best_model_rf_random, X_test_in)
cm_rf_in <- confusionMatrix(y_pred_in, Y_test_in,positive = "Malignant")

y_pred_ex <- predict(best_model_rf_random, X_test_ex)
cm_rf_ex <- confusionMatrix(y_pred_ex, Y_test_ex,positive = "Malignant")



get_ci <- function(value, n, conf.level = 0.95) {
  ci <- binom.test(round(value * n), n, conf.level = conf.level)$conf.int
  return(sprintf("%.3f - %.3f", ci[1], ci[2]))
}

# 创建格式化数据框
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
  
  sensitivity <- cm_object$byClass["Sensitivity"]
  specificity <- cm_object$byClass["Specificity"]
  ppv <- cm_object$byClass["Pos Pred Value"]
  npv <- cm_object$byClass["Neg Pred Value"]
  Acu <- cm_object$overall["Accuracy"]
  ACU_low<-cm_object$overall["AccuracyLower"]
  ACU_hign<-cm_object$overall["AccuracyUpper"]

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
                          auc(roc_rf_train)[1], ci_auc_train[1], ci_auc_train[3],
                          cm_rf_train, n_train)

results_df <- add_row_to_df(results_df, "Internal Cohort", "Random Forest",
                          auc(roc_rf_in)[1],ci_auc_in[1], ci_auc_in[3],
                          cm_rf_in, n_in)

results_df <- add_row_to_df(results_df, "External Cohort", "Random Forest",
                          auc(roc_rf_ex)[1],  ci_auc_ex[1], ci_auc_ex[3],
                          cm_rf_ex, n_ex)
write.csv(results_df, "AUC_Age_Gender_performance_summary.csv", row.names = FALSE)



X_train <- info_train[, names(info_train) != "Group"]
Y_train <- info_train$Group
X_test_in <- info_inter_test[, names(info_inter_test) != "Group"]
Y_test_in <- info_inter_test$Group
X_test_ex <- info_exter_test[, names(info_inter_test) != "Group"]
Y_test_ex <- info_exter_test$Group


X_train$Age<-NULL
X_test_in$Age<-NULL
X_test_ex$Age<-NULL
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
    tuneLength = 20,    # Automatically adjusts the hyperparameters
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
saveRDS(best_model_rf_random,"rf_model_gender.rds")
rf_probs <- predict(best_model_rf_random, X_test_ex, type = "prob")[,1]
roc_rf_ex <- roc(Y_test_ex, rf_probs)

predictions<-best_model_rf_random$pred
roc_rf_train<-roc(predictions$obs,predictions$Malignant)


pdf("rf_model_all_gender.pdf")
plot(roc_rf_train, col = "blue", main = "ROC Curve Comparison", print.auc = F, lwd = 2)
plot(roc_rf_in, col = "red", add = TRUE, print.auc = F, lwd = 2)
plot(roc_rf_ex, col = "green", add = TRUE, print.auc = F, lwd = 2)

ci_auc_train <- ci.auc(roc_rf_train)
ci_auc_in <- ci.auc(roc_rf_in)
ci_auc_ex <- ci.auc(roc_rf_ex)

auc_text_rf <- paste0("Training Cohort AUC: ", round(auc(roc_rf_train), 3), 
                      " (95% CI: ", round(ci_auc_train[1], 3), "-", round(ci_auc_train[3], 3), ")")


auc_text_in <- paste0("Internal Cohort AUC: ", round(auc(roc_rf_in), 3), 
                      " (95% CI: ", round(ci_auc_in[1], 3), "-", round(ci_auc_in[3], 3), ")")


auc_text_ex <- paste0("External Cohort AUC: ", round(auc(roc_rf_ex), 3), 
                      " (95% CI: ", round(ci_auc_ex[1], 3), "-", round(ci_auc_ex[3], 3), ")")
legend("bottomright", 
       legend = c(auc_text_rf, auc_text_in, auc_text_ex),
       col = c("blue", "red", "green"), lwd = 2, cex = 0.8)
dev.off()

predictions<-best_model_rf_random$pred
cm_rf_train <- confusionMatrix(predictions$pred, predictions$obs,positive = "Malignant")

y_pred_in <- predict(best_model_rf_random, X_test_in)
cm_rf_in <- confusionMatrix(y_pred_in, Y_test_in,positive = "Malignant")

y_pred_ex <- predict(best_model_rf_random, X_test_ex)
cm_rf_ex <- confusionMatrix(y_pred_ex, Y_test_ex,positive = "Malignant")



get_ci <- function(value, n, conf.level = 0.95) {
  ci <- binom.test(round(value * n), n, conf.level = conf.level)$conf.int
  return(sprintf("%.3f - %.3f", ci[1], ci[2]))
}


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
  
 
  sensitivity <- cm_object$byClass["Sensitivity"]
  specificity <- cm_object$byClass["Specificity"]
  ppv <- cm_object$byClass["Pos Pred Value"]
  npv <- cm_object$byClass["Neg Pred Value"]
  Acu <- cm_object$overall["Accuracy"]
  ACU_low<-cm_object$overall["AccuracyLower"]
  ACU_hign<-cm_object$overall["AccuracyUpper"]

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
                          auc(roc_rf_train)[1], ci_auc_train[1], ci_auc_train[3],
                          cm_rf_train, n_train)

results_df <- add_row_to_df(results_df, "Internal Cohort", "Random Forest",
                          auc(roc_rf_in)[1],ci_auc_in[1], ci_auc_in[3],
                          cm_rf_in, n_in)

results_df <- add_row_to_df(results_df, "External Cohort", "Random Forest",
                          auc(roc_rf_ex)[1],  ci_auc_ex[1], ci_auc_ex[3],
                          cm_rf_ex, n_ex)
write.csv(results_df, "AUC_Gender_performance_summary.csv", row.names = FALSE)

#######Age Only########
X_train <- info_train[, names(info_train) != "Group"]
Y_train <- info_train$Group
X_test_in <- info_inter_test[, names(info_inter_test) != "Group"]
Y_test_in <- info_inter_test$Group
X_test_ex <- info_exter_test[, names(info_inter_test) != "Group"]
Y_test_ex <- info_exter_test$Group


X_train$Gender<-NULL
X_test_in$Gender<-NULL
X_test_ex$Gender<-NULL
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
    tuneLength = 20,    # Automatically adjusts the hyperparameters
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
saveRDS(best_model_rf_random,"rf_model_Age.rds")
rf_probs <- predict(best_model_rf_random, X_test_ex, type = "prob")[,1]
roc_rf_ex <- roc(Y_test_ex, rf_probs)

predictions<-best_model_rf_random$pred
roc_rf_train<-roc(predictions$obs,predictions$Malignant)


pdf("rf_model_all_Age.pdf")
plot(roc_rf_train, col = "blue", main = "ROC Curve Comparison", print.auc = F, lwd = 2)
plot(roc_rf_in, col = "red", add = TRUE, print.auc = F, lwd = 2)
plot(roc_rf_ex, col = "green", add = TRUE, print.auc = F, lwd = 2)

ci_auc_train <- ci.auc(roc_rf_train)
ci_auc_in <- ci.auc(roc_rf_in)
ci_auc_ex <- ci.auc(roc_rf_ex)

auc_text_rf <- paste0("Training Cohort AUC: ", round(auc(roc_rf_train), 3), 
                      " (95% CI: ", round(ci_auc_train[1], 3), "-", round(ci_auc_train[3], 3), ")")


auc_text_in <- paste0("Internal Cohort AUC: ", round(auc(roc_rf_in), 3), 
                      " (95% CI: ", round(ci_auc_in[1], 3), "-", round(ci_auc_in[3], 3), ")")


auc_text_ex <- paste0("External Cohort AUC: ", round(auc(roc_rf_ex), 3), 
                      " (95% CI: ", round(ci_auc_ex[1], 3), "-", round(ci_auc_ex[3], 3), ")")
legend("bottomright", 
       legend = c(auc_text_rf, auc_text_in, auc_text_ex),
       col = c("blue", "red", "green"), lwd = 2, cex = 0.8)
dev.off()

predictions<-best_model_rf_random$pred
cm_rf_train <- confusionMatrix(predictions$pred, predictions$obs,positive = "Malignant")

y_pred_in <- predict(best_model_rf_random, X_test_in)
cm_rf_in <- confusionMatrix(y_pred_in, Y_test_in,positive = "Malignant")

y_pred_ex <- predict(best_model_rf_random, X_test_ex)
cm_rf_ex <- confusionMatrix(y_pred_ex, Y_test_ex,positive = "Malignant")



get_ci <- function(value, n, conf.level = 0.95) {
  ci <- binom.test(round(value * n), n, conf.level = conf.level)$conf.int
  return(sprintf("%.3f - %.3f", ci[1], ci[2]))
}

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
  
  
  sensitivity <- cm_object$byClass["Sensitivity"]
  specificity <- cm_object$byClass["Specificity"]
  ppv <- cm_object$byClass["Pos Pred Value"]
  npv <- cm_object$byClass["Neg Pred Value"]
  Acu <- cm_object$overall["Accuracy"]
  ACU_low<-cm_object$overall["AccuracyLower"]
  ACU_hign<-cm_object$overall["AccuracyUpper"]
 
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
                          auc(roc_rf_train)[1], ci_auc_train[1], ci_auc_train[3],
                          cm_rf_train, n_train)

results_df <- add_row_to_df(results_df, "Internal Cohort", "Random Forest",
                          auc(roc_rf_in)[1],ci_auc_in[1], ci_auc_in[3],
                          cm_rf_in, n_in)

results_df <- add_row_to_df(results_df, "External Cohort", "Random Forest",
                          auc(roc_rf_ex)[1],  ci_auc_ex[1], ci_auc_ex[3],
                          cm_rf_ex, n_ex)
write.csv(results_df, "AUC_Age_performance_summary.csv", row.names = FALSE)

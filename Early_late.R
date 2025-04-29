library(randomForest)
library(pROC)
library(caret)
library(ggplot2)
library(dplyr)
library(parallel)
library(doParallel)

test_all<-c(select_sample_in,select_sample_ex)
test_group<-group_type[test_all,,drop=FALSE]
table(group_type[test_all,])
Early_test_samples<-rownames(test_group[test_group$Group == "Early",,drop=FALSE])
Late_test_samples<-rownames(test_group[test_group$Group == "Late",,drop=FALSE])
Benign_test_samples<-rownames(test_group[test_group$Group == "Benign",,drop=FALSE])

set.seed(123)

# Randomly select from 329 benign samples
all_benign <- Benign_test_samples  
# Step 1: First, randomly select 184 samples as Late_II_samples
Late_II_samples <- sample(all_benign, 184)

# tep 2: Then, select 71 samples from the remaining samples as Early_I_samples
remaining_samples <- setdiff(all_benign, Late_II_samples)
Early_I_samples <- sample(remaining_samples, 71)

Validation_I_sample<-c(Early_test_samples,Early_I_samples)

Validation_II_sample<-c(Late_test_samples,Late_II_samples)

data <- read.table("Group_lefse_input_form.xls", header = TRUE, sep = "\t", check.names = FALSE)
import_info <- read.table("hmp_aerobiosis_small.res", header = F, sep = "\t", check.names = FALSE)
import_info[, 5] <- as.numeric(import_info[, 5])
filtered_data <- import_info[import_info[, 5] < 0.05 & !is.na(import_info[, 5]), ]
first_column <- filtered_data[, 1]
selected_rows <- first_column[grepl("s__", first_column)]
importance<-data[selected_rows,]
importance$Taxonomy<-NULL

otu_table<-importance
otu_table_t<-t(otu_table)
otu_table_t <- as.data.frame(otu_table_t)
info_I_test<-otu_table_t[Validation_I_sample,]
info_II_test<-otu_table_t[Validation_II_sample,]
info_I_test$Group<-group_type[Validation_I_sample,]
info_II_test$Group<-group_type[Validation_II_sample,]
info_I_test$Group<-as.factor(info_I_test$Group)
info_II_test$Group<-as.factor(info_II_test$Group)
X_test_I <- info_I_test[, -ncol(info_I_test)]
Y_test_I <- info_I_test$Group
X_test_II <- info_II_test[, -ncol(info_II_test)]
Y_test_II <- info_II_test$Group
X_test_all<-c(X_test_I,X_test_II)
X_test_combined <- rbind(X_test_I, X_test_II)
Y_test_combined <- c(Y_test_I, Y_test_II)
rf_probs <- predict(best_model_rf, X_test_I, type = "prob")[,1]
roc_rf_I <-  roc(Y_test_I, rf_probs)
rf_probs <- predict(best_model_rf, X_test_II, type = "prob")[,1]
roc_rf_II <- roc(Y_test_II, rf_probs)
rf_probs <- predict(best_model_rf, X_test_combined, type = "prob")[,1]
roc_rf_combined <- roc(Y_test_combined, rf_probs)

pdf("rf_model_stage.pdf")
plot(roc_rf_combined, col = "blue", main = "ROC Curve Comparison", print.auc = F, lwd = 2)
plot(roc_rf_I, col = "red", add = TRUE, print.auc = F, lwd = 2)
plot(roc_rf_II , col = "green", add = TRUE, print.auc = F, lwd = 2)

ci_auc_train <- ci.auc(roc_rf_combined)
ci_auc_in <- ci.auc(roc_rf_I)
ci_auc_ex <- ci.auc(roc_rf_II)

auc_text_rf <- paste0("Combined AUC: ", round(auc(roc_rf_combined), 3), 
                      " (95% CI: ", round(ci_auc_train[1], 3), "-", round(ci_auc_train[3], 3), ")")


auc_text_in <- paste0("Validation I AUC: ", round(auc(roc_rf_I), 3), 
                      " (95% CI: ", round(ci_auc_in[1], 3), "-", round(ci_auc_in[3], 3), ")")


auc_text_ex <- paste0("Validation II AUC: ", round(auc(roc_rf_II ), 3), 
                      " (95% CI: ", round(ci_auc_ex[1], 3), "-", round(ci_auc_ex[3], 3), ")")
legend("bottomright", 
       legend = c(auc_text_rf, auc_text_in, auc_text_ex),
       col = c("blue", "red", "green"), lwd = 2, cex = 0.8)
dev.off()


y_pred_com <- predict(best_model_rf, X_test_combined)
cm_rf_com <- confusionMatrix(y_pred_com, Y_test_combined,positive = "Malignant")

y_pred_I <- predict(best_model_rf, X_test_I)
cm_rf_I <- confusionMatrix(y_pred_I, Y_test_I,positive = "Malignant")

y_pred_II <- predict(best_model_rf, X_test_II)
cm_rf_II <- confusionMatrix(y_pred_II, Y_test_II,positive = "Malignant")

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

n_train <- sum(cm_rf_com $table) 
n_in <- sum(cm_rf_I$table)       
n_ex <- sum(cm_rf_II$table)       
results_df <- add_row_to_df(results_df, "Combined", "Random Forest",
                          auc(roc_rf_combined)[1], ci_rf_train[1], ci_rf_train[3],
                          cm_rf_com, n_train)

results_df <- add_row_to_df(results_df, "Validation I", "Random Forest",
                          auc(roc_rf_I)[1], ci_rf_in[1], ci_rf_in[3],
                          cm_rf_I, n_in)

results_df <- add_row_to_df(results_df, "Validation II", "Random Forest",
                          auc(roc_rf_II)[1],  ci_rf_ex[1], ci_rf_ex[3],
                          cm_rf_II, n_ex)

write.csv(formatted_results, "AUC_model_performance_summary.csv", row.names = FALSE)
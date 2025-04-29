library(pROC)
library(ggplot2)
library(dplyr)
group_file <- "group.txt"
data <- read.table("lefse_input_form_group.xls", header = TRUE, sep = "\t", check.names = FALSE)

# Replace the characters in the first column
data[, 1] <- gsub("\\.", "_", data[, 1])
data[, 1] <- gsub("-", "_", data[, 1])
data[, 1] <- gsub("\\|", ".", data[, 1])
data[, 1] <- gsub("\\[", "_", data[, 1])
data[, 1] <- gsub("\\]", "_", data[, 1]) 
data[, 1] <- gsub("\\(", "_", data[, 1])
data[, 1] <- gsub("\\)", "_", data[, 1])


# Write the modified data back to the file
#write.table(data, "newname_lefse_input_form_group.xls", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
rownames(data)<-data$Taxonomy

import_info <- read.table("hmp_aerobiosis_small.res", header = F, sep = "\t", check.names = FALSE)

import_info[, 5] <- as.numeric(import_info[, 5])

# Filter the fifth column that is less than 0.05 and not NA
filtered_data <- import_info[import_info[, 5] < 0.05 & !is.na(import_info[, 5]), ]
# Extract 1st column
first_column <- filtered_data[, 1]
selected_rows <- first_column[grepl("s__", first_column)]
importance<-data[selected_rows,]
importance$Taxonomy<-NULL

otu_table<-importance
otu_table_t<-t(otu_table)
otu_table_t <- as.data.frame(otu_table_t)


rf_model_impor <- randomForest(
  Group ~ .,  # Suppose 'Group' is the target variable
  data = info_train,  
  importance = TRUE  
)
importance_scores <- importance(rf_model_impor, type = 1)
important_features <- importance_scores[importance_scores[, "MeanDecreaseAccuracy"] > 1, ]


##gender
Title="Gender"
pos<-"Male"
nes<-"Female"

Male_sample <- data_all %>% 
  filter(Gender == pos) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(Gender == nes) %>% 
  select(Sample) %>% 
  pull()


###Change the parameters

Title="Age"
pos<-"Old"
nes<-"Non-old"
yuzhi<-60

Male_sample <- data_all %>% 
  filter(Age>=yuzhi) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(Age<yuzhi) %>% 
  select(Sample) %>% 
  pull()


Title<-"Smoking"
pos<-"Smoking"
nes<-"No-Smoking"

Male_sample <- data_all %>% 
  filter(`Smoking_History` %in% c("Smoking","Quit-smoking")) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(`Smoking_History`== "Non-smoking") %>% 
  select(Sample) %>% 
  pull()

Title<-"Differentiation"
pos<-"Hign&Mid"
nes<-"Low"

Male_sample <- data_all %>% 
  filter(Differentiation %in% c("Mid","Hign")) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(Differentiation == "Low")  %>% 
  select(Sample) %>% 
  pull()

Title<-"Drinking"
pos<-"Drinking"
nes<-"No-drinking"

Male_sample <- data_all %>% 
  filter(Drinking_History %in% c("Drinking","Quit-drinking")) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(Drinking_History == "Non-drinking") %>% 
  select(Sample) %>% 
  pull()

pos<-"Male"
nes<-"Female"

Male_sample <- data_all %>% 
  filter(Gender == pos) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(Gender == nes) %>% 
  select(Sample) %>% 
  pull()


Title="HER2_Status"
pos<-"HER2+"
nes<-"HER2-"

Male_sample <- data_all %>% 
  filter(HER2_Status == pos) %>% 
  select(Sample) %>% 
  pull()

# Use the filter() function to filter out samples whose Age is less than 60
Female_sample <- data_all %>% 
  filter(HER2_Status ==nes) %>% 
  select(Sample) %>% 
  pull()

Title="MSS_Group"
pos<-"MSI-H"
nes<-"MSS"

Male_sample <- data_all %>% 
  filter(MSS_Group == pos) %>% 
  select(Sample) %>% 
  pull()

# Use the filter() function to filter out samples whose Age is less than 60
Female_sample <- data_all %>% 
  filter(MSS_Group == nes) %>% 
  select(Sample) %>% 
  pull()


###### If there's ">" symbol
Title="CA125"
pos<-"High"
nes<-"Low"
yuzhi<-35

Male_sample <- data_all %>% 
  filter(`CA125(<=35.00)`>yuzhi) %>% 
  select(Sample) %>% 
  pull()


Female_sample <- data_all %>% 
  filter(`CA125(<=35.00)`<=yuzhi) %>% 
  select(Sample) %>% 
  pull()

Title="CA242"
pos<-"High"
nes<-"Low"
yuzhi<-20

Male_sample <- data_all %>% 
  filter(`CA242(<=20.00)`>yuzhi) %>% 
  select(Sample) %>% 
  pull()


Female_sample <- data_all %>% 
  filter(`CA242(<=20.00)`<=yuzhi) %>% 
  select(Sample) %>% 
  pull()


Title="CA724"
pos<-"High"
nes<-"Low"
yuzhi<-6.9

Male_sample <- data_all %>% 
  filter(`CA724(<=6.90)`>yuzhi) %>% 
  select(Sample) %>% 
  pull()


Female_sample <- data_all %>% 
  filter(`CA724(<=6.90)`<=yuzhi) %>% 
  select(Sample) %>% 
  pull()

Title="Overall_Survival_time"
pos<-"over 3 years"
nes<-"less 3 years"
yuzhi<-36

Male_sample <- data_all %>% 
  filter(OS_time >= yuzhi) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(OS_time < yuzhi) %>% 
  select(Sample) %>% 
  pull()

Title="CEA"
pos<-"High"
nes<-"Low"
yuzhi<-5

Male_sample <- data_all %>% 
  filter(`CEA (<5.00)`>=yuzhi) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(`CEA (<5.00)`<yuzhi) %>% 
  select(Sample) %>% 
  pull()

Title="CA199"
pos<-"High"
nes<-"Low"
yuzhi<-34

Male_sample <- data_all %>% 
  filter(`CA199(<34.00)`>=yuzhi) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(`CA199(<34.00)`<yuzhi) %>% 
  select(Sample) %>% 
  pull()

Title="AFP"
pos<-"High"
nes<-"Low"
yuzhi<-7

Male_sample <- data_all %>% 
  filter(`AFP(<7.00)`>=yuzhi) %>% 
  select(Sample) %>% 
  pull()

Female_sample <- data_all %>% 
  filter(`AFP(<7.00)`<yuzhi) %>% 
  select(Sample) %>% 
  pull()


####stop!!!#####

disease_group_male <- data_all %>%
   filter(Sample %in% Male_sample) %>%
  select(Disease_Group) %>%
  pull()
disease_group_female <- data_all %>%
  filter(Sample %in% Female_sample) %>%
  select(Disease_Group) %>%
  pull()
info_male<-otu_table_t[Male_sample,]
info_male$Group<-disease_group_male
info_male$Group <- as.factor(info_male$Group)

info_female<-otu_table_t[Female_sample,]
info_female$Group<-disease_group_female
info_female$Group <- as.factor(info_female$Group)
X_male <- info_male[, -ncol(info_male)]
Y_male <- info_male$Group
X_female <- info_female[, -ncol(info_female)]
Y_female <- info_female$Group

#####if benign!=0
male_pred <- predict(best_model_rf, X_male)
cm_rf_male <- confusionMatrix(male_pred, Y_male,positive = "Malignant")
female_pred <- predict(best_model_rf, X_female)
cm_rf_female <- confusionMatrix(female_pred, Y_female,positive = "Malignant")
male_sens<-cm_rf_male$byClass["Sensitivity"]
female_sens<-cm_rf_female$byClass["Sensitivity"]
true_benign_m<-cm_rf_male$table[2,2]
false_malignant_m<-cm_rf_male$table[1,2]
true_benign_f <-cm_rf_female$table[2,2]
false_malignant_f <-cm_rf_female$table[1,2]

#####if benign=0
male_pred <- predict(best_model_rf, X_male)
true_benign_m = sum(Y_male == "Malignant" & male_pred == "Malignant") # TP

false_malignant_m =  sum(Y_male == "Malignant" & male_pred == "Benign")

male_sens<-true_benign_m/(true_benign_m+false_malignant_m)

female_pred <- predict(best_model_rf, X_female)
true_benign_f = sum(Y_female == "Malignant" & female_pred == "Malignant")
false_malignant_f = sum(Y_female == "Malignant" & female_pred == "Benign")

female_sens<-true_benign_f/(true_benign_f+false_malignant_f)

sensitivity_data <- data.frame(
  Gender = c(pos, nes),
  Sensitivity = c(male_sens,female_sens),
  true_benign =c(true_benign_m,true_benign_f),
  false_malignant=c(false_malignant_m,false_malignant_f)
)
sensitivity_data$Gender <- factor(sensitivity_data$Gender, levels = c(pos, nes))
pdf_filename <- paste0(Title, "_sensitivity.pdf")

pdf(pdf_filename)
p1 <- ggplot(sensitivity_data, aes(x = Gender, y = Sensitivity * 100, fill = Gender)) + 
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(y = Sensitivity * 50, label = sprintf("%.1f%%", Sensitivity * 100)),  
            vjust = 0.5, hjust = 0.5, color = "white", size = 5) +  
  geom_text(aes(label = paste0(Gender, " (", true_benign, "/", true_benign + false_malignant, ")")), vjust = -0.5) +
  labs(title = "Sensitivity", x = Title, y = "Sensitivity (%)") +  
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#00a381", "#824880"))  

print(p1)
dev.off()

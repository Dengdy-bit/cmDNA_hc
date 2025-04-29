Title="CA199"
pos<-"High"
nes<-"Normol"
yuzhi<-34

line<-"CA199(<34.00)"
Male_sample_early <- data_all %>% 
  filter(`CA199(<34.00)`>= yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Male_sample_late <- data_all %>% 
  filter(`CA199(<34.00)`>=yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()

Female_sample_early <- data_all %>% 
  filter(`CA199(<34.00)`< yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Female_sample_late<- data_all %>% 
  filter(`CA199(<34.00)`<yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()


Title="CEA"
pos<-"High"
nes<-"Normal"
yuzhi<-5

line<-"CEA(<5.00)"
Male_sample_early <- data_all %>% 
  filter(`CEA (<5.00)`>= yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Male_sample_late <- data_all %>% 
  filter(`CEA (<5.00)`>=yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()

Female_sample_early <- data_all %>% 
  filter(`CEA (<5.00)`< yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Female_sample_late<- data_all %>% 
  filter(`CEA (<5.00)`<yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()


Title="AFP(<7.00)"
pos<-"High"
nes<-"Normal"
yuzhi<-7

line<-"AFP(<7.00)"
Male_sample_early <- data_all %>% 
  filter(`AFP(<7.00)`>= yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Male_sample_late <- data_all %>% 
  filter(`AFP(<7.00)`>=yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()

Female_sample_early <- data_all %>% 
  filter(`AFP(<7.00)`< yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Female_sample_late<- data_all %>% 
  filter(`AFP(<7.00)`<yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()

Title="CA125(<=35.00)"
pos<-"High"
nes<-"Normal"
yuzhi<-35

line<-"CA125(<=35.00)"
Male_sample_early <- data_all %>% 
  filter(`CA125(<=35.00)`> yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Male_sample_late <- data_all %>% 
  filter(`CA125(<=35.00)`>yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()

Female_sample_early <- data_all %>% 
  filter(`CA125(<=35.00)`< yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Female_sample_late<- data_all %>% 
  filter(`CA125(<=35.00)`<yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()


Title="CA242"
pos<-"High"
nes<-"Normal"
yuzhi<-20
line<-"CA242(<=20.00)"
Male_sample_early <- data_all %>% 
  filter(`CA242(<=20.00)`> yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Male_sample_late <- data_all %>% 
  filter(`CA242(<=20.00)`>yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()

Female_sample_early <- data_all %>% 
  filter(`CA242(<=20.00)`< yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Female_sample_late<- data_all %>% 
  filter(`CA242(<=20.00)`<yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()


Title="CA724"
pos<-"High"
nes<-"Normal"
yuzhi<-6.9
line<-"CA724(<=6.90)"
Male_sample_early <- data_all %>% 
  filter(`CA724(<=6.90)`> yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Male_sample_late <- data_all %>% 
  filter(`CA724(<=6.90)`>yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()

Female_sample_early <- data_all %>% 
  filter(`CA724(<=6.90)`< yuzhi,Stage == "Early") %>% 
  select(Sample) %>% 
  pull()

Female_sample_late<- data_all %>% 
  filter(`CA724(<=6.90)`<yuzhi,Stage == "Late") %>% 
  select(Sample) %>% 
  pull()


disease_group_male_early <- data_all %>%
   filter(Sample %in% Male_sample_early) %>%
  select(Disease_Group) %>%
  pull()

disease_group_male_late <- data_all %>%
   filter(Sample %in% Male_sample_late) %>%
  select(Disease_Group) %>%
  pull()

disease_group_female_early <- data_all %>%
  filter(Sample %in% Female_sample_early) %>%
  select(Disease_Group) %>%
  pull()
 
disease_group_female_late <- data_all %>%
  filter(Sample %in% Female_sample_late) %>%
  select(Disease_Group) %>%
  pull()

info_male_e<-otu_table_t[Male_sample_early,]
info_male_e$Group<-disease_group_male_early
info_male_e$Group <- as.factor(info_male_e$Group)

info_female_e<-otu_table_t[Female_sample_early,]
info_female_e$Group<-disease_group_female_early
info_female_e$Group <- as.factor(info_female_e$Group)

info_male_l<-otu_table_t[Male_sample_late,]
info_male_l$Group<-disease_group_male_late
info_male_l$Group <- as.factor(info_male_l$Group)

info_female_l<-otu_table_t[Female_sample_late,]
info_female_l$Group<-disease_group_female_late
info_female_l$Group <- as.factor(info_female_l$Group)

X_male_e <- info_male_e[, -ncol(info_male_e)]
Y_male_e <- info_male_e$Group
X_male_l <- info_male_l[, -ncol(info_male_l)]
Y_male_l <- info_male_l$Group
X_female_e <- info_female_e[, -ncol(info_female_e)]
Y_female_e <- info_female_e$Group
X_female_l <- info_female_l[, -ncol(info_female_l)]
Y_female_l <- info_female_l$Group

male_pred_e <- predict(best_model_rf, X_male_e)

true_benign_m_e = sum(Y_male_e == "Malignant" & male_pred_e == "Malignant")

false_malignant_m_e =  sum(Y_male_e == "Malignant" & male_pred_e == "Benign")  

male_sens_e<-true_benign_m_e/(true_benign_m_e+false_malignant_m_e)

male_pred_l <- predict(best_model_rf, X_male_l)

true_benign_m_l = sum(Y_male_l == "Malignant" & male_pred_l == "Malignant")
#false_benign =sum(Y_male == "Benign" & male_pred == "Malignant")
#true_malignant =sum(Y_male == "Benign" & male_pred == "Benign")  # TP
false_malignant_m_l =  sum(Y_male_l == "Malignant" & male_pred_l == "Benign")  # FP  # FN

male_sens_l<-true_benign_m_l/(true_benign_m_l+false_malignant_m_l )

female_pred_e <- predict(best_model_rf, X_female_e)
true_benign_f_e = sum(Y_female_e == "Malignant" & female_pred_e == "Malignant")  # TP
false_malignant_f_e = sum(Y_female_e == "Malignant" & female_pred_e == "Benign")

female_sens_e<-true_benign_f_e/(true_benign_f_e+false_malignant_f_e)

female_pred_l <- predict(best_model_rf, X_female_l)
true_benign_f_l = sum(Y_female_l == "Malignant" & female_pred_l == "Malignant")  # TP
false_malignant_f_l = sum(Y_female_l == "Malignant" & female_pred_l == "Benign")

female_sens_l<-true_benign_f_l/(true_benign_f_l+false_malignant_f_l)

pos_e<-"High&Early"
pos_l<-"High&Late"
nes_e<-"Normal&Early"
nes_l<-"Normal&Late"

Sensitivity_data <- data.frame(
  Gender = c(pos_e, pos_l,nes_e,nes_l),
  Sensitivity = c(male_sens_e,male_sens_l,female_sens_e,female_sens_l),
  true_benign =c(true_benign_m_e,true_benign_m_l,true_benign_f_e,true_benign_f_l),
  false_malignant=c(false_malignant_m_e,false_malignant_m_l,false_malignant_f_e,false_malignant_f_l)
)

pdf_filename <- paste0(Title, "_sensitivity_v2.pdf")  # Create PDF filename
pdf(pdf_filename)  # Start PDF device

# Generate the plot
p1 <- ggplot(Sensitivity_data, aes(x = Gender, y = Sensitivity * 100, fill = Gender)) +  
  geom_bar(stat = "identity", width = 0.5) +  # Create bar plot with specified width
  geom_text(aes(y = Sensitivity * 50, label = sprintf("%.1f%%", Sensitivity * 100)),  
            vjust = 0.5, hjust = 0.5, color = "white", size = 5) +  # Add percentage labels
  geom_text(aes(label = paste0(Gender, " (", true_benign, "/", true_benign + false_malignant, ")")), vjust = -0.5) +  # Add true/false counts
  labs(title = paste0("Sensitivity ",Title), x = Title, y = "Sensitivity (%)") +  # Label axes and title
  theme_minimal() +  # Minimal theme
  theme(legend.position = "none") +  # Remove legend
  scale_fill_manual(values = c("High&Early" = "#00a381", "High&Late" = "#00a381", 
                               "Normal&Early" = "#824880", "Normal&Late" = "#824880"))  # Set colors for High and Low categories

print(p1)  # Print the plot to the PDF
dev.off()  # Close the PDF device

##smoking





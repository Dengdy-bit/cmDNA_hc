library(ggplot2)
library(ggsignif)
Val_I_table<-predict(best_model_rf, X_test_I, type = "prob")
Val_II_table<-predict(best_model_rf, X_test_II, type = "prob")
Val_I_table$Sample<-rownames(Val_I_table)
Val_II_table$Sample<-rownames(Val_II_table)
group_type<-read.table("../../group_Group.txt",sep = "\t",col.names = c("SampleID", "Group"),row.names = 1)
group_type$SampleID<-rownames(group_type)
Val_I_type<-group_type[rownames(Val_I_table),]
Val_II_type<-group_type[rownames(Val_II_table),]
merged_df_I<- merge(Val_I_table,Val_I_type, by.x = "Sample", by.y = "SampleID")
merged_df_II <- merge(Val_II_table,Val_II_type, by.x = "Sample", by.y = "SampleID")


pdf("Cancer_Scores_Val_I.pdf")
ggplot(merged_df_I, aes(x = Group, y = Malignant, fill = Group)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.6),  # 调整箱线图之间的空隙
               outlier.shape = NA, size = 0.9, width = 0.5, alpha = 0.8) +  # 调整箱线图边框颜色、大小、宽度和透明度
  geom_jitter(position = position_jitterdodge(jitter.width = 0.25, # 散点抖动宽度
                                              dodge.width = 0.7))+  # 减小散点抖动范围的宽度，避免散点溢出
  geom_signif(
    comparisons = list(c("Benign", "Malignant")),
    step_increase = 0.3,
    map_signif_level = TRUE,
    test = wilcox.test
  )+  # 添加显著性标注
  scale_fill_manual(values = c("#0072B5", "#E18727",
                               "#de1c31")) +  # 蓝色和橙色配色方案
  scale_color_manual(values = c("#0072B5", "#E18727","#de1c31")) +  # 蓝色和橙色的散点颜色
  theme_minimal(base_size = 14) +  # 简洁的主题和更大字号
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_line(size = 1, color = "black"),  # 添加坐标轴线并加粗
    panel.grid = element_blank(),  # 移除网格线
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(title = "Validation I ", y = "Cancer Score", x = "") +
  theme_minimal() +
  theme(legend.position = "none") # 隐藏图例
dev.off()

pdf("Cancer_Scores_Val_II.pdf")
ggplot(merged_df_II, aes(x = Group, y = Malignant, fill = Group)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.6),  # 调整箱线图之间的空隙
               outlier.shape = NA, size = 0.9, width = 0.5, alpha = 0.8) +  # 调整箱线图边框颜色、大小、宽度和透明度
  geom_jitter(position = position_jitterdodge(jitter.width = 0.25, # 散点抖动宽度
                                              dodge.width = 0.7))+  # 减小散点抖动范围的宽度，避免散点溢出
  geom_signif(
    comparisons = list(c("Benign", "Malignant")),
    step_increase = 0.3,
    map_signif_level = TRUE,
    test = wilcox.test
  )+  # 添加显著性标注
  scale_fill_manual(values = c("#0072B5", "#E18727",
                               "#de1c31")) +  # 蓝色和橙色配色方案
  scale_color_manual(values = c("#0072B5", "#E18727","#de1c31")) +  # 蓝色和橙色的散点颜色
  theme_minimal(base_size = 14) +  # 简洁的主题和更大字号
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_line(size = 1, color = "black"),  # 添加坐标轴线并加粗
    panel.grid = element_blank(),  # 移除网格线
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(title = "Validation II ", y = "Cancer Score", x = "") +
  theme_minimal() +
  theme(legend.position = "none") # 隐藏图例
dev.off()
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
args = commandArgs(T)

otu_file<-args[1]
base_name <- tools::file_path_sans_ext(basename(otu_file)) 
otu <- read.table(otu_file, header = TRUE,sep="\t",check.names = FALSE)

rownames(otu)<-otu$Tax_detail
otu$Tax_detail<-NULL
sample_group <- read.table("../../group_Group.txt", header = F, sep = "\t", stringsAsFactors = F)
colnames(sample_group) <- c("SampleID", "Group")
abundance_data<-otu
abundance_data<-abundance_data[,sample_group$SampleID]

# Generate the result matrix (including p-value and average abundance)
results <- apply(abundance_data, 1, function(species) {
  # Extract grouped data
  group_benign <- species[sample_group$Group == "Benign"]
  group_malignant <- species[sample_group$Group == "Malignant"]
  
  # Calculate the average abundance
  mean_benign <- mean(group_benign, na.rm = TRUE)
  mean_malignant <- mean(group_malignant, na.rm = TRUE)
  
  # Perform the Wilcoxon test ）
  if(length(group_benign) >= 1 & length(group_malignant) >= 1) {
    test <- wilcox.test(group_benign, group_malignant)
    p_value <- test$p.value
  } else {
    p_value <- NA
  }
  
  # Return the result
  return(c(
    p.value = p_value,
    mean_benign = mean_benign,
    mean_malignant = mean_malignant
  ))
})

# Convert to a data frame and process the format
results_df <- as.data.frame(t(results)) %>%
  tibble::rownames_to_column("Taxonomy") %>%
  mutate(
    # Sort by total average abundance (the sorting basis can be modified as needed)
    total_mean = mean_benign + mean_malignant,
    # FDR
    fdr = p.adjust(p.value, method = "BH")
  ) %>%
  # Arranged in descending order of total average abundance
  arrange(desc(total_mean)) %>%
  select(Taxonomy, mean_benign, mean_malignant, total_mean, p.value, fdr)

sorted_results <-results_df %>% arrange(desc(total_mean))

significant_species_benign <- sorted_results %>%
  filter(p.value < 0.05)

output_file <- paste0(base_name, "_wilcox_results.xls")
write.table(results_df , output_file, sep = "\t", row.names = FALSE, quote = FALSE)

group_info <- sample_group$Group

significant_otus<-abundance_data[significant_species_benign$Taxonomy,]

new_rownames <- gsub(".*__([^;]+);?$", "\\1", significant_species_benign$Taxonomy)

new_rownames <-make.unique(new_rownames)
rownames(significant_otus)<-new_rownames 
significant_species_benign$Taxonomy<-new_rownames 
significant_otus$Taxonomy <-rownames(significant_otus)

long_data <- significant_otus %>%
  pivot_longer(-Taxonomy, names_to = "Sample", values_to = "Abundance") %>%
  left_join(sample_group, by = c("Sample" = "SampleID"))

max_taxonomy_per_plot <- 15

# Grouped by Taxonomy
taxonomy_groups <- split(unique(long_data$Taxonomy), 
                         ceiling(seq_along(unique(long_data$Taxonomy)) / max_taxonomy_per_plot))

# Create corresponding subsets for each group of Taxonomy and draw images
for (i in seq_along(taxonomy_groups)) {
  # Extract the Taxonomy 
  current_taxonomy <- taxonomy_groups[[i]]
  
  # Filter the data
  current_data <- long_data %>% filter(Taxonomy %in% current_taxonomy)
  
  current_significant <- subset(significant_species_benign, Taxonomy == current_taxonomy)
  annotation <-  current_significant %>%
	mutate(p_label = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)))
  max_abundance <- current_data %>%
	group_by(Taxonomy) %>%
	summarise(max_abun = max(Abundance, na.rm = TRUE), .groups = "drop")

  #Merge the annotation
  annotation <- annotation %>%
  left_join(max_abundance, by = "Taxonomy") %>%
  mutate(ypos = max_abun * 1.1)
  
  p <- ggplot(current_data, aes(x = Taxonomy, y = Abundance, fill = Group)) +
  geom_boxplot(
    aes(color = Group),
    fill = "white",
    outlier.shape = NA,
    position = position_dodge(width = 0.8),
    alpha = 0.8,
    width = 0.6,
    size = 0.8
  ) +
  geom_jitter(
    aes(color = Group),
    position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.8),
    size = 0.8
  ) +
  scale_fill_manual(values = c("#2b83ba", "#fdae61")) +
  scale_color_manual(values = c("#2b83ba", "#fdae61")) +
  labs(
    title = paste("Significant Microbial Taxonomy Abundance Across Groups - Part", i),
    x = "Taxonomy",
    y = "Relative Abundance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
   
    axis.line = element_line(color = "black", size = 0.5),      
    axis.ticks = element_line(color = "black", size = 0.5),     
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
   
    panel.background = element_blank(),
    panel.border = element_blank()
  )+
geom_text(data = annotation, 
          mapping = aes(x = Taxonomy, y = max(ypos), label = p_label),
          inherit.aes = FALSE,
          size = 4, color = "black", vjust = 0.5)

  
  # Save the image to PDF and PNG
  pdf(file = paste0(base_name, "_significant_boxplot_part", i, ".pdf"), width = 12, height = 8)
  print(p)
  dev.off()
  
  png(file = paste0(base_name, "_significant_boxplot_part", i, ".png"), width = 12, height = 8, units = "in", res = 300)
  print(p)
  dev.off()
}

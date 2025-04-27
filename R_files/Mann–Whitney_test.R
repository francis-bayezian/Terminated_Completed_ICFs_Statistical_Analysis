# Load required packages
library(rstatix)
library(ggpubr)
library(dplyr)

# Load data
icf_data <- read.csv("./Data/reprocessing/icf_similarity_analysis_for_R.csv")

# Set clean plotting theme
theme_set(theme_minimal())

# Function to perform Wilcoxon test and report group medians
perform_wilcox_test_with_medians <- function(data, y_var, group_var) {
  cat("\n###", y_var, "by", group_var, "###\n")
  
  # Calculate group medians and IQR
  summary_stats <- data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      median = median(.data[[y_var]], na.rm = TRUE),
      IQR = IQR(.data[[y_var]], na.rm = TRUE),
      n = n()
    )
  print(summary_stats)
  
  # Perform Wilcoxon test
  test_result <- wilcox_test(data, formula = as.formula(paste(y_var, "~", group_var)))
  test_result <- add_significance(test_result)
  print(test_result)
}

# Function to create boxplots
create_boxplot <- function(data, x_var, y_var, title, y_label, y_position) {
  ggboxplot(data, x = x_var, y = y_var, fill = x_var,
            palette = "Set2", add = "jitter", shape = x_var, width = 0.6) +
    stat_compare_means(method = "wilcox.test", label.y = y_position) +
    labs(title = title, x = "", y = y_label) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      legend.position = "none"
    )
}

################################################################################
# Statistical Analysis with Medians

# H1: Cosine Similarity
perform_wilcox_test_with_medians(icf_data, "group_mean_cosine", "trial_status")

# H2: Jaccard Similarity
perform_wilcox_test_with_medians(icf_data, "group_mean_jaccard", "trial_status")

# H3: Edit Distance
perform_wilcox_test_with_medians(icf_data, "group_mean_edit_dist", "trial_status")

# Clause Count
perform_wilcox_test_with_medians(icf_data, "clause_count", "trial_status")

################################################################################
# Create Boxplots

# Clause Count Plot
create_boxplot(icf_data, "trial_status", "clause_count",
               "Clause Count by Trial Status", "Number of Clauses", y_position = 18)

# Cosine Similarity Plot
create_boxplot(icf_data, "trial_status", "group_mean_cosine",
               "Cosine Similarity by Trial Status", "Group Median Cosine Similarity", y_position = 0.75)

# Jaccard Similarity Plot
create_boxplot(icf_data, "trial_status", "group_mean_jaccard",
               "Jaccard Similarity by Trial Status", "Group Median Jaccard Similarity", y_position = 1.05)

# Edit Distance Plot
create_boxplot(icf_data, "trial_status", "group_mean_edit_dist",
               "Edit Distance by Trial Status", "Group Median Edit Distance", y_position = 1.02)

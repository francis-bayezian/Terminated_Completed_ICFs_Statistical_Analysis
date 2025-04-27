# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Load the ICF similarity dataset
icf_data <- read.csv("./Data/reprocessing/icf_similarity_analysis_for_R.csv")

# Preview the data
head(icf_data)

# Summary for numeric variables
summary(icf_data)

# Descriptive stats grouped by trial_status
icf_data %>%
  group_by(trial_status) %>%
  summarise(
    n = n(),
    mean_clause_count = mean(clause_count, na.rm = TRUE),
    sd_clause_count = sd(clause_count, na.rm = TRUE),
    mean_cosine = mean(group_mean_cosine, na.rm = TRUE),
    sd_cosine = sd(group_mean_cosine, na.rm = TRUE),
    mean_jaccard = mean(group_mean_jaccard, na.rm = TRUE),
    sd_jaccard = sd(group_mean_jaccard, na.rm = TRUE),
    mean_edit_distance = mean(group_mean_edit_dist, na.rm = TRUE),
    sd_edit_distance = sd(group_mean_edit_dist, na.rm = TRUE)
  )

# Clause count by trial status
ggplot(icf_data, aes(x = trial_status, y = clause_count, fill = trial_status)) +
  geom_boxplot() +
  labs(title = "Clause Count by Trial Status", y = "Number of Clauses") +
  theme_minimal()

# Cosine similarity by trial status
ggplot(icf_data, aes(x = trial_status, y = group_mean_cosine, fill = trial_status)) +
  geom_boxplot() +
  labs(title = "Cosine Similarity by Trial Status", y = "Group Mean Cosine") +
  theme_minimal()

# Jaccard similarity by trial status
ggplot(icf_data, aes(x = trial_status, y = group_mean_jaccard, fill = trial_status)) +
  geom_boxplot() +
  labs(title = "Jaccard Similarity by Trial Status", y = "Group Mean Jaccard") +
  theme_minimal()

# Edit distance by trial status
ggplot(icf_data, aes(x = trial_status, y = group_mean_edit_dist, fill = trial_status)) +
  geom_boxplot() +
  labs(title = "Edit Distance by Trial Status", y = "Group Mean Edit Distance") +
  theme_minimal()


##############################################################################################################
# Descriptive Statistics:
# Across the metrics evaluated (clause count, cosine similarity, Jaccard similarity, and edit distance), completed trials tended to demonstrate higher internal similarity than terminated trials. 
# Cosine and Jaccard metrics particularly indicated stronger internal consistency among completed trials, supporting hypotheses regarding linguistic and ethical clause coherence. 
# Differences in clause count and edit distance appeared visually minimal, suggesting more subtle structural distinctions.
##############################################################################################################
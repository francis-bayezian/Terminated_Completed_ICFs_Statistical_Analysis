# Load libraries
library(tidyverse)
library(pscl)              # For pseudo-R²
library(ResourceSelection) # For Hosmer-Lemeshow Test
library(ggplot2)
library(broom)

# Load data
icf_data <- read.csv("./Data/reprocessing/icf_similarity_analysis_for_R.csv")

# Prepare data
icf_data$file_name <- str_trim(icf_data$file_name)
icf_data$trial_status <- factor(icf_data$trial_status, levels = c("terminated", "completed"))

# Build logistic regression model
logit_model <- glm(trial_status ~ group_mean_cosine + group_mean_jaccard +
                     group_mean_edit_dist + clause_count,
                   data = icf_data, family = binomial)

# View model summary
summary(logit_model)

# --- Model Evaluation ---

# 1. AIC (lower = better)
model_aic <- AIC(logit_model)
cat("Model AIC:", model_aic, "\n\n")

# 2. McFadden's R-squared
pseudo_r2 <- pR2(logit_model)
cat("McFadden's R²:", pseudo_r2["McFadden"], "\n\n")

# 3. Hosmer-Lemeshow Goodness of Fit Test
hoslem_result <- hoslem.test(as.integer(icf_data$trial_status) - 1, fitted(logit_model))
print(hoslem_result)
################################################################################

# Calculate Odds Ratios (exp(coef))
odds_ratios <- exp(coef(logit_model))

# Calculate 95% Confidence Intervals for Odds Ratios
conf_intervals <- exp(confint(logit_model))

# Combine into a tidy table
odds_table <- data.frame(
  Predictor = names(odds_ratios),
  Odds_Ratio = odds_ratios,
  CI_Lower = conf_intervals[, 1],
  CI_Upper = conf_intervals[, 2]
)

# View the Odds Ratios table
print(odds_table)

############################### Probability Plottings #################################################-
plot_predicted_curve <- function(variable_name, variable_label, color_choice) {
  
  # Create a sequence of values for the varying predictor
  seq_var <- seq(min(icf_data[[variable_name]], na.rm = TRUE),
                 max(icf_data[[variable_name]], na.rm = TRUE),
                 length.out = 100)
  
  # Create a dataframe that repeats means across 100 rows
  new_data <- data.frame(
    group_mean_cosine = rep(mean(icf_data$group_mean_cosine, na.rm = TRUE), 100),
    group_mean_jaccard = rep(mean(icf_data$group_mean_jaccard, na.rm = TRUE), 100),
    group_mean_edit_dist = rep(mean(icf_data$group_mean_edit_dist, na.rm = TRUE), 100),
    clause_count = rep(mean(icf_data$clause_count, na.rm = TRUE), 100)
  )
  
  # Replace the variable we want to vary
  new_data[[variable_name]] <- seq_var
  
  # Predict probabilities using the logistic model
  new_data$predicted_prob <- predict(logit_model, newdata = new_data, type = "response")
  
  # Generate the plot
  ggplot(new_data, aes_string(x = variable_name, y = "predicted_prob")) +
    geom_line(color = color_choice, size = 2) +
    geom_ribbon(aes(ymin = predicted_prob - 0.05, ymax = predicted_prob + 0.05),
                fill = color_choice, alpha = 0.2) +
    labs(
      title = paste("Predicted Probability vs", variable_label),
      x = variable_label,
      y = "Predicted Probability of Completion"
    ) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
      axis.title.x = element_text(face = "bold", size = 16),
      axis.title.y = element_text(face = "bold", size = 16),
      axis.text = element_text(size = 14)
    )
}

# Generate Each Plot Individually

# H1: Group Mean Cosine Similarity
plot_cosine <- plot_predicted_curve("group_mean_cosine", "Group Mean Cosine Similarity", "blue")

# H2: Group Mean Jaccard Similarity
plot_jaccard <- plot_predicted_curve("group_mean_jaccard", "Group Mean Jaccard Similarity", "darkgreen")

# H3: Group Mean Edit Distance
plot_edit <- plot_predicted_curve("group_mean_edit_dist", "Group Mean Edit Distance", "red")

#Combine All Plots into 1 Figure

combined_prediction_plot <- ggarrange(
  plot_cosine,
  plot_jaccard,
  plot_edit,
  ncol = 1, nrow = 3,
  labels = c("A", "B", "C"),   # Adds (A), (B), (C) labels
  align = "v",                 # Vertical alignment
  common.legend = FALSE
)

ggsave(
  filename = "./Data/reprocessing/predicted_probabilities_by_hypothese.png",
  plot = combined_prediction_plot,
  width = 8, height = 14, dpi = 400
)

############################### Odds Ratio Forest Plot #################################################-
# Extract odds ratios and confidence intervals
tidy_logit <- tidy(logit_model) %>%
  mutate(
    OR = exp(estimate),
    OR_low = exp(estimate - 1.96 * std.error),
    OR_high = exp(estimate + 1.96 * std.error)
  )

# Remove intercept (optional)
tidy_logit <- tidy_logit %>% filter(term != "(Intercept)")

# Clean predictor labels for publication
tidy_logit$term <- recode(tidy_logit$term,
                          "group_mean_cosine" = "Group Mean Cosine Similarity",
                          "group_mean_jaccard" = "Group Mean Jaccard Similarity",
                          "group_mean_edit_dist" = "Group Mean Edit Distance",
                          "clause_count" = "Clause Count")

# --- Create Forest Plot ---

forest_plot <- ggplot(tidy_logit, aes(x = OR, y = fct_reorder(term, OR))) +
  geom_point(color = "blue", size = 3) +
  geom_errorbarh(aes(xmin = OR_low, xmax = OR_high), height = 0.2, color = "blue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  scale_x_log10() +
  labs(
    title = "Odds Ratios for Predictors of Trial Completion",
    x = "Odds Ratio (log scale)",
    y = ""
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14)
  )

# --- Show the plot
print(forest_plot)

# --- Save the plot (high-resolution)
ggsave("./Data/reprocessing/forest_plot_odds_ratios.png",
       plot = forest_plot, width = 9, height = 6, dpi = 400)
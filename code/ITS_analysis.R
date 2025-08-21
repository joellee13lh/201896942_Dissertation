# ==============================================================================
# Interrupted Time Series Analysis for Diabetes Prescribing Patterns
# West Yorkshire, England - ICS Implementation Impact Assessment
# ==============================================================================

# Load required packages
library(tidyverse)
library(ggplot2)
library(patchwork)

# Load data
data <- read.csv("diabetes_its_analysis_data_lsoa.csv")
data$date <- as.Date(data$date)

# Set intervention date (ICS implementation)
intervention_date <- as.Date("2022-07-01")

# ==============================================================================
# ITS Analysis Function with Control Variable
# ==============================================================================

run_its_analysis <- function(data, outcome_var) {
  
  # Prepare monthly aggregated data
  monthly_data <- data %>%
    group_by(date, time_point, intervention, post_intervention) %>%
    summarise(
      outcome = median(get(outcome_var), na.rm = TRUE),
      control_var = median(total_diabetes_items, na.rm = TRUE),
      n_lsoas = n(),
      .groups = "drop"
    ) %>%
    arrange(date)
  
  # ITS model with control variable
  model <- lm(outcome ~ time_point + intervention + post_intervention + control_var, 
              data = monthly_data)
  
  # Generate counterfactual (removing intervention effects)
  counterfactual_data <- monthly_data %>%
    mutate(intervention = 0, post_intervention = 0)
  
  monthly_data$counterfactual <- predict(model, counterfactual_data)
  
  # Calculate confidence intervals
  pred_se <- predict(model, counterfactual_data, se.fit = TRUE)$se.fit
  monthly_data$counterfactual_lower <- monthly_data$counterfactual - 1.96 * pred_se
  monthly_data$counterfactual_upper <- monthly_data$counterfactual + 1.96 * pred_se
  
  # Calculate effects
  monthly_data$pointwise_effect <- monthly_data$outcome - monthly_data$counterfactual
  monthly_data$cumulative_effect <- cumsum(ifelse(monthly_data$intervention == 1, 
                                                  monthly_data$pointwise_effect, 0))
  
  return(list(data = monthly_data, model = model))
}

# ==============================================================================
# Create ITS Visualization
# ==============================================================================

create_its_plot <- function(its_result, title) {
  
  monthly_data <- its_result$data
  
  # Common theme
  theme_its <- theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 10, face = "bold")
    )
  
  # Panel 1: Observed vs Counterfactual
  p1 <- ggplot(monthly_data, aes(x = date)) +
    geom_ribbon(aes(ymin = counterfactual_lower, ymax = counterfactual_upper),
                fill = "grey70", alpha = 0.5) +
    geom_line(aes(y = counterfactual), color = "white", linetype = "dashed", size = 1.2) +
    geom_line(aes(y = outcome), color = "black", size = 1.2) +
    geom_vline(xintercept = intervention_date, linetype = "dotted", color = "red", alpha = 0.7) +
    labs(title = "Original", x = "", y = paste(title, "(median)")) +
    theme_its +
    theme(axis.text.x = element_blank())
  
  # Panel 2: Pointwise Effects
  p2 <- ggplot(monthly_data, aes(x = date)) +
    geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +
    geom_line(aes(y = pointwise_effect), color = "black", size = 1) +
    geom_vline(xintercept = intervention_date, linetype = "dotted", color = "red", alpha = 0.7) +
    labs(title = "Pointwise", x = "", y = "Effect size") +
    theme_its +
    theme(axis.text.x = element_blank())
  
  # Panel 3: Cumulative Effects
  p3 <- ggplot(monthly_data, aes(x = date)) +
    geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +
    geom_line(aes(y = cumulative_effect), color = "black", size = 1) +
    geom_vline(xintercept = intervention_date, linetype = "dotted", color = "red", alpha = 0.7) +
    labs(title = "Cumulative", x = "Date", y = "Cumulative effect") +
    theme_its +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Combine panels
  combined_plot <- p1 / p2 / p3 +
    plot_annotation(
      title = paste("Interrupted Time Series Analysis:", title),
      subtitle = "Black line = Observed; White dashed = Counterfactual (95% CI); Red line = ICS Implementation",
      theme = theme(plot.title = element_text(size = 14, face = "bold"),
                    plot.subtitle = element_text(size = 11))
    )
  
  return(combined_plot)
}

# ==============================================================================
# Main Analysis
# ==============================================================================

cat("Running ITS Analysis for Diabetes Prescribing Indicators...\n\n")

# Run analyses for three core indicators
results_IDR <- run_its_analysis(data, "IDR")
results_ATU <- run_its_analysis(data, "ATU") 
results_HMR <- run_its_analysis(data, "HMR")

# Generate plots
plot_IDR <- create_its_plot(results_IDR, "Insulin Dependency Ratio (IDR)")
plot_ATU <- create_its_plot(results_ATU, "Advanced Therapy Utilization (ATU)")
plot_HMR <- create_its_plot(results_HMR, "Hypoglycemia Risk Management Ratio (HMR)")

# Display plots
plot_IDR
plot_ATU
plot_HMR

# Save plots
output_dir <- './plots_its/'
if (!dir.exists(output_dir)) dir.create(output_dir)

ggsave(paste0(output_dir, "IDR_its_analysis.png"), plot_IDR, 
       width = 12, height = 10, dpi = 300)
ggsave(paste0(output_dir, "ATU_its_analysis.png"), plot_ATU, 
       width = 12, height = 10, dpi = 300)
ggsave(paste0(output_dir, "HMR_its_analysis.png"), plot_HMR, 
       width = 12, height = 10, dpi = 300)

# ==============================================================================
# Model Results Summary
# ==============================================================================

cat("ITS Model Results Summary\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

models <- list(IDR = results_IDR$model, ATU = results_ATU$model, HMR = results_HMR$model)

# Create summary table
summary_table <- data.frame(
  Indicator = character(),
  Immediate_Effect = numeric(),
  Immediate_SE = numeric(),
  Immediate_P = numeric(),
  Slope_Change = numeric(),
  Slope_SE = numeric(),
  Slope_P = numeric(),
  R_squared = numeric(),
  stringsAsFactors = FALSE
)

for(name in names(models)) {
  model <- models[[name]]
  coeffs <- summary(model)$coefficients
  
  summary_table <- rbind(summary_table, data.frame(
    Indicator = name,
    Immediate_Effect = round(coeffs["intervention", "Estimate"], 4),
    Immediate_SE = round(coeffs["intervention", "Std. Error"], 4),
    Immediate_P = round(coeffs["intervention", "Pr(>|t|)"], 4),
    Slope_Change = round(coeffs["post_intervention", "Estimate"], 4),
    Slope_SE = round(coeffs["post_intervention", "Std. Error"], 4),
    Slope_P = round(coeffs["post_intervention", "Pr(>|t|)"], 4),
    R_squared = round(summary(model)$r.squared, 4)
  ))
}

print(summary_table)

# Calculate effect sizes
cat("\n\nEffect Size Summary\n")
cat(paste(rep("=", 40), collapse = ""), "\n")

for(name in names(models)) {
  result <- get(paste0("results_", name))
  post_data <- result$data[result$data$intervention == 1, ]
  
  if(nrow(post_data) > 0) {
    mean_effect <- round(mean(post_data$pointwise_effect, na.rm = TRUE), 4)
    final_cumulative <- round(tail(post_data$cumulative_effect, 1), 4)
    
    cat(paste0(name, ":\n"))
    cat(paste0("  Mean pointwise effect: ", mean_effect, "\n"))
    cat(paste0("  Final cumulative effect: ", final_cumulative, "\n\n"))
  }
}

cat("Analysis completed successfully!\n")
cat(paste0("Plots saved in: ", output_dir, "\n"))

# ==============================================================================
# Export results for further analysis
# ==============================================================================

# Save model summary table
write.csv(summary_table, paste0(output_dir, "its_model_summary.csv"), row.names = FALSE)

cat("Model summary exported to: its_model_summary.csv\n")

# Diabetes Prescription Analysis by Index of Multiple Deprivation (IMD)
# Analysis of diabetes prescribing patterns across socioeconomic groups in West Yorkshire

# Load required libraries
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(tidybayes)

# Create output directory
output_dir_plots <- './plots_diabetes/'
if (!dir.exists(output_dir_plots)) {
  dir.create(output_dir_plots)
}

# Load data
diabetes_data <- read.csv("diabetes_its_analysis_data_lsoa.csv")
diabetes_data$date <- as.Date(diabetes_data$date)

# Load and process IMD data
imd_data <- read.csv("LSOA_decile.csv")
imd_clean <- imd_data %>%
  rename(LSOA_CODE = FeatureCode, IMD_decile_rank = Value) %>%
  select(LSOA_CODE, IMD_decile_rank) %>%
  mutate(IMD_decile_rank = as.numeric(IMD_decile_rank)) %>%
  filter(!is.na(IMD_decile_rank), !is.na(LSOA_CODE))

# Merge diabetes and IMD data
merged_data <- diabetes_data %>%
  left_join(imd_clean, by = "LSOA_CODE") %>%
  filter(!is.na(IMD_decile_rank))

cat("Data merged successfully. Total observations:", nrow(merged_data), "\n")
cat("Data coverage:", min(merged_data$date), "to", max(merged_data$date), "\n")
cat("Number of LSOAs:", n_distinct(merged_data$LSOA_CODE), "\n")

# ============================================================================
# Figure 1: Overall Diabetes Prescription Trends
# ============================================================================

time_start <- as.Date("2020-01-01")

# 1A: Annual prescription volumes by drug category
for_bars_annual <- merged_data %>%
  filter(month == 3, year %in% c(2020, 2021, 2022, 2023, 2024)) %>%
  select(year, Short.acting.insulins_items, Intermediate.and.long.acting.insulins_items, 
         Sulfonylureas_items, Biguanides_items, Other.antidiabetic.drugs_items) %>%
  group_by(year) %>%
  summarise(
    Short_Acting_Insulin = sum(Short.acting.insulins_items, na.rm = TRUE),
    Long_Acting_Insulin = sum(Intermediate.and.long.acting.insulins_items, na.rm = TRUE),
    Sulfonylureas = sum(Sulfonylureas_items, na.rm = TRUE),
    Metformin = sum(Biguanides_items, na.rm = TRUE),
    Other_Antidiabetic = sum(Other.antidiabetic.drugs_items, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Short_Acting_Insulin, Long_Acting_Insulin, Sulfonylureas, Metformin, Other_Antidiabetic), 
               names_to = "Drug_Category", values_to = "Prescriptions") %>%
  mutate(Drug_Category = gsub("_", " ", Drug_Category))

bars_annual_diabetes <- ggplot(data = for_bars_annual,
                               aes(x = as.factor(year), y = Prescriptions, fill = Drug_Category)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  labs(title = "A", x = "Year", y = "Number of Items") +
  theme(plot.title = element_text(size = 25),
        axis.text = element_text(size = 12),
        title = element_text(size = 12)) +
  scale_y_continuous(labels = scales::unit_format(suffix = "k", scale = 0.001, 
                                                  sep = "", big.mark = ",")) +
  guides(fill = guide_legend(title = "Diabetes medication subset")) +
  scale_fill_grey()

# 1B: Monthly time series
for_lines_diabetes <- merged_data %>%
  filter(date >= time_start) %>%
  select(date, Short.acting.insulins_items, Intermediate.and.long.acting.insulins_items, 
         Sulfonylureas_items, Biguanides_items, Other.antidiabetic.drugs_items, total_diabetes_items) %>%
  group_by(date) %>%
  summarise(
    short_insulin = sum(Short.acting.insulins_items, na.rm = TRUE),
    long_insulin = sum(Intermediate.and.long.acting.insulins_items, na.rm = TRUE),
    sulfonylureas = sum(Sulfonylureas_items, na.rm = TRUE),
    metformin = sum(Biguanides_items, na.rm = TRUE),
    other_diabetes = sum(Other.antidiabetic.drugs_items, na.rm = TRUE),
    all_diabetes = sum(total_diabetes_items, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(short_insulin, long_insulin, sulfonylureas, metformin, other_diabetes, all_diabetes),
               names_to = "category", values_to = "prescriptions") %>%
  mutate(chart = if_else(category == "all_diabetes", "Totals", "Subsets"),
         chart_label = TRUE,
         category = gsub("_", " ", category))

last_actual_date <- max(for_lines_diabetes$date)
label_data <- for_lines_diabetes %>%
  filter(date == last_actual_date, chart_label == TRUE) %>%
  mutate(date = date + 60)

lines_diabetes <- ggplot(for_lines_diabetes, aes(x = date, y = prescriptions, group = category)) +
  geom_line() +
  facet_wrap(~chart, scales = "free_y", ncol = 2) +
  labs(x = "Date", y = "Prescriptions written") +
  theme_minimal() +
  labs(title = "B") +
  theme(plot.title = element_text(size = 25)) +
  scale_y_continuous(labels = scales::unit_format(suffix = "k", scale = 0.001, 
                                                  sep = "", big.mark = ",")) +
  geom_vline(xintercept = as.Date("2022-07-01"), linetype = 4, color = "red") +
  theme(legend.position = "none") +
  geom_text(data = label_data,
            aes(x = date, y = prescriptions, label = category),
            size = 2.5, hjust = "left", vjust = 0.5) +
  scale_x_date(limits = c(min(for_lines_diabetes$date), max(for_lines_diabetes$date) + 150),
               expand = expansion(mult = c(0.02, 0.25))) +
  theme(
    plot.margin = margin(5.5, 60, 5.5, 5.5, "pt"),
    panel.spacing.x = unit(0.8, "cm"),
    strip.text = element_text(size = 12)
  )

# 1C: Percentage change around intervention
intervention_analysis <- merged_data %>%
  filter(date %in% c(as.Date("2022-06-01"), as.Date("2022-08-01"))) %>%
  select(date, Short.acting.insulins_items, Intermediate.and.long.acting.insulins_items, 
         Sulfonylureas_items, Biguanides_items, Other.antidiabetic.drugs_items, total_diabetes_items) %>%
  group_by(date) %>%
  summarise(
    short_insulin = sum(Short.acting.insulins_items, na.rm = TRUE),
    long_insulin = sum(Intermediate.and.long.acting.insulins_items, na.rm = TRUE),
    sulfonylureas = sum(Sulfonylureas_items, na.rm = TRUE),
    metformin = sum(Biguanides_items, na.rm = TRUE),
    other_diabetes = sum(Other.antidiabetic.drugs_items, na.rm = TRUE),
    all_diabetes = sum(total_diabetes_items, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(short_insulin, long_insulin, sulfonylureas, metformin, other_diabetes, all_diabetes),
               names_to = "category", values_to = "prescriptions")

for_bars_perc_diabetes <- intervention_analysis %>%
  group_by(category) %>%
  arrange(date) %>%
  summarise(percent_change = ((prescriptions[2] - prescriptions[1]) / prescriptions[1] * 100)) %>%
  mutate(chart = if_else(category == "all_diabetes", "Totals", "Subsets"),
         category = gsub("_", " ", category))

bars_perc_diabetes <- ggplot(data = for_bars_perc_diabetes, 
                             aes(x = category, y = percent_change)) +
  geom_bar(stat = "identity", width = 0.5) +
  facet_wrap(~chart, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(x = "Subset", y = "% change Jun to Aug 2022", 
       caption = "Percentage change in prescriptions around intervention") +
  labs(title = "C") +
  theme(plot.title = element_text(size = 25),
        plot.caption = element_text(size = 10, hjust = 0.5)) +
  scale_y_continuous(labels = scales::unit_format(suffix = "%")) +
  theme(
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 5)),
    axis.title.x = element_text(margin = margin(t = 15)),
    plot.margin = margin(5.5, 5.5, 25, 5.5, "pt")
  ) +
  theme(legend.position = "none")

figure_1_diabetes <- grid.arrange(bars_annual_diabetes, lines_diabetes, bars_perc_diabetes)
ggsave(paste0(output_dir_plots, "figure_1_diabetes.jpg"), 
       plot = figure_1_diabetes, dpi = 300, width = 8, height = 10, units = "in")

# ============================================================================
# Figure 2: Temporal Trends and Socioeconomic Inequalities
# ============================================================================

# Prepare data for ribbon plot
measures_rib_plot_diabetes <- merged_data %>%
  filter(date >= time_start) %>%
  select(date, LSOA_CODE, IDR, ATU, HMR, IMD_decile_rank) %>%
  pivot_longer(cols = c(IDR, ATU, HMR), names_to = "measure", values_to = "value") %>%
  filter(!is.na(value), !is.infinite(value)) %>%
  rbind(
    merged_data %>%
      filter(date >= time_start) %>%
      select(date, LSOA_CODE, IDR, ATU, HMR) %>%
      pivot_longer(cols = c(IDR, ATU, HMR), names_to = "measure", values_to = "value") %>%
      filter(!is.na(value), !is.infinite(value)) %>%
      mutate(IMD_decile_rank = "All")
  )

measures_rib_plot_diabetes_med <- measures_rib_plot_diabetes %>%
  group_by(date, measure, IMD_decile_rank) %>%
  summarise(decile_median = median(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    lab_var = IMD_decile_rank %in% c(1, 10),
    lab_text = case_when(
      IMD_decile_rank == 1 ~ " Most deprived",
      IMD_decile_rank == 10 ~ " Least deprived",
      IMD_decile_rank == "All" ~ " National Median",
      TRUE ~ ""
    )
  )

# Ribbon plot
figure_2_diabetes <- ggplot() +
  geom_lineribbon(
    data = measures_rib_plot_diabetes %>% 
      group_by(date, measure) %>%
      median_qi(value, .width = c(.5, .8, .95)),
    aes(x = date, y = value, ymin = .lower, ymax = .upper),
    linetype = "dashed", color = "white", linewidth = 0.8
  ) +
  geom_line(
    data = measures_rib_plot_diabetes_med %>% filter(IMD_decile_rank %in% c(1, 10)),
    aes(x = date, y = decile_median, color = as.factor(IMD_decile_rank), group = IMD_decile_rank),
    linewidth = 0.8
  ) +
  facet_wrap(~measure, scales = "free_y", ncol = 1) +
  geom_vline(xintercept = as.Date("2022-07-01"), linetype = 4, color = "red") +
  theme_minimal() +
  scale_fill_grey(start = 0.8, end = 0.2) +
  scale_color_manual(values = c("1" = "red", "10" = "green")) +
  labs(x = "Date", y = "Prescribing measure index (%)", 
       title = "") +
  theme(legend.position = "right") +
  guides(color = guide_legend(title = "IMD Decile", 
                              labels = c("1" = "Most deprived", "10" = "Least deprived"))) +
  scale_x_date(limits = c(min(measures_rib_plot_diabetes$date, na.rm = TRUE), 
                          max(measures_rib_plot_diabetes$date, na.rm = TRUE)),
               expand = expansion(mult = c(0.02, 0.02)))

ggsave(paste0(output_dir_plots, "figure_2_diabetes.jpg"), 
       plot = figure_2_diabetes, dpi = 300, width = 10, height = 8, units = "in")

# ============================================================================
# Figure 3: Differential Impact of ICS Implementation
# ============================================================================

# 3A: Pre/Post period comparison
pre_dates <- seq(as.Date("2021-04-01"), by = "month", length.out = 15)
post_dates <- seq(as.Date("2022-08-01"), by = "month", length.out = 15)

measures_pre_post_diabetes <- measures_rib_plot_diabetes %>%
  filter(date >= as.Date("2021-04-01"), date <= as.Date("2023-10-01")) %>%
  mutate(
    date_period = case_when(
      date %in% pre_dates ~ "pre",
      date == as.Date("2022-07-01") ~ "intervention",
      date %in% post_dates ~ "post",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(date_period))

measures_pre_post_median_diabetes <- measures_pre_post_diabetes %>%
  group_by(date_period, LSOA_CODE, measure, IMD_decile_rank) %>%
  summarise(period_avg = median(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(date_period, measure, IMD_decile_rank) %>%
  summarise(period_avg = median(period_avg, na.rm = TRUE), .groups = "drop")

for_line_IMD_diabetes <- measures_pre_post_median_diabetes %>%
  filter(IMD_decile_rank %in% c("1", "10", "All")) %>%
  mutate(
    IMD_decile_rank = case_when(
      IMD_decile_rank == "1" ~ "Most deprived",
      IMD_decile_rank == "10" ~ "Least deprived", 
      IMD_decile_rank == "All" ~ "National Median",
      TRUE ~ as.character(IMD_decile_rank)
    ),
    date_period = factor(date_period, levels = c("pre", "intervention", "post")),
    lab_var = (date_period == "post")
  )

measlines_prepost_diabetes <- ggplot(
  data = for_line_IMD_diabetes,
  aes(x = date_period, y = period_avg, group = IMD_decile_rank, 
      color = IMD_decile_rank, linetype = IMD_decile_rank)
) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  theme_minimal() +
  facet_wrap(~measure, scales = "free_y", ncol = 1) +
  theme(legend.position = "right") +
  labs(x = "Time Period", y = "Prescribing index (%)", title = "A") +
  theme(plot.title = element_text(size = 25)) +
  scale_color_manual(values = c("Most deprived" = "red", 
                                "Least deprived" = "green", 
                                "National Median" = "black")) +
  scale_linetype_manual(values = c("Most deprived" = "solid", 
                                   "Least deprived" = "solid", 
                                   "National Median" = "dashed")) +
  guides(color = guide_legend(title = "Deprivation Level", 
                              title.position = "top"),
         linetype = guide_legend(title = "Deprivation Level", 
                                 title.position = "top")) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

# 3B: Intervention effect size by deprivation level
perc_dif_lsoa_diabetes <- merged_data %>%
  filter(date %in% c(as.Date("2022-06-01"), as.Date("2022-08-01"))) %>%
  select(date, LSOA_CODE, IDR, ATU, HMR, IMD_decile_rank) %>%
  pivot_longer(cols = c(IDR, ATU, HMR), names_to = "measure", values_to = "value") %>%
  pivot_wider(id_cols = c("LSOA_CODE", "measure", "IMD_decile_rank"),
              names_from = date, names_prefix = "month_", values_from = value) %>%
  mutate(perc_dif = ((`month_2022-08-01` - `month_2022-06-01`) / `month_2022-06-01`) * 100) %>%
  filter(!is.na(perc_dif), !is.infinite(perc_dif))

perc_dif_decile_diabetes <- perc_dif_lsoa_diabetes %>%
  group_by(measure, IMD_decile_rank) %>%
  summarise(perc_dif_median = median(perc_dif, na.rm = TRUE), .groups = "drop") %>%
  rbind(
    perc_dif_lsoa_diabetes %>%
      group_by(measure) %>%
      summarise(perc_dif_median = median(perc_dif, na.rm = TRUE), .groups = "drop") %>%
      mutate(IMD_decile_rank = "All")
  )

perc_dif_decile_bars_diabetes <- perc_dif_decile_diabetes %>%
  filter(IMD_decile_rank %in% c("1", "10", "All")) %>%
  mutate(
    IMD_decile_rank = case_when(
      IMD_decile_rank == "1" ~ "Highest dep.",
      IMD_decile_rank == "10" ~ "Lowest dep.",
      IMD_decile_rank == "All" ~ "National",
      TRUE ~ as.character(IMD_decile_rank)
    ),
    IMD_decile_rank = factor(IMD_decile_rank, 
                             levels = c("Lowest dep.", "National", "Highest dep."))
  )

bars_IMD_diabetes <- ggplot(perc_dif_decile_bars_diabetes, 
                            aes(x = IMD_decile_rank, y = perc_dif_median, 
                                fill = ifelse(perc_dif_median >= 0, "Increase", "Decrease"))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  facet_wrap(~measure, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(x = "IMD decile rank", y = "Median % change around intervention", title = "B",
       caption = "Positive values = increase; Negative values = decrease") +
  theme(plot.title = element_text(size = 25),
        axis.text = element_text(size = 10),
        title = element_text(size = 10),
        axis.text.x = element_text(angle = 35, hjust = 0.75),
        plot.caption = element_text(size = 9, hjust = 0.5)) +
  scale_fill_manual(values = c("Increase" = "steelblue", "Decrease" = "orange"), 
                    name = "Direction") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)

figure_3_diabetes <- grid.arrange(measlines_prepost_diabetes, bars_IMD_diabetes,
                                  layout_matrix = rbind(c(1, 1, 1, 2, 2),
                                                        c(1, 1, 1, 2, 2),
                                                        c(1, 1, 1, 2, 2),
                                                        c(1, 1, 1, 2, 2)))

ggsave(paste0(output_dir_plots, "figure_3_diabetes.jpg"), 
       plot = figure_3_diabetes, dpi = 300, width = 12, height = 10, units = "in")

# ============================================================================
# Summary
# ============================================================================

cat("\nAnalysis completed successfully!\n")
cat("Generated files:\n")
cat("- figure_1_diabetes.jpg: Overall prescription trends\n")
cat("- figure_2_diabetes.jpg: Temporal trends and socioeconomic inequalities\n") 
cat("- figure_3_diabetes.jpg: Differential impact of ICS implementation\n")
cat("\nOutput directory:", output_dir_plots, "\n")
# CAPM Volatility-Regime Analysis
# Thesis: Testing the Explanatory Power of CAPM under High- and Low-Volatility Market Conditions
library(tidyverse)
library(zoo)
library(broom)
library(writexl)

# File paths
industry_path <- "raw data/10_Industry_Portfolios.csv"
factors_path <- "raw data/F-F_Research_Data_Factors.csv"

# Read raw lines to inspect structure
industry_lines <- readLines(industry_path, warn = FALSE)
factor_lines <- readLines(factors_path, warn = FALSE)

cat("First 20 lines of industry file:\n")
cat(industry_lines[1:20], sep = "\n")

cat("\n\nFirst 20 lines of factor file:\n")
cat(factor_lines[1:20], sep = "\n")


# -----------------------------
# 1. Clean 10 Industry Portfolio data
# -----------------------------

# Find the line where monthly industry returns begin
industry_start <- which(str_detect(industry_lines, "^,NoDur"))[1]

# Read industry data from that line onward
industry_raw <- read_csv(
  industry_path,
  skip = industry_start - 1,
  show_col_types = FALSE
)

# Rename first column to Date
industry_raw <- industry_raw %>%
  rename(Date = 1)

# Keep only rows where Date is numeric YYYYMM
industry_clean <- industry_raw %>%
  filter(str_detect(as.character(Date), "^[0-9]{6}$")) %>%
  mutate(Date = as.integer(Date))

# -----------------------------
# 2. Clean Fama-French Factors data
# -----------------------------

# Find the line where factor returns begin
factor_start <- which(str_detect(factor_lines, "^,Mkt-RF"))[1]

# Read factor data from that line onward
factors_raw <- read_csv(
  factors_path,
  skip = factor_start - 1,
  show_col_types = FALSE
)

# Rename first column to Date
factors_raw <- factors_raw %>%
  rename(Date = 1)

# Keep only rows where Date is numeric YYYYMM
factors_clean <- factors_raw %>%
  filter(str_detect(as.character(Date), "^[0-9]{6}$")) %>%
  mutate(Date = as.integer(Date))

# -----------------------------
# 3. Check cleaned data
# -----------------------------

cat("Industry data dimensions:\n")
print(dim(industry_clean))

cat("\nFactor data dimensions:\n")
print(dim(factors_clean))

cat("\nFirst rows of industry data:\n")
print(head(industry_clean))

cat("\nFirst rows of factor data:\n")
print(head(factors_clean))









# -----------------------------
# 1. Clean 10 Industry Portfolio data properly
# -----------------------------

# Find monthly value-weighted section
industry_start <- which(str_detect(industry_lines, "^,NoDur"))[1]

# Find the first empty line after the monthly table starts
industry_end <- industry_start + which(industry_lines[(industry_start + 1):length(industry_lines)] == "")[1] - 1

# Read only the monthly value-weighted table
industry_clean <- read_csv(
  paste(industry_lines[industry_start:(industry_end - 1)], collapse = "\n"),
  show_col_types = FALSE
) %>%
  rename(Date = 1) %>%
  filter(str_detect(as.character(Date), "^[0-9]{6}$")) %>%
  mutate(Date = as.integer(Date))

# -----------------------------
# 2. Clean Fama-French Factors data properly
# -----------------------------

factor_start <- which(str_detect(factor_lines, "^,Mkt-RF"))[1]

factor_end <- factor_start + which(factor_lines[(factor_start + 1):length(factor_lines)] == "")[1] - 1

factors_clean <- read_csv(
  paste(factor_lines[factor_start:(factor_end - 1)], collapse = "\n"),
  show_col_types = FALSE
) %>%
  rename(Date = 1) %>%
  filter(str_detect(as.character(Date), "^[0-9]{6}$")) %>%
  mutate(Date = as.integer(Date))

# -----------------------------
# 3. Check cleaned data
# -----------------------------

cat("Industry data dimensions:\n")
print(dim(industry_clean))

cat("\nFactor data dimensions:\n")
print(dim(factors_clean))

cat("\nLast rows of industry data:\n")
print(tail(industry_clean))

cat("\nLast rows of factor data:\n")
print(tail(factors_clean))





# -----------------------------
# 4. Merge industry returns with factor data
# -----------------------------

merged_data <- industry_clean %>%
  inner_join(factors_clean, by = "Date") %>%
  rename(
    Mkt_RF = `Mkt-RF`
  )

# -----------------------------
# 5. Keep thesis sample period: July 1963 - December 2024
# -----------------------------

sample_data <- merged_data %>%
  filter(Date >= 196307, Date <= 202412)

# -----------------------------
# 6. Convert returns from percent to decimals
# Example: 2.50 becomes 0.025
# -----------------------------

return_columns <- c(
  "NoDur", "Durbl", "Manuf", "Enrgy", "HiTec",
  "Telcm", "Shops", "Hlth", "Utils", "Other",
  "Mkt_RF", "SMB", "HML", "RF"
)

sample_data <- sample_data %>%
  mutate(across(all_of(return_columns), ~ .x / 100))

# -----------------------------
# 7. Create excess returns for each industry portfolio
# Industry excess return = Industry return - RF
# -----------------------------

industry_names <- c(
  "NoDur", "Durbl", "Manuf", "Enrgy", "HiTec",
  "Telcm", "Shops", "Hlth", "Utils", "Other"
)

capm_data_wide <- sample_data %>%
  mutate(across(
    all_of(industry_names),
    ~ .x - RF,
    .names = "{.col}_excess"
  ))

# -----------------------------
# 8. Check the dataset
# -----------------------------

cat("Merged sample data dimensions:\n")
print(dim(capm_data_wide))

cat("\nDate range:\n")
print(range(capm_data_wide$Date))

cat("\nFirst rows of CAPM dataset:\n")
print(head(capm_data_wide))

cat("\nLast rows of CAPM dataset:\n")
print(tail(capm_data_wide))


# -----------------------------
# 9. Create 12-month rolling market volatility
# -----------------------------

capm_data_wide <- capm_data_wide %>%
  arrange(Date) %>%
  mutate(
    rolling_vol_12m = rollapply(
      data = Mkt_RF,
      width = 12,
      FUN = sd,
      align = "right",
      fill = NA
    )
  )

# -----------------------------
# 10. Remove months without rolling volatility
# First 11 months are lost because 12 months are needed
# -----------------------------

capm_data_wide <- capm_data_wide %>%
  filter(!is.na(rolling_vol_12m))

# -----------------------------
# 11. Median split into low- and high-volatility regimes
# -----------------------------

median_vol <- median(capm_data_wide$rolling_vol_12m, na.rm = TRUE)

capm_data_wide <- capm_data_wide %>%
  mutate(
    volatility_regime = if_else(
      rolling_vol_12m > median_vol,
      "High volatility",
      "Low volatility"
    )
  )

# -----------------------------
# 12. Check regime split
# -----------------------------

cat("Median 12-month rolling volatility:\n")
print(median_vol)

cat("\nNumber of months by volatility regime:\n")
print(table(capm_data_wide$volatility_regime))

cat("\nDate range after rolling volatility calculation:\n")
print(range(capm_data_wide$Date))

cat("\nFirst rows with volatility regime:\n")
print(
  capm_data_wide %>%
    select(Date, Mkt_RF, rolling_vol_12m, volatility_regime) %>%
    head(15)
)

cat("\nHighest volatility months:\n")
print(
  capm_data_wide %>%
    select(Date, Mkt_RF, rolling_vol_12m, volatility_regime) %>%
    arrange(desc(rolling_vol_12m)) %>%
    head(15)
)




# -----------------------------
# 13. Convert data from wide format to long format
# -----------------------------

capm_data_long <- capm_data_wide %>%
  select(
    Date,
    Mkt_RF,
    RF,
    rolling_vol_12m,
    volatility_regime,
    ends_with("_excess")
  ) %>%
  pivot_longer(
    cols = ends_with("_excess"),
    names_to = "Industry",
    values_to = "Industry_Excess_Return"
  ) %>%
  mutate(
    Industry = str_replace(Industry, "_excess", "")
  )

cat("Long CAPM dataset dimensions:\n")
print(dim(capm_data_long))

cat("\nFirst rows of long CAPM dataset:\n")
print(head(capm_data_long))


# -----------------------------
# 14. Function to run one CAPM regression
# -----------------------------

run_capm <- function(data, regime_label) {
  
  model <- lm(Industry_Excess_Return ~ Mkt_RF, data = data)
  
  tidy_model <- tidy(model)
  glance_model <- glance(model)
  
  alpha_row <- tidy_model %>% filter(term == "(Intercept)")
  beta_row <- tidy_model %>% filter(term == "Mkt_RF")
  
  tibble(
    Regime = regime_label,
    Alpha = alpha_row$estimate,
    Alpha_p_value = alpha_row$p.value,
    Beta = beta_row$estimate,
    Beta_p_value = beta_row$p.value,
    R_squared = glance_model$r.squared,
    Adjusted_R_squared = glance_model$adj.r.squared,
    Observations = glance_model$nobs
  )
}


# -----------------------------
# 15. Run full-sample, low-volatility, and high-volatility CAPM regressions
# -----------------------------

full_results <- capm_data_long %>%
  group_by(Industry) %>%
  group_modify(~ run_capm(.x, "Full sample")) %>%
  ungroup()

low_results <- capm_data_long %>%
  filter(volatility_regime == "Low volatility") %>%
  group_by(Industry) %>%
  group_modify(~ run_capm(.x, "Low volatility")) %>%
  ungroup()

high_results <- capm_data_long %>%
  filter(volatility_regime == "High volatility") %>%
  group_by(Industry) %>%
  group_modify(~ run_capm(.x, "High volatility")) %>%
  ungroup()

capm_results <- bind_rows(
  full_results,
  low_results,
  high_results
) %>%
  arrange(Industry, factor(Regime, levels = c("Full sample", "Low volatility", "High volatility")))


# -----------------------------
# 16. Check regression results
# -----------------------------

cat("\nCAPM regression results:\n")
print(capm_results, n = 30)




# -----------------------------
# 17. Print full regression results clearly
# -----------------------------

capm_results_print <- capm_results %>%
  mutate(
    Alpha = round(Alpha, 5),
    Alpha_p_value = round(Alpha_p_value, 4),
    Beta = round(Beta, 4),
    Beta_p_value = round(Beta_p_value, 4),
    R_squared = round(R_squared, 4),
    Adjusted_R_squared = round(Adjusted_R_squared, 4)
  )

print(capm_results_print, n = 30, width = Inf)

# -----------------------------
# 18. Export regression results to Excel
# -----------------------------

write_xlsx(
  capm_results_print,
  path = "results/capm_regression_results.xlsx"
)

# Also export the clean long dataset
write_csv(
  capm_data_long,
  file = "clean data/capm_clean_long_dataset.csv"
)

cat("Files exported successfully.\n")



# -----------------------------
# 19. Create comparison table: Low vs High volatility
# -----------------------------

comparison_table <- capm_results %>%
  filter(Regime %in% c("Low volatility", "High volatility")) %>%
  select(
    Industry,
    Regime,
    Alpha,
    Alpha_p_value,
    Beta,
    Adjusted_R_squared,
    Observations
  ) %>%
  pivot_wider(
    names_from = Regime,
    values_from = c(Alpha, Alpha_p_value, Beta, Adjusted_R_squared, Observations)
  ) %>%
  mutate(
    Beta_Difference_High_minus_Low =
      `Beta_High volatility` - `Beta_Low volatility`,
    Adj_R2_Difference_High_minus_Low =
      `Adjusted_R_squared_High volatility` - `Adjusted_R_squared_Low volatility`,
    Alpha_Difference_High_minus_Low =
      `Alpha_High volatility` - `Alpha_Low volatility`
  ) %>%
  arrange(desc(Adj_R2_Difference_High_minus_Low))

comparison_table_print <- comparison_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

print(comparison_table_print, n = 10, width = Inf)

# Export comparison table
write_xlsx(
  comparison_table_print,
  path = "results/capm_low_high_comparison.xlsx"
)

cat("Comparison table exported successfully.\n")




# -----------------------------
# 20. Create overall summary statistics
# -----------------------------

summary_table <- tibble(
  Metric = c(
    "Average adjusted R-squared - Low volatility",
    "Average adjusted R-squared - High volatility",
    "Average adjusted R-squared difference",
    "Number of industries with higher adjusted R-squared in high volatility",
    "Average beta - Low volatility",
    "Average beta - High volatility",
    "Average beta difference",
    "Number of industries with higher beta in high volatility"
  ),
  Value = c(
    mean(comparison_table$`Adjusted_R_squared_Low volatility`),
    mean(comparison_table$`Adjusted_R_squared_High volatility`),
    mean(comparison_table$Adj_R2_Difference_High_minus_Low),
    sum(comparison_table$Adj_R2_Difference_High_minus_Low > 0),
    mean(comparison_table$`Beta_Low volatility`),
    mean(comparison_table$`Beta_High volatility`),
    mean(comparison_table$Beta_Difference_High_minus_Low),
    sum(comparison_table$Beta_Difference_High_minus_Low > 0)
  )
) %>%
  mutate(Value = round(Value, 4))

print(summary_table, width = Inf)

write_xlsx(
  summary_table,
  path = "results/capm_summary_statistics.xlsx"
)

cat("Summary statistics exported successfully.\n")




# -----------------------------
# 21. Professional figures
# -----------------------------

library(ggplot2)
library(scales)

# Create folder for figures
if (!dir.exists("results/figures")) {
  dir.create("results/figures")
}

# Convert YYYYMM numeric date into actual date format
capm_data_wide <- capm_data_wide %>%
  mutate(
    Date_plot = as.Date(paste0(substr(Date, 1, 4), "-", substr(Date, 5, 6), "-01"))
  )

# Professional thesis theme
theme_thesis <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 17, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 12)),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10, color = "gray25"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray88"),
      panel.grid.major.y = element_line(color = "gray88"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Aesthetic color palette
regime_colors <- c(
  "Low volatility" = "#2E86AB",
  "High volatility" = "#F18F01"
)

single_line_color <- "#263238"
accent_color <- "#D1495B"





figure_1_market_return <- capm_data_wide %>%
  ggplot(aes(x = Date_plot, y = Mkt_RF)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_line(color = single_line_color, linewidth = 0.45) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Monthly Market Excess Return",
    subtitle = "Fama-French Mkt-RF, June 1964–December 2024",
    x = "Year",
    y = "Market excess return"
  ) +
  theme_thesis()

ggsave(
  "results/figures/figure_1_market_excess_return_professional.png",
  figure_1_market_return,
  width = 9,
  height = 5,
  dpi = 300
)




figure_2_rolling_volatility <- capm_data_wide %>%
  ggplot(aes(x = Date_plot, y = rolling_vol_12m)) +
  geom_line(color = single_line_color, linewidth = 0.6) +
  geom_hline(
    yintercept = median_vol,
    color = accent_color,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "12-Month Rolling Market Volatility",
    subtitle = "Dashed line shows the median cutoff used to classify low- and high-volatility months",
    x = "Year",
    y = "Rolling volatility"
  ) +
  theme_thesis()

ggsave(
  "results/figures/figure_2_rolling_volatility_professional.png",
  figure_2_rolling_volatility,
  width = 9,
  height = 5,
  dpi = 300
)




adj_r2_plot_data <- comparison_table %>%
  select(
    Industry,
    `Adjusted_R_squared_Low volatility`,
    `Adjusted_R_squared_High volatility`
  ) %>%
  pivot_longer(
    cols = starts_with("Adjusted_R_squared"),
    names_to = "Regime",
    values_to = "Adjusted_R_squared"
  ) %>%
  mutate(
    Regime = case_when(
      str_detect(Regime, "Low") ~ "Low volatility",
      str_detect(Regime, "High") ~ "High volatility"
    ),
    Industry = factor(Industry, levels = comparison_table$Industry)
  )

figure_3_adj_r2 <- adj_r2_plot_data %>%
  ggplot(aes(x = Industry, y = Adjusted_R_squared, fill = Regime)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_manual(values = regime_colors) +
  scale_y_continuous(labels = number_format(accuracy = 0.01), limits = c(0, 1)) +
  labs(
    title = "CAPM Explanatory Power by Volatility Regime",
    subtitle = "Adjusted R-squared is higher in high-volatility periods for all ten industry portfolios",
    x = "Industry portfolio",
    y = "Adjusted R-squared",
    fill = "Regime"
  ) +
  theme_thesis()

ggsave(
  "results/figures/figure_3_adjusted_r2_comparison_professional.png",
  figure_3_adj_r2,
  width = 9,
  height = 5,
  dpi = 300
)
beta_plot_data <- comparison_table %>%
  select(
    Industry,
    `Beta_Low volatility`,
    `Beta_High volatility`
  ) %>%
  pivot_longer(
    cols = starts_with("Beta"),
    names_to = "Regime",
    values_to = "Beta"
  ) %>%
  mutate(
    Regime = case_when(
      str_detect(Regime, "Low") ~ "Low volatility",
      str_detect(Regime, "High") ~ "High volatility"
    ),
    Industry = factor(Industry, levels = comparison_table$Industry)
  )

figure_4_beta <- beta_plot_data %>%
  ggplot(aes(x = Industry, y = Beta, fill = Regime)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_manual(values = regime_colors) +
  labs(
    title = "CAPM Beta by Volatility Regime",
    subtitle = "Beta changes across regimes, but the direction of change differs by industry",
    x = "Industry portfolio",
    y = "Beta",
    fill = "Regime"
  ) +
  theme_thesis()

ggsave(
  "results/figures/figure_4_beta_comparison_professional.png",
  figure_4_beta,
  width = 9,
  height = 5,
  dpi = 300
)

figure_5_adj_r2_difference <- comparison_table %>%
  mutate(
    Industry = reorder(Industry, Adj_R2_Difference_High_minus_Low)
  ) %>%
  ggplot(aes(x = Industry, y = Adj_R2_Difference_High_minus_Low)) +
  geom_col(fill = "#3A7D44", width = 0.65) +
  coord_flip() +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Difference in CAPM Explanatory Power",
    subtitle = "High-volatility adjusted R-squared minus low-volatility adjusted R-squared",
    x = "Industry portfolio",
    y = "Adjusted R-squared difference"
  ) +
  theme_thesis()

ggsave(
  "results/figures/figure_5_adjusted_r2_difference_professional.png",
  figure_5_adj_r2_difference,
  width = 9,
  height = 5,
  dpi = 300
)

cat("Professional figures exported successfully.\n")





# -----------------------------
# 26. Robustness check: interaction regression for beta differences
# -----------------------------

interaction_results <- capm_data_long %>%
  mutate(
    High_Dummy = if_else(volatility_regime == "High volatility", 1, 0),
    Mkt_RF_High = Mkt_RF * High_Dummy
  ) %>%
  group_by(Industry) %>%
  group_modify(~ {
    
    model <- lm(
      Industry_Excess_Return ~ High_Dummy + Mkt_RF + Mkt_RF_High,
      data = .x
    )
    
    tidy_model <- tidy(model)
    glance_model <- glance(model)
    
    alpha_base <- tidy_model %>% filter(term == "(Intercept)")
    alpha_high_diff <- tidy_model %>% filter(term == "High_Dummy")
    beta_low <- tidy_model %>% filter(term == "Mkt_RF")
    beta_high_diff <- tidy_model %>% filter(term == "Mkt_RF_High")
    
    tibble(
      Alpha_Low = alpha_base$estimate,
      Alpha_Difference_High_minus_Low = alpha_high_diff$estimate,
      Alpha_Difference_p_value = alpha_high_diff$p.value,
      Beta_Low = beta_low$estimate,
      Beta_Difference_High_minus_Low = beta_high_diff$estimate,
      Beta_Difference_p_value = beta_high_diff$p.value,
      Beta_High = beta_low$estimate + beta_high_diff$estimate,
      Adjusted_R_squared = glance_model$adj.r.squared,
      Observations = glance_model$nobs
    )
  }) %>%
  ungroup()

interaction_results_print <- interaction_results %>%
  mutate(
    Alpha_Low = round(Alpha_Low * 100, 3),
    Alpha_Difference_High_minus_Low = round(Alpha_Difference_High_minus_Low * 100, 3),
    Alpha_Difference_p_value = round(Alpha_Difference_p_value, 4),
    Beta_Low = round(Beta_Low, 3),
    Beta_Difference_High_minus_Low = round(Beta_Difference_High_minus_Low, 3),
    Beta_Difference_p_value = round(Beta_Difference_p_value, 4),
    Beta_High = round(Beta_High, 3),
    Adjusted_R_squared = round(Adjusted_R_squared, 3)
  )

print(interaction_results_print, n = 10, width = Inf)

write_xlsx(
  interaction_results_print,
  path = "results/capm_interaction_regression_results.xlsx"
)

cat("Interaction regression robustness check exported successfully.\n")
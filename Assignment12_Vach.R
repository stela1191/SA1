rm(list = ls())
library(rio)
library(tidyverse)
library(psych)
library(emmeans)
library(multcomp)  # for TukeyHSD plot enhancements
library(gridExtra)  # for arranging multiple plots in one PDF

# Load the ESS dataset
data <- import("~/_FSEV_EŠ/1_YEAR_2_SEMESTER/SA1_statistical_analysis_1/scripts/w10_stat_analysis/ESS10.sav")
unique(data$cntry)
# List of countries by region based on the instructions
eastern_european_codes <- c("BG", "CZ", "EE", "HR", "HU", "LT", "ME", "MK", "PL", "RO", "SI", "SK")
western_eu_codes <- c("AT", "BE", "DE", "DK", "FI", "FR", "IE", "IT", "LU", "NL", "SE", "UK")
western_non_eu_codes <- c("CH", "IS", "NO")

# Add a new column for region classification
data_subset <- data %>%
  rename(country = cntry) %>% 
  filter(!is.na(happy)) %>% 
  mutate(country_rec = case_when(
    country %in% eastern_european_codes ~ "Post_communist",
    country %in% western_eu_codes ~ "Western_EU",
    country %in% western_non_eu_codes ~ "Western_non_EU",
    TRUE ~ NA_character_
  ))

# Filter out rows with NA regions
data_subset <- data_subset %>% filter(!is.na(country_rec))

# Run the ANOVA model
anova <- aov(happy ~ country_rec, data = data_subset)
anova_summary <- summary(anova)

# Tukey HSD test
tukey <- TukeyHSD(anova)
tukey_df <- as.data.frame(tukey$country_rec)
tukey_df$comparison <- rownames(tukey_df)

# Convert Tukey HSD results to data frame for ggplot
tukey_plot <- ggplot(tukey_df, aes(x = comparison, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    y = "Differences in Mean Levels of Happiness",
    x = "Region Pairs",
    title = "Tukey HSD Test for Happiness by Region"
  ) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Estimated Marginal Means
emmeans <- emmeans(anova, ~ country_rec)
emmeans_df <- as.data.frame(emmeans)

# Create the plot for estimated marginal means
emmeans_plot <- ggplot(data = emmeans_df, aes(x = country_rec, y = emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = .5) +
  theme_minimal() +
  labs(
    y = "EMM, 95% CL",
    x = "Region",
    title = "Estimated Marginal Means of Happiness by Region"
  ) +
  coord_flip()

# Combine the plots into one PDF
pdf("Happiness_Analysis_Results.pdf")
grid.arrange(tukey_plot, emmeans_plot, ncol = 1)
dev.off()

# Print the ANOVA summary for interpretation
print(anova_summary)
rm(list = ls())
library(rio)
library(tidyverse)
library(psych)
library(emmeans)

data <- import("~/_FSEV_EŠ/1_YEAR_2_SEMESTER/SA1_statistical_analysis_1/scripts/w10_stat_analysis/ESS10.sav")
unique(data$cntry)
data_subset <- data %>%
  rename(country = cntry) %>% 
  filter(!is.na(happy)) %>% 
  mutate(country_rec = case_when(
    country %in% c("BG", "CZ", "EE", "HR", 
                   "HU", "LT", "ME", "MK", "PL", "RO", "SI", "SK") ~ "Post_communist",
    country %in% c("AT", "BE", "DE", "DK", 
                   "FI", "FR", "IE", "IT", "LU", "NL", "SE", "UK") ~ "Western EU",
    TRUE ~ "Western_non_EU"
  ))

unique(data_subset$country_rec)
unique(data_subset$happy)

anova <- aov(happy ~ country_rec, data = data_subset)
summary(anova)

tukey <- TukeyHSD(anova)
plot(tukey)
emmeans <- emmeans(anova, ~ country_rec)
emmeans_df <- as.data.frame(emmeans)

ggplot(data = emmeans_df, aes(country_rec, y = emmean))+
  geom_point()+
  geom_errorbar(aes(ymin= lower.CL,
                    ymax = upper.CL),
                width = .5)+
  theme_minimal()+
  labs(
    y = "EMM, 95% CL",
  )+
  coord_flip()

export()

library(swirl)
library(dplyr)
library(rio)

swirl()
## done

#
setwd("~/_FSEV_EŠ/1_YEAR_2_SEMESTER/SA1_statistical_analysis_1/scripts/w3")
#
rm(list = ls())
#
voting <- read.csv("voting.csv")
#
dim(voting)
View(voting)
#
voting$voted_recorded <- ifelse(voting$voted == 0, "no", "yes")
#
table(voting$voted)
prop.table(table(voting$voted))
#
round(mean(voting$voted), 1) #after rounding, it is clear that merely around 30% of subjects in the data set chose to vote
#
#summarise(voting, voted, .by = message)
#
voting_summary <- voting %>%
  group_by(message, voted) %>%
  summarise(count = n(), .groups = "drop")

print(voting_summary) #didnt message and didnt vote - 134513 occasions, didnt message and voted - 56730 occasions, messaged but didnt vote - 23763 occasions, messaged and voted - 14438 occasions
# 
voting$messages_num <- ifelse(voting$message == "no", 0, 1)
message_voted <- mean(voting$voted[voting$messages_num == 1])
message_not_voted <- mean(voting$voted[voting$messages_num == 0])
DiM_message <- message_voted - message_not_voted
DiM_message #the difference of means suggests that people who got a message were by around 8 p.p. more likely to vote
#
treatment <- c("message","control")
turnout_rates <- c(round(mean(voting$voted[voting$messages_num == 1]), 2), round(mean(voting$voted[voting$messages_num == 0]), 2))
voting_table <- data.frame(treatment, turnout_rates)
voting_table
#
DiM_message <- round(message_voted - message_not_voted, 2)
voting_table <- rbind(voting_table, data.frame(treatment = "DiM", turnout_rates = DiM_message))
voting_table
#
export(voting_table, "~/_FSEV_EŠ/1_YEAR_2_SEMESTER/SA1_statistical_analysis_1/scripts/w3/vot_table.xlsx")
#
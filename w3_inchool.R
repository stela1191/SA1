library("rio")
setwd("~/_FSEV_EŠ/1_YEAR_2_SEMESTER/SA1_statistical_analysis_1/scripts/w3")
star <- import("STAR.csv")

head(star, n=6)
View(star)

star$small <- ifelse(star$classtype == "small", 1, 0)

#takto zistim kolko dat chyba
sum(is.na(star))
#tymto vyzazem chybajuce data
star <- na.omit(star)

mean(star$math)
median(star$math)
sd(star$math)

grad <- 0
nongrad <- 0

# NEFUNKCNY LOOP
#for (i in 1:1274) {
#  if (star$graduated[i] == 1) {
#    grad <- grad + 1
#  } else {
#    nongrad <- nongrad + 1
#  }
#}

graduated_count <- sum(star$graduated == 1)
total_students <- length(star$graduated)
proportion_graduated <- graduated_count / total_students

graduated_count
proportion_graduated

# # Suppose we have a vector of colors
# colors <- c("red", "blue", "red", "green", "blue", "blue")
# 
# # Use table() to count occurrences of each color
# color_counts <- table(colors)
# 
# # Print the counts
# print(color_counts)

# kolko_kde <- sum(table(star))
# print(kolko_kde)

hist(star$math)
boxplot(star$math)

# NOVA KNIZNICA
install.packages("dplyr")
library("dplyr")
install.packages("plotly")

glimpse(star)
table(star$classtype)
prop.table(table(star$classtype))
#
write.csv(star, file = "STAR1.csv", row.names = FALSE)
#
t_test_result <- t.test(star$math ~ star$small)
print(t_test_result)

grad_small <- mean(star$graduated[star$small == 1])
grad_reg <- mean(star$graduated[star$small == 0])

DiM_grad <- grad_small - grad_reg
print(DiM_grad)

round(DiM_grad*100, 2)
#jednoduchsi sposob ako najst priemery
table(star$classtype)
summarize(star, mean(reading), .by = classtype)

#pomaly vytvorim tabulku s priemermi z matiky
treatment <- c("Treated", "Control")
test_score <- c(mean(star$math[star$small == 1]), mean(star$math[star$small == 0]))
treatment
test_score

score_table <- data.frame(treatment, test_score)
View(score_table)

export(score_table, "score_table.csv")

library("swirl")
swirl()





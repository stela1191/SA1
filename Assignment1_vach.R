setwd("~/_FSEV_EŠ/1_YEAR_2_SEMESTER/SA1_statistical_analysis_1/scripts/w2")
library(rio)
voting <- import("voting.csv")
#
head(voting, n=10)
#
names(voting)
#
class(voting$birth)
unique(voting$birth) #numeric non-binary

class(voting$message) #character

class(voting$voted)
unique(voting$voted) #numeric binary
#
nrow(voting) # n riadkov = 229444
ncol(voting) # n stlpcov = 3
#
mean_age <- mean(voting$birth)
mean_age # priemerny rok narodenia respondenta je 1956.18
#
min(voting$birth) # najnizsi rok narodenia je 1900
max(voting$birth) #najvyssi rok narodenia je 1986
#
install.packages("psych")

# This toolbox is designed for psychometric analysis and experimental 
# psychology, offering multivariate analysis, scale construction, and 
# reliability analysis. It supports Item Response Theory, multilevel 
# statistical functions, and structural equation modeling, along with 
# validation and graphical displays of models. Additionally, it's used 
# for educational purposes in psychometric theory and personality research.
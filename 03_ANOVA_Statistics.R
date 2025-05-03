
###############################################################
# REQUIRED LIBRARIES
###############################################################
# install.packages("agricolae")
# install.packages("car")
# install.packages("multcomp")
# install.packages("lsmeans")
# install.packages("emmeans")
# install.packages("DescTools")

library(agricolae)
library(car)
library(multcomp)
library(lsmeans)
library(emmeans)
library(DescTools)

###############################################################
# ONE-WAY ANOVA
###############################################################
X <- c(4.7, 4.9, 5, 4.8, 4.7)
Y <- c(4.6, 4.4, 4.3, 4.4, 4.1, 4.2)
Z <- c(4.8, 4.7, 4.6, 4.4, 4.7, 4.8)
W <- c(4.9, 5.2, 5.4, 5.1, 5.6)
anova_result <- aov(c(X, Y, Z, W) ~ rep(c("X", "Y", "Z", "W"), c(5, 6, 6, 5)))
summary(anova_result)

###############################################################
# ANOVA WITH EXTERNAL DATASET (Diets)
###############################################################
# diets <- read.csv("diets.csv", header = TRUE)
# anova1 <- aov(weight_gain ~ Diets, data = diets)
# summary(anova1)
# leveneTest(weight_gain ~ Diets, data = diets, center = "median")
# bartlett.test(weight_gain ~ Diets, data = diets)
# TukeyHSD(anova1, "Diets")
# scheffe.test(anova1, "Diets")
# duncan.test(anova1, "Diets", alpha = 0.05)

###############################################################
# TWO-WAY ANOVA
###############################################################
Sex <- rep(c("Males", "Females"), each = 12)
Species <- rep(c("Sp1", "Sp2", "Sp3"), times = 8)
Measurement <- c(21.5, 14.5, 16, 19.6, 17.4, 20.3, 20.9, 15, 18.5, 22.8, 17.8, 19.3,
                 14.8, 12.1, 14.4, 15.6, 11.4, 14.7, 13.5, 12.7, 13.8, 16.4, 14.5, 12)
data_two_way <- data.frame(Sex, Species, Measurement)
anova_result_two_way <- aov(Measurement ~ Sex * Species, data = data_two_way)
summary(anova_result_two_way)

###############################################################
# BLOCK DESIGN ANOVA
###############################################################
data_block <- data.frame(
  Person = rep(c("A", "B", "C", "D", "E", "F"), each = 4),
  Weight = rep(c("No weight", "1 kg", "2 kg", "5 kg"), times = 6),
  Joint_Range = c(180, 178, 130, 81, 171, 170, 99, 65, 180, 180, 105, 72,
                  179, 120, 102, 75, 177, 180, 110, 83, 179, 175, 170, 98)
)
model <- aov(Joint_Range ~ Weight + Error(Person), data = data_block)
summary(model)
DunnettTest(x = data_block$Joint_Range, g = data_block$Weight)

###############################################################
# ANCOVA
###############################################################
data_cov <- data.frame(
  BMI = c(32, 30, 31, 32, 31, 30, 30, 32, 33, 31, 33, 32, 34, 35, 33, 34, 32, 33, 34, 35),
  Weight = c(75, 79, 75, 72, 76, 80, 83, 79, 81, 78, 75, 70, 73, 72, 71, 73, 76, 78, 72, 74),
  Dependent_Variable = c(1.3, 2, 0.9, 2.5, 1.4, 0.9, 1.2, 0.8, 1, 0.9, 2, 1.5, 1.3, 1.9,
                         1.4, 1.2, 1.6, 1.5, 0.9, 1),
  Sex = rep(c("F", "M"), each = 10),
  Treatment = rep(1:2, each = 5)
)
lm_model <- lm(Dependent_Variable ~ BMI + Weight + Sex + Treatment, data = data_cov)
anova(lm_model)

###############################################################
# MANOVA
###############################################################
data_manova <- data.frame(
  Drug = rep(c("A", "B"), each = 5),
  D1_Glucose = c(236, 200, 125, 145, 100, 120, 140, 160, 100, 89),
  D1_HB = c(207.5, 340, 179.5, 356, 131, 131, 169.5, 73, 124.5, 105.5),
  D2_Glucose = c(189, 300, 156, 224.5, 102, 109, 207, 94, 89, 100),
  D2_HB = c(118, 260, 185.5, 124.5, 134, 118, 169.5, 73, 124.5, 105.5),
  D3_Glucose = c(159, 230, 123, 204, 99, 120, 169, 150, 127, 109),
  D3_HB = c(131, 169.5, 73, 124.5, 105.5, 185.5, 224.5, 134, 118, 118)
)
manova_result <- manova(cbind(D1_Glucose, D1_HB, D2_Glucose, D2_HB, D3_Glucose, D3_HB) ~ Drug, data = data_manova)
summary(manova_result, test = "Pillai")

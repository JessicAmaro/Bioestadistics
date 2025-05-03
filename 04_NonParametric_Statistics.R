
###############################################################
# REQUIRED LIBRARIES
###############################################################
# install.packages("DescTools")
# install.packages("multcomp")
# install.packages("emmeans")
library(DescTools)
library(multcomp)
library(emmeans)

###############################################################
# SIMPLE CHI-SQUARE TEST
###############################################################
observed <- c(152, 39, 53, 6)
expected <- c(140.6, 46.9, 46.9, 15.6)
chisq.test(observed, p = expected / sum(expected))

###############################################################
# KOLMOGOROV-SMIRNOV TEST
###############################################################
observed <- c(5, 5, 3, 1, 1)
expected <- c(3, 3, 3, 3, 3)
ecdf_observed <- cumsum(sort(observed)) / sum(observed)
ecdf_expected <- cumsum(expected) / sum(expected)
differences <- abs(ecdf_observed - ecdf_expected)
max(differences)

###############################################################
# MCNEMAR TEST
###############################################################
data <- matrix(c(16, 18, 12, 4), nrow = 2, byrow = TRUE)
mcnemar.test(data)

###############################################################
# WILCOXON TEST
###############################################################
x <- c(142,140,144,144,142,146,149,150,142,148)
y <- c(138,136,147,139,146,141,143,145,136,146)
wilcox.test(x, y, paired = TRUE)

###############################################################
# MANN-WHITNEY TEST
###############################################################
group1 <- c(118.6,120.1,122,124.1,126.5,128.8,129.6)
group2 <- c(121.5,123.4,123.8,124.3,130.2,130.8)
wilcox.test(group1, group2)

###############################################################
# COCHRAN TEST
###############################################################
data <- data.frame(Group1=c(1,1,0,1,0,0,0,0), Group2=c(0,1,0,1,1,1,0,0),
                   Group3=c(0,1,0,0,1,0,1,1), Group4=c(0,1,1,1,1,0,1,1),
                   Group5=c(1,1,0,0,1,1,1,0))
# Remove rows with identical values
clean_data <- data[!apply(data,1,function(r) all(r==r[1])),]

###############################################################
# FRIEDMAN TEST
###############################################################
data <- data.frame(Treat1=c(1.5,1.4,1.4,1.2,1.4), Treat2=c(2.7,2.9,2.1,3.0,3.3),
                   Treat3=c(2.2,2.2,2.4,2.0,2.5), Treat4=c(1.3,1.0,1.1,1.3,1.5))
friedman.test(as.matrix(data))

###############################################################
# KRUSKAL-WALLIS TEST
###############################################################
data <- data.frame(Group1=c(14,12.1,19.6,8.2), Group2=c(8.4,5.1,7.3,6.6),
                   Group3=c(6.9,5.3,5.8,4.1))
groups <- rep(1:3, each=4)
kruskal.test(unlist(data), g=groups)

###############################################################
# ANCOVA
###############################################################
data2 <- data.frame(Covariate=rep(40:60,each=2),
                    Shift=rep(c("Morning","Evening"),each=9),
                    Method=as.factor(rep(1:3)),
                    X=runif(18,40,100))
ancova_model <- lm(X ~ Method * Shift + Covariate, data = data2)
anova(ancova_model)

###############################################################
# SHAPIRO-WILK TEST EXAMPLE
###############################################################
before <- c(214,362,202,158,403,219,307,331)
after <- c(232,276,224,412,562,203,340,313)
shapiro.test(before)
shapiro.test(after)

# Title     : Kruskal-Wallis
# Objective : Likelihoods - comparison
# Created by: Christina Mayr
# Created on: 20.01.22
library(ggplot2)
library(Hmisc)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(writexl)
library(gmodels)
library(DescTools)
library(dplyr)
library(writexl)
library(xtable)
library(readxl)

set.seed(1234)

alpha <- 0.05
source("src/read_data.R")
source("src/constants.R")
source("src/derived_quantities.R")
source("src/statTests.R")

mannWhitneyUTest <- function(variables, cond1, cond2, route) {

  output1 <- droplevels(variables[variables$condition == cond1,])
  output2 <- droplevels(variables[variables$condition == cond2,])
  variables_ <- rbind(output1, output2)

  # Otherwise, if both x and y are given and paired is FALSE, a Wilcoxon rank sum test
  # (equivalent to the Mann-Whitney test: see the Note) is carried out.
  # In this case, the null hypothesis is that the distributions of x and y differ by a
  # location shift of mu and the alternative is that they differ by some other location shift
  # (and the one-sided alternative "greater" is that x is shifted to the right of y).
  # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/wilcox.test
  m1 <- wilcox.test(eval(as.symbol(route)) ~ condition, data = variables_, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)

  return(m1)
}


is_different_mann_whitney_u <- function(variables, conditionPairs, route, difference) {

  df <- data.frame(Route = c(), Difference = c(), Condition1 = c(), Condition2 = c(), pValue = c(), isDifferent = c())

  for (condition1 in colnames(conditionPairs)) {
    condition2 <- as.character(conditionPairs[[condition1]])
    m1 <- mannWhitneyUTest(variables = variables, cond1 = condition1, cond2 = condition2, route)
    val <- m1$p.value
    W <- m1$statistic[[1]]

    df_ <- data.frame(Route = c(route),
                      Difference = c(difference),
                      Condition1 = c(condition1),
                      Condition2 = c(condition2),
                      pValue = c(val),
                      isDifferent = c(val <= alpha),
                      W = W
    )
    df <- rbind(df, df_)
  }
  return(df)
}

get_pvalues_for_subpopulation <- function(variables, groupname) {


  effect.motivation <- data.frame(
    "Congestion info + arrow" = "Congestion info + arrow + team spirit",
    "Congestion info + arrow + top down view" = "Congestion info + arrow + top down view + team spirit",
    "Arrow" = "Arrow + team spirit",
    "Arrow + top down view" = "Arrow + top down view + team spirit"
  )

  effect.density <- data.frame(
    "Arrow" = "Congestion info + arrow",
    "Arrow + top down view" = "Congestion info + arrow + top down view",
    "Arrow + team spirit" = "Congestion info + arrow + team spirit",
    "Arrow + top down view + team spirit" = "Congestion info + arrow + top down view + team spirit"
  )

  effect.route <- data.frame(
    "Congestion info + arrow" = "Congestion info + arrow + top down view",
    "Congestion info + arrow + team spirit" = "Congestion info + arrow + top down view + team spirit",
    "Arrow" = "Arrow + top down view",
    "Arrow + team spirit" = "Arrow + top down view + team spirit"
  )

  df <- data.frame(route = c(), difference = c(), cond1 = c(), cond2 = c(), pval = c(), isDifferent = c())

  for (route_ in c("Long", "Medium", "Short")) {
    df <- rbind(df, is_different_mann_whitney_u(variables, effect.density, route_, "Congestion info"))
    df <- rbind(df, is_different_mann_whitney_u(variables, effect.motivation, route_, "Team spirit"))
    df <- rbind(df, is_different_mann_whitney_u(variables, effect.route, route_, "Top down view"))
  }

  df <- cbind(Group = groupname, df)

  return(df)
}


# extract variables from data

# students

survey_results <- get_survey_results("data/Table-S6-Survey-Raw-data.xlsx", transform_likert = TRUE)

survey_results <- subset(survey_results, Informed == "Information provided")

variablesstudents <- subset(all, group == "Student & faculty associate")
res1 <- get_pvalues_for_subpopulation(variablesstudents, "Students")

# football fans
variablesfans <- subset(all, group == "Fan")
res2 <- get_pvalues_for_subpopulation(variablesfans, "Fans")





filename <- "output/pvals.tex"

print(xtable(res1, type = "latex", digits = 2), floating = FALSE, file = filename, include.rownames = FALSE)


print(xtable(results, type = "latex", digits = 2), floating = FALSE, file = filename, include.rownames = FALSE)







# print statistical differences only

aa <- write_mean_and_kruskal_wallis(informed = TRUE)
results_2 <- results
results_2$mean1 <- -1
results_2$mean2 <- -1
for(i in 1:nrow(results_2)) {
  c1 <- lapply(results_2[i,]$Condition1[[1]][1], as.character)[1]
  c2 <- lapply(results_2[i,]$Condition2[[1]][1], as.character)[1]
  Group <- lapply(results_2[i,]$Group[[1]][1], as.character)[1]
  route <- lapply(results_2[i,]$Route[[1]][1], as.character)[1]

  aaa <- aa[aa$Group == Group & aa$Route == route & aa$Condition == c1, ]
  bbb <- aa[aa$Group == Group & aa$Route == route & aa$Condition == c2, ]

  results_2[results_2$Group == Group & results_2$Route == route & results_2$Condition1 == c1, c("mean1") ] <- aaa$mean
  results_2[results_2$Group == Group & results_2$Route == route & results_2$Condition2 == c2, c("mean2") ] <- bbb$mean
}

results_2$mean2_mean1 <- results_2$mean2 - results_2$mean1
results_2$AddedComponent <- results_2$Difference
results_2$Condition <- results_2$Condition1

results_difference <- results_2[results_2$isDifferent == TRUE, ]
results_difference <- results_difference[order(results_difference$mean2_mean1),]
extract <- c("Group", "Route", "Condition", "AddedComponent", "mean1" , "mean2", "pValue", "W")
results_difference <- results_difference[, extract]
results_difference <- results_difference[order(results_difference$AddedComponent),]
results_difference <- results_difference[order(results_difference$Group),]



filename <- "output/pvalsAndMeansSignificant.tex"
print(xtable(results_difference, type = "latex", digits = 4), floating = FALSE, file = filename, include.rownames = FALSE)
print("finished")







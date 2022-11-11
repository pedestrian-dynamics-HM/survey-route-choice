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
PVALPRECISION <- 3

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

  for (condition1 in names(conditionPairs)) {
    condition2 <- as.character(conditionPairs[condition1])
    m1 <- mannWhitneyUTest(variables = variables, cond1 = condition1, cond2 = condition2, route)
    val <- m1$p.value
    W <- m1$statistic[[1]]

    df_ <- data.frame(Route = c(route),
                      Difference = c(difference),
                      Condition1 = c(condition1),
                      Condition2 = c(condition2),
                      pValue = c(round(val, PVALPRECISION)),
                      W = round(W, 1),
                      isDifferent = c(val <= alpha)
    )
    df <- rbind(df, df_)
  }
  return(df)
}

get_effect_component_using_Mann_whitney_U <- function(route_attractiveness, subpopulation) {

  route_attractiveness <- subset(route_attractiveness, group == subpopulation)

  # compare condition with and without team spirit
  teamspirit <- c(
    "Congestion info + arrow" = "Congestion info + arrow + team spirit",
    "Congestion info + arrow + top down view" = "Congestion info + arrow + top down view + team spirit",
    "Arrow" = "Arrow + team spirit",
    "Arrow + top down view" = "Arrow + top down view + team spirit"
  )

  # compare condition with and without congestion info
  congestionInfo <- c(
    "Arrow" = "Congestion info + arrow",
    "Arrow + top down view" = "Congestion info + arrow + top down view",
    "Arrow + team spirit" = "Congestion info + arrow + team spirit",
    "Arrow + top down view + team spirit" = "Congestion info + arrow + top down view + team spirit"
  )

  # compare condition with and without top down view
  topDownView <- c(
    "Congestion info + arrow" = "Congestion info + arrow + top down view",
    "Congestion info + arrow + team spirit" = "Congestion info + arrow + top down view + team spirit",
    "Arrow" = "Arrow + top down view",
    "Arrow + team spirit" = "Arrow + top down view + team spirit"
  )

  df <- data.frame(route = c(), difference = c(), cond1 = c(), cond2 = c(), pval = c(), isDifferent = c())

  for (route_ in c("RouteAttractivenessLong", "RouteAttractivenessMedium", "RouteAttractivenessShort")) {
    df <- rbind(df, is_different_mann_whitney_u(route_attractiveness, congestionInfo, route_, "Congestion info"))
    df <- rbind(df, is_different_mann_whitney_u(route_attractiveness, teamspirit, route_, "Team spirit"))
    df <- rbind(df, is_different_mann_whitney_u(route_attractiveness, topDownView, route_, "Top down view"))
  }

  return(df)
}


extract_designs_with_sig_difference <- function(all_designs) {

  data <- subset(all_designs, isDifferent == TRUE)
  data$pW <- paste("p = ", round(data$pValue, PVALPRECISION), ", W = ", data$W)
  data <- data[c("Difference", "Route", "Condition1", "Condition2", "pW")]
  data <- data %>% pivot_wider(names_from = Route, values_from = pW)
  data <- data[order(data$Difference),]

  return(data)
}


# read data
survey_results <- get_survey_results("data/Table-S6-Survey-Raw-data.xlsx", transform_likert = TRUE)
route_attractiveness <- get_route_attractiveness_long_format(survey_results)

# compare the effects of components only makes sense, when information is provided:
route_attractiveness_informed <- subset(route_attractiveness, Informed == "InformationProvided")

## for students
supplements_table_S4_students <- get_effect_component_using_Mann_whitney_U(route_attractiveness_informed, subpopulation = "Student & faculty associate")
## for fans
supplements_table_S5_fans <- get_effect_component_using_Mann_whitney_U(route_attractiveness_informed, subpopulation = "Fan")

## report significant differences in main manuscript
table_3_students <- extract_designs_with_sig_difference(supplements_table_S4_students)
table_4_5_fans <- extract_designs_with_sig_difference(supplements_table_S5_fans)

# write results
print(xtable(supplements_table_S4_students,
             type = "latex",
             digits = PVALPRECISION), floating = FALSE,
      file = "output/supplements_table_S4_students.tex",
      include.rownames = FALSE)

print(xtable(supplements_table_S5_fans,
             type = "latex",
             digits = PVALPRECISION), floating = FALSE,
      file = "output/supplements_table_S5_fans.tex",
      include.rownames = FALSE)

print(xtable(table_3_students,
             type = "latex",
             digits = PVALPRECISION), floating = FALSE,
      file = "output/table_3_students.tex",
      include.rownames = FALSE)

print(xtable(table_4_5_fans,
             type = "latex",
             digits = PVALPRECISION), floating = FALSE,
      file = "output/table_4_5_fans.tex",
      include.rownames = FALSE)



# print statistical differences only

taa <- write_mean_and_kruskal_wallis(informed = TRUE)
results_2 <- results
results_2$mean1 <- -1
results_2$mean2 <- -1
for (i in 1:nrow(results_2)) {
  c1 <- lapply(results_2[i,]$Condition1[[1]][1], as.character)[1]
  c2 <- lapply(results_2[i,]$Condition2[[1]][1], as.character)[1]
  Group <- lapply(results_2[i,]$Group[[1]][1], as.character)[1]
  route <- lapply(results_2[i,]$Route[[1]][1], as.character)[1]

  aaa <- aa[aa$Group == Group &
              aa$Route == route &
              aa$Condition == c1,]
  bbb <- aa[aa$Group == Group &
              aa$Route == route &
              aa$Condition == c2,]

  results_2[results_2$Group == Group &
              results_2$Route == route &
              results_2$Condition1 == c1, c("mean1")] <- aaa$mean
  results_2[results_2$Group == Group &
              results_2$Route == route &
              results_2$Condition2 == c2, c("mean2")] <- bbb$mean
}

results_2$mean2_mean1 <- results_2$mean2 - results_2$mean1
results_2$AddedComponent <- results_2$Difference
results_2$Condition <- results_2$Condition1

results_difference <- results_2[results_2$isDifferent == TRUE,]
results_difference <- results_difference[order(results_difference$mean2_mean1),]
extract <- c("Group", "Route", "Condition", "AddedComponent", "mean1", "mean2", "pValue", "W")
results_difference <- results_difference[, extract]
results_difference <- results_difference[order(results_difference$AddedComponent),]
results_difference <- results_difference[order(results_difference$Group),]


filename <- "output/pvalsAndMeansSignificant.tex"
print(xtable(results_difference, type = "latex", digits = 4), floating = FALSE, file = filename, include.rownames = FALSE)
print("finished")







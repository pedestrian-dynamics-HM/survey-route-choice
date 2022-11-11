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
MEANPREC <- 2

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
  mann_whit_results <- wilcox.test(eval(as.symbol(route)) ~ condition, data = variables_, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)

  mann_whit_results$mean1 <- round(mean(output1[, c(route)]), MEANPREC)
  mann_whit_results$mean2 <- round(mean(output2[, c(route)]), MEANPREC)

  return(mann_whit_results)
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
                      isDifferent = c(val <= alpha),
                      meanValCondition1 = m1$mean1,
                      meanValCondition2 = m1$mean2
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

format_table <- function(table) {

  table$Condition1 <- paste(table$Condition1, " (mean = ", table$meanValCondition1, ")", sep = "")
  table$Condition2 <- paste(table$Condition2, " (mean = ", table$meanValCondition2, ")", sep = "")
  table <- table %>% dplyr::select(-c("isDifferent", "meanValCondition1", "meanValCondition2" ))

  return(table)
}


# read data
survey_results <- get_survey_results("data/Table-S6-Survey-Raw-data.xlsx", transform_likert = TRUE)
route_attractiveness <- get_route_attractiveness_long_format(survey_results)
route_attractiveness_informed <- subset(route_attractiveness, Informed == "InformationProvided")
route_attractiveness_priorToInfo <- subset(route_attractiveness, Informed == "PriorToInformation")


# 1 normality check
normality.long <- route_attractiveness_informed %>% group_by(group, condition) %>% shapiro_test(RouteAttractivenessLong)
normality.medium <- route_attractiveness_informed %>% group_by(group, condition) %>% shapiro_test(RouteAttractivenessMedium)
normality.short <- route_attractiveness_informed %>% group_by(group, condition) %>% shapiro_test(RouteAttractivenessShort)
normality <- rbind(normality.long, normality.medium, normality.short)
## comment: the distributions are not normally distributed -> we need to use non-parametric tests in step 2 (Mann Whitney U)


# 2 Statistics +  Kruskal Wallis test
## before we compare two message desings we check whether there is a difference or not

kruskal.long <- route_attractiveness %>% group_by(Informed, group) %>% kruskal_test(RouteAttractivenessLong ~ condition)
kruskal.medium <- route_attractiveness %>% group_by(Informed, group) %>% kruskal_test(RouteAttractivenessMedium ~ condition)
kruskal.short <- route_attractiveness %>% group_by(Informed, group) %>% kruskal_test(RouteAttractivenessShort ~ condition)

kruskal <- rbind(kruskal.long, kruskal.medium, kruskal.short)
supplements_table_S3_kruskal <- kruskal[with(kruskal, order(Informed, group)), ]


stat.long <- route_attractiveness %>% group_by(Informed, group, condition) %>% get_summary_stats(RouteAttractivenessLong, type = "full")
stat.medium <- route_attractiveness %>% group_by(Informed, group, condition) %>% get_summary_stats(RouteAttractivenessMedium, type = "full")
stat.short <- route_attractiveness %>% group_by(Informed, group, condition) %>% get_summary_stats(RouteAttractivenessShort, type = "full")

stat <- rbind(stat.long, stat.medium, stat.short)
supplements_table_S1_S2_stats <- stat[with(stat, order(Informed,  group, variable)), ]
supplements_table_S1_S2_stats <- supplements_table_S1_S2_stats[, c("Informed", "variable", "group", "condition",  "mean", "median", "sd", "n")]

# 3 Mann Whintey U tests
## We only need to consider the informed case (without info, the message design does not exit, thus, has no effect)
## We compare whether adding one of the three message components (congestion info, top down view, team spirit)
## makes a difference.

supplements_table_S4_students <- get_effect_component_using_Mann_whitney_U(route_attractiveness_informed, subpopulation = "Student & faculty associate")
supplements_table_S5_fans <- get_effect_component_using_Mann_whitney_U(route_attractiveness_informed, subpopulation = "Fan")

## extract significant differences in main manuscript
table_3_students <- extract_designs_with_sig_difference(supplements_table_S4_students)
table_4_5_fans <- extract_designs_with_sig_difference(supplements_table_S5_fans)


# 3 write results

supplements_table_S4_students <- format_table(supplements_table_S4_students)
supplements_table_S5_fans <- format_table(supplements_table_S5_fans)

print(xtable(supplements_table_S1_S2_stats,
             type = "latex",
             digits = PVALPRECISION), floating = FALSE,
      file = "output/supplements_table_S1_S2_stats.tex",
      include.rownames = FALSE)

print(xtable(supplements_table_S3_kruskal,
             type = "latex",
             digits = PVALPRECISION), floating = FALSE,
      file = "output/supplements_table_S3_kruskal.tex",
      include.rownames = FALSE)

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


print("Finished data export.")




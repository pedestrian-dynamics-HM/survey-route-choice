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

source("src/read_data.R")
source("src/constants.R")

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

  mann_whit_results$mean1 <- round(mean(output1[, c(route)]), PRECISION_VAR)
  mann_whit_results$mean2 <- round(mean(output2[, c(route)]), PRECISION_VAR)

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
                      pValue = c(round(val, PRECISION_PVAL)),
                      W = round(W, PRECISION_TEST_STAT),
                      isDifferent = c(val <= ALPHA),
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
  data$pW <- paste("p = ", round(data$pValue, PRECISION_PVAL), ", W = ", data$W)
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

print(" ---- Script started - Investigate effect of message components ----")


# read data
path_to_survey_file <-  file.path(getwd(), "data", "Table S1.xlsx") # see sub-dir data
survey_results <- get_survey_results(path_to_survey_file, transform_likert = TRUE)
route_attractiveness <- get_route_attractiveness_long_format(survey_results)
route_attractiveness_informed <- subset(route_attractiveness, Informed == "InformationProvided")
route_attractiveness_priorToInfo <- subset(route_attractiveness, Informed == "PriorToInformation")


# 1 normality check
normality.long <- route_attractiveness_informed %>% group_by(group, condition) %>% shapiro_test(RouteAttractivenessLong)
normality.medium <- route_attractiveness_informed %>% group_by(group, condition) %>% shapiro_test(RouteAttractivenessMedium)
normality.short <- route_attractiveness_informed %>% group_by(group, condition) %>% shapiro_test(RouteAttractivenessShort)
normality <- rbind(normality.long, normality.medium, normality.short)
## comment: the distributions are not normally distributed -> we need to use non-parametric tests in step 2 (Mann Whitney U)


# 2 Statistics

stat.long <- route_attractiveness %>% group_by(Informed, group, condition) %>% get_summary_stats(RouteAttractivenessLong, type = "full")
stat.medium <- route_attractiveness %>% group_by(Informed, group, condition) %>% get_summary_stats(RouteAttractivenessMedium, type = "full")
stat.short <- route_attractiveness %>% group_by(Informed, group, condition) %>% get_summary_stats(RouteAttractivenessShort, type = "full")

stat <- rbind(stat.long, stat.medium, stat.short)
routeAttractiveness_statistics <- stat[with(stat, order(Informed, group, variable)), ]
routeAttractiveness_statistics <- routeAttractiveness_statistics[, c("Informed", "variable", "group", "condition", "mean", "median", "sd", "n")]

#  3 Kruskal Wallis test
## before we compare two message desings we check whether there is a difference or not

kruskal.long <- route_attractiveness %>% group_by(Informed, group) %>% kruskal_test(RouteAttractivenessLong ~ condition)
kruskal.medium <- route_attractiveness %>% group_by(Informed, group) %>% kruskal_test(RouteAttractivenessMedium ~ condition)
kruskal.short <- route_attractiveness %>% group_by(Informed, group) %>% kruskal_test(RouteAttractivenessShort ~ condition)

kruskal <- rbind(kruskal.long, kruskal.medium, kruskal.short)
routeAttractiveness_kruskal_difference_messagedesign <- kruskal[with(kruskal, order(Informed, group)), ]


# 4 Mann Whintey U tests
## We only need to consider the informed case (without info, the message design does not exit, thus, has no effect)
## We compare whether adding one of the three message components (congestion info, top down view, team spirit)
## makes a difference.

MannWhitneyMessageComp_students <- get_effect_component_using_Mann_whitney_U(route_attractiveness_informed, subpopulation = "Student & faculty associate")
MannWhitneyMessageComp_fans <- get_effect_component_using_Mann_whitney_U(route_attractiveness_informed, subpopulation = "Fan")

## extract significant differences in main manuscript
MannWhitneyMessageComp_significant_students <- extract_designs_with_sig_difference(MannWhitneyMessageComp_students)
MannWhitneyMessageComp_significant_fans <- extract_designs_with_sig_difference(MannWhitneyMessageComp_fans)

# 5 assess whether the route attractiveness differs when adding topdownview, teamspirit or top down view & team spirit

## check difference for fans only for the following three conditions
route_attractiveness_subset <- subset(route_attractiveness_informed, group=="Fan")
c1 <- "Congestion info + arrow + team spirit"
c2 <- "Congestion info + arrow + top down view"
c3 <- "Congestion info + arrow + top down view + team spirit"

route_attractiveness_subset <- route_attractiveness_subset %>% filter(condition == c1 | condition == c2 | condition == c3 )
fans_long_route_no_diff_when_adding_together <- route_attractiveness_subset %>% kruskal_test(RouteAttractivenessLong ~ condition)


# 6 write results

MannWhitneyMessageComp_students <- format_table(MannWhitneyMessageComp_students)
MannWhitneyMessageComp_fans <- format_table(MannWhitneyMessageComp_fans)

print("Start export ...")


## manuscript data

filename <- file.path(getwd(), "output", "manuscript_table_3.tex")
print(xtable(MannWhitneyMessageComp_significant_students,
             type = "latex",
             digits = PRECISION_PVAL), floating = FALSE,
      file = filename,
      include.rownames = FALSE)
print(filename)

filename <- file.path(getwd(), "output", "manuscript_table_4_5.tex")
print(xtable(MannWhitneyMessageComp_significant_fans,
             type = "latex",
             digits = PRECISION_PVAL), floating = FALSE,
      file = filename,
      include.rownames = FALSE)
print(filename)

filename <- file.path(getwd(), "output", "manuscript_adding_comp_together_no_effect_fans.tex")
print(xtable(fans_long_route_no_diff_when_adding_together,
             type = "latex",
             digits = PRECISION_PVAL), floating = FALSE,
      file = filename,
      include.rownames = FALSE)
print(filename)

## data stored in supplements

filename <- file.path(getwd(), "output", "supplements_table_S2.tex")
print(xtable(MannWhitneyMessageComp_students,
             type = "latex",
             digits = PRECISION_PVAL), floating = FALSE,
      file = filename,
      include.rownames = FALSE)
print(filename)

filename <- file.path(getwd(), "output", "supplements_table_S3.tex")
print(xtable(MannWhitneyMessageComp_fans,
             type = "latex",
             digits = PRECISION_PVAL), floating = FALSE,
      file = filename,
      include.rownames = FALSE)
print(filename)

filename <- file.path(getwd(), "output", "supplements_table_S9.tex")
print(xtable(routeAttractiveness_statistics,
             type = "latex",
             digits = PRECISION_PVAL), floating = FALSE,
      file = filename,
      include.rownames = FALSE)
print(filename)

filename <- file.path(getwd(), "output", "supplements_table_S8.tex")
print(xtable(routeAttractiveness_kruskal_difference_messagedesign,
             type = "latex",
             digits = PRECISION_PVAL), floating = FALSE,
      file = filename,
      include.rownames = FALSE)
print(filename)

print("... export finished.")

print(" -------------------- Script finished -------------------------")




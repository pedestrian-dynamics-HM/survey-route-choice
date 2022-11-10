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
library(wordspace)
library(xtable)


set.seed(1234)

alpha <- 0.05
source("survey_results/src/read_data.R")
source("survey_results/src/constants.R")
source("survey_results/src/derived_quantities.R")



# extract variables from data

conds <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4")

compute_route_utilizations <- function(output, population, condition){

  output <- output %>% dplyr::select("RouteA", "RouteB", "RouteC")
  output <- normalize.rows(as.matrix(output), method = "manhattan") #L1 norm
  output <- colSums(output, dims = 1)
  output <- normalize.cols(as.matrix(output), method = "manhattan") #L1 norm

  df_ <- data.frame(
      Group = c(population),
      Condition = c(condition),
      Short = c(output["RouteC",]*100),
      Medium = c(output["RouteB",]*100),
      Long = c(output["RouteA",]*100)

  )
  return(df_)
}

compute_route_utilization_conditionwise <- function(variables, population= "Fans") {

  variables <- variables %>% dplyr::select("condition", "RouteA", "RouteB", "RouteC")
  df <- data.frame(Group = c(), Condition = c(), Short = c(), Medium = c(), Long = c())
  for (c_ in conds) {
    print(c_)
    output <- droplevels(variables[variables$condition == c_,])
    df_ <- compute_route_utilizations(output, population, c_)
    df <- rbind(df, df_)

  }
  return(df)
}

# students
variablesstudents <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = (1:500))
variablesstudents <- get_4x2_informed(variablesstudents)
res1 <- compute_route_utilization_conditionwise(variablesstudents, "Students")


# football fans
variablesfans <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = -(1:500))
variablesfans <- get_4x2_informed(variablesfans)
res2 <- compute_route_utilization_conditionwise(variablesfans, "Fans")

# students
variablesstudents <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = (1:500))
variablesstudents <- get_4x2_uninformed(variablesstudents)
res1_uninformed <- compute_route_utilizations(variablesstudents, "Students", "Uninformed")


# football fans
variablesfans <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = -(1:500))
variablesfans <- get_4x2_uninformed(variablesfans)
res2_uninformed <- compute_route_utilizations(variablesfans, "Fans", "Uninformed")


# write results
results <- rbind(res1_uninformed, res1, res2_uninformed, res2)

condition_short <- c("Uninformed", "A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4")
condition_long <- c("No congestion info",
                    "Congestion info + arrow",
                    "Congestion info + arrow + top down view",
                    "Congestion info + arrow + team spirit",
                    "Congestion info + arrow + top down view + team spirit",
                    "Arrow",
                    "Arrow + top down view",
                    "Arrow + team spirit ",
                    "Arrow + top down view + team spirit")

results$Condition <- plyr::mapvalues(results$Condition, from = condition_short, to = condition_long)
filename <- "survey_results/final_table/RouteUtilizations.tex"
print(xtable(results, type = "latex", digits = 0), floating = FALSE, file = filename, include.rownames = FALSE)





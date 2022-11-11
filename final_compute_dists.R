
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
source("src/read_data.R")
source("src/constants.R")
source("src/derived_quantities.R")


# extract variables from data


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


# read data
survey_results <- get_survey_results("data/Table-S6-Survey-Raw-data.xlsx", transform_likert = TRUE)
route_attractiveness <- get_route_attractiveness_long_format(survey_results)



filename <- "output/table_6_RouteChoiceProportions.tex"
print(xtable(results, type = "latex", digits = 0), floating = FALSE, file = filename, include.rownames = FALSE)





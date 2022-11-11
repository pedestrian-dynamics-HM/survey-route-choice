
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
source("src/read_data.R")
source("src/constants.R")
source("src/derived_quantities.R")


traffic_assignment_model <- function(output, ...){

  if (is_grouped_df(output)) {
    results <- output %>% doo(traffic_assignment_model, ...)
    return(results)
  }

  output <- output[, c("RouteAttractivenessLong", "RouteAttractivenessMedium", "RouteAttractivenessShort") ]
  output <- normalize.rows(as.matrix(output), method = "manhattan") #L1 norm
  output <- colSums(output, dims = 1)
  output <- normalize.cols(as.matrix(output), method = "manhattan") #L1 norm

  df_ <- data.frame(
      Short = c(output["RouteAttractivenessShort",]*100),
      Medium = c(output["RouteAttractivenessMedium",]*100),
      Long = c(output["RouteAttractivenessLong",]*100)

  )
  return(df_)
}

# read data
survey_results <- get_survey_results("data/Table-S6-Survey-Raw-data.xlsx", transform_likert = TRUE)
route_attractiveness <- get_route_attractiveness_long_format(survey_results)

# prior to information the message design has no effect (see Kruskal Wallis test results)
## we lump the conditions
route_attractiveness$condition[route_attractiveness$Informed == "PriorToInformation"] <- "PriorToInformation"

#  use traffic assignment model to compute route choice distributions
table_6_RouteChoiceProportions <- route_attractiveness %>% group_by(group, condition) %>% traffic_assignment_model()

print(xtable(table_6_RouteChoiceProportions, type = "latex", digits = 2),
floating = FALSE,
file = "output/table_6_RouteChoiceProportions.tex",
include.rownames = FALSE)





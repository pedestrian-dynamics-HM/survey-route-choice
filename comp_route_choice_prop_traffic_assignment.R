
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

source("src/read_data.R")
source("src/constants.R")


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

print(" ----- Script started - Compute route choice distributions ----")

# 1 read data
path_to_survey_file <-  file.path(getwd(), "data", "Table S1.xlsx") # see sub-dir data
survey_results <- get_survey_results(path_to_survey_file) # excluding incomplete samples
route_attractiveness <- get_route_attractiveness_long_format(survey_results)

# 2 prior to information the message design has no effect (see Kruskal Wallis test results)
## we lump the conditions
route_attractiveness$condition[route_attractiveness$Informed == "PriorToInformation"] <- "PriorToInformation"

# 3 use traffic assignment model to compute route choice distributions
table_6_RouteChoiceProportions <- route_attractiveness %>% group_by(group, condition) %>% traffic_assignment_model()

# 4 export route choice distributions
print("Start export ...")

filename <- file.path(getwd(), "output", "manuscript_table_6.tex")
print(xtable(table_6_RouteChoiceProportions, type = "latex", digits = PRECISION_PERCENTAGE),
floating = FALSE,
file = filename,
include.rownames = FALSE)
print(filename)
print("... export finished.")

print(" -------------------- Script finished -------------------------")

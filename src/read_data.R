library(plyr)
library(dplyr)
library(ggplot2)
library(ltm)
library(reshape2)


read.raw_data <- function(filepath, completed_only = TRUE) {
  # read excel file
  library(readxl)
  output <- readxl::read_excel(filepath)

  # remove incomplete survey results
  if (completed_only) {
    output <- subset(output, submitdate == "1980-01-01 00:00:00") # finished samples have this timestamp
  }

  # extract variables of interest
  output <- output %>% dplyr::select("id", "group" | "condition" | starts_with("Var", ignore.case = TRUE) | "lastpage")
  output <- output %>% dplyr::select(-ends_with("Time", ignore.case = TRUE))
  output <- output %>% dplyr::select(-ends_with("VarRandomCondition", ignore.case = TRUE))

  # A0 we remove the message design A0 since it does not include a route recommendation
  output$condition <- str_trim(output$condition)
  output$condition <- replace_na(output$condition, "NotAssigned")
  output <- subset(output, condition != "A0")

  output$lastpage <- replace_na(output$lastpage, -1)

  output <- data.frame(output)

  return(output)
}

rename.variables <- function(data_frame) {

  col_names <- c("PriorToInformation_RouteAttractivenessLong" = "VarConditionB0.NoInfoRouteA.",
                 "PriorToInformation_RouteAttractivenessMedium" = "VarConditionB0.NoInfoRouteB.",
                 "PriorToInformation_RouteAttractivenessShort" = "VarConditionB0.NoInfoRouteC.",
                 "InformationProvided_RouteAttractivenessLong" = "VarConditionInfo.InformedRouteA.",
                 "InformationProvided_RouteAttractivenessMedium" = "VarConditionInfo.InformedRouteB.",
                 "InformationProvided_RouteAttractivenessShort" = "VarConditionInfo.InformedRouteC."
  )

  for (name in names(col_names)) {
    names(data_frame)[names(data_frame) == col_names[[name]]] <- name
  }


  condition_short <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4")
  condition_long <- c("Congestion info + arrow",
                      "Congestion info + arrow + top down view",
                      "Congestion info + arrow + team spirit",
                      "Congestion info + arrow + top down view + team spirit",
                      "Arrow",
                      "Arrow + top down view",
                      "Arrow + team spirit",
                      "Arrow + top down view + team spirit")

  data_frame$condition <- plyr::mapvalues(data_frame$condition, from = condition_short, to = condition_long)

  return(data_frame)

}

transform.likert_scales <- function(data_frame) {
  # data_frame: table or dataframe with mixed scales (likert, numeric)
  # this function only transforms the likert scaled features

  likert_scale <- c("Very unlikely" = 1,
                    "Unlikely" = 2,
                    "Neutral" = 3,
                    "Likely" = 4,
                    "Very likely" = 5,
                    "Strongly disagree" = 1,
                    "Disagree" = 2,
                    "Neutral" = 3,
                    "Agree" = 4,
                    "Strongly agree" = 5
  )
  order <- c("1", "2", "3", "4", "5")

  for (ii in get_likert_scaled_variable_names()) {
    data_frame[[ii]] <- revalue(data_frame[[ii]],
                                likert_scale,
                                warn_missing = FALSE)
    data_frame[[ii]] <- factor(data_frame[[ii]], levels = order)
  }

  return(data_frame)
}

get_route_attractiveness_long_format <- function(data) {


  col_names <- c("PriorToInformation_RouteAttractivenessLong",
                 "PriorToInformation_RouteAttractivenessMedium",
                 "PriorToInformation_RouteAttractivenessShort",
                 "InformationProvided_RouteAttractivenessLong",
                 "InformationProvided_RouteAttractivenessMedium",
                 "InformationProvided_RouteAttractivenessShort"
  )

  data <- data %>% dplyr::select( c("group", "condition", col_names) )

  data <- reshape(data,
                  direction = 'long',
                  varying = col_names,
                  timevar = 'Informed',
                  times = c('PriorToInformation', 'InformationProvided'),
                  v.names = c("RouteAttractivenessLong", "RouteAttractivenessMedium", "RouteAttractivenessShort"),
  )

  data <- data[ , c("Informed", names(data)[names(data) != "Informed"])]

  return(data)
}


get_within_factors <- function(variables) {

  variables <- variables[variables$Informed == "Informed",]

  variables <- variables %>% dplyr::select(-starts_with("Route", ignore.case = TRUE))
  variables <- variables %>% dplyr::select(-starts_with("fav_routes.", ignore.case = TRUE))

  return(variables)

}

get_survey_results <- function(filepath,
                               completed_only = TRUE,
                               transform_likert = TRUE) {
  data <- read.raw_data(filepath = filepath,
                        completed_only = completed_only)


  if (transform_likert) {
    data <- get_likert_scaled_vars_as_numeric(data)
  }

  data <- rename.variables(data)

  return(data)
}

get_likert_scaled_vars_as_numeric <- function(data_frame) {

  data_frame <- transform.likert_scales(data_frame)

  for (ii in get_likert_scaled_variable_names()) {
    data_frame[[ii]] <- as.numeric(data_frame[[ii]])
  }
  return(data_frame)
}




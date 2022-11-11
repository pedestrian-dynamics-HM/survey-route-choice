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

  # simplyfing the variable names
  output <- output %>% dplyr::select("condition" | starts_with("Var", ignore.case = TRUE))
  output <- output %>% dplyr::select(-ends_with("Time", ignore.case = TRUE))
  output <- output %>% dplyr::select(-ends_with("VarRandomCondition", ignore.case = TRUE))

  #A0 we remove the message design A0 since it does not include a route recommendation
  output <- droplevels(output[output$condition != "A0",])

  output <- data.frame(output)

  return(output)
}

rename.variables <- function(data_frame) {

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

  names(data_frame)[names(data_frame) == "RouteA"] <- "Long"
  names(data_frame)[names(data_frame) == "RouteB"] <- "Medium"
  names(data_frame)[names(data_frame) == "RouteC"] <- "Short" # unfortunately letter order and length are opposite

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

tidy_stages <- function(data) {
  # route_A route_B route_C route_A_informed route_B_informed route_C_informed
  # val_1   val_2   val_3   val_4   val_5   val_6
  #
  # informed   A    B     C
  # no        val_1 val_2 val_3
  # yes       val_4 val_5 val_6

  col_names <- c("NotInformed_RouteA" = "VarConditionB0.NoInfoRouteA.",
                 "NotInformed_RouteB" = "VarConditionB0.NoInfoRouteB.",
                 "NotInformed_RouteC" = "VarConditionB0.NoInfoRouteC.",
                 "Informed_RouteA" = "VarConditionInfo.InformedRouteA.",
                 "Informed_RouteB" = "VarConditionInfo.InformedRouteB.",
                 "Informed_RouteC" = "VarConditionInfo.InformedRouteC."
  )

  for (name in names(col_names)) {
    names(data)[names(data) == col_names[[name]]] <- name
  }


  data <- reshape(data,
                  direction = 'long',
                  varying = names(col_names),
                  timevar = 'Informed',
                  times = c('NotInformed', 'Informed'),
                  v.names = c('RouteA', 'RouteB', 'RouteC'),
  )

  # remove id column
  data <- subset(data, select = -c(id))

  # rename and factor state of information
  infostate <- c('Prior to information', 'Information provided')
  data$Informed <- plyr::mapvalues(data$Informed, from = c('NotInformed', 'Informed'), to = infostate )
  data$Informed <- factor(data$Informed, levels = infostate)

  # reorder cols
  data <- data %>%
    dplyr::select("Informed", everything())

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

  data <- tidy_stages(data)

  if (transform_likert) {
    data <- transform.likert_scales(data)
    data <- get_likert_scaled_vars_as_numeric(data)
  }

  data <- rename.variables(data)

  return(data)
}

get_likert_scaled_vars_as_numeric <- function(data_frame) {

  for (ii in get_likert_scaled_variable_names()) {
    data_frame[[ii]] <- as.numeric(data_frame[[ii]])
  }
  return(data_frame)
}




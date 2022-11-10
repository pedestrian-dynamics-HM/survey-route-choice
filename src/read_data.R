library(plyr)
library(dplyr)
library(ggplot2)
library(ltm)
library(reshape2)


read.raw_data <- function(filepath, completed_only = TRUE, remove_helper_var = TRUE, subpopulation = NULL) {
  output <- read.csv(filepath)
  if (completed_only) {
    output <- droplevels(output[output$submitdate != "",])
  }

  # see differences in populations
  if (subpopulation){
    output <- output[subpopulation, , drop = FALSE]
  }

  if (remove_helper_var) {
    output <- output %>% dplyr::select("condition" | starts_with("Var", ignore.case = TRUE))
    output <- output %>% dplyr::select(-ends_with("Time", ignore.case = TRUE))
    output <- output %>% dplyr::select(-ends_with("VarRandomCondition", ignore.case = TRUE))
  }

  return(output)
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
  order <- c( "1", "2", "3", "4", "5")

  for (ii in get_likert_scaled_variable_names()) {
    data_frame[[ii]] <- revalue(data_frame[[ii]],
                                likert_scale,
                                warn_missing = FALSE)
    data_frame[[ii]] <- factor( data_frame[[ii]] ,levels=order)
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

  col_names <- c("NotInformed.RouteA" = "VarConditionB0.NoInfoRouteA.",
                 "NotInformed.RouteB" = "VarConditionB0.NoInfoRouteB.",
                 "NotInformed.RouteC" = "VarConditionB0.NoInfoRouteC.",
                 "Informed.RouteA" = "VarConditionInfo.InformedRouteA.",
                 "Informed.RouteB" = "VarConditionInfo.InformedRouteB.",
                 "Informed.RouteC" = "VarConditionInfo.InformedRouteC."
  )

  for (name in names(col_names)) {
    names(data)[names(data) == col_names[[name]]] <- name
  }

  order <- c('NotInformed', 'Informed')
  data <- reshape(data,
                  direction = 'long',
                  varying = names(col_names),
                  timevar = 'Informed',
                  times = order,
                  v.names = c('RouteA', 'RouteB', 'RouteC'),
  )

  # remove id column
  data <- subset(data, select = -c(id))
  data$Informed <- factor(data$Informed, levels=order)

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
                               remove_helper_var = TRUE,
                               transform_likert = TRUE,
                               subpopulation = NULL,
                               decompose_condition = TRUE,
                               add_fav_routes = TRUE) {
  data <- read.raw_data(filepath = filepath,
                        completed_only = completed_only,
                        remove_helper_var = remove_helper_var,
                        subpopulation = subpopulation)
  data <- tidy_stages(data)

  if (decompose_condition){
    data <- add_decomposed_conditions(data)
  }

  if (add_fav_routes){
    data <- cbind(data,derive_favorite_route_from_likelihoods(data))
  }

  if (transform_likert) {
    data <- transform.likert_scales(data)
    data <- get_likert_scaled_vars_as_numeric(data)
  }

  return(data)
}


get_likert_scaled_vars_as_numeric <- function (data_frame){

  for (ii in get_likert_scaled_variable_names()) {
    data_frame[[ii]] <- as.numeric(data_frame[[ii]])
  }
  return(data_frame)
}

get_categorial_vars <- function (variables, remove_comment_vars = TRUE){
  data_frame <- variables[, unlist(lapply(variables, function(x) is.numeric(x) == FALSE))]

  if (remove_comment_vars){
    data_frame <- data_frame %>% dplyr::select(-ends_with("comment.", ignore.case = TRUE))
  }

  return(data_frame)
}

add_decomposed_conditions <- function(data_frame){

  data_frame <- data_frame %>% separate(condition, c("dummy", "AB", "levelN"), "", remove = FALSE)

  data_frame$DensityMap <- "ShowDensity"
  data_frame[data_frame$AB == "B", "DensityMap"] <- "NoDensity"

  data_frame$Motivation <- "NotMotivated"
  data_frame[data_frame$levelN == 3 | data_frame$levelN == 4, "Motivation"] <- "Motivated"

  data_frame$TopDownRoute <- "RouteNotDisplayed"
  data_frame[data_frame$levelN == 2 | data_frame$levelN == 4, "TopDownRoute"] <- "RouteDisplayed"

  data_frame$Recommendation <- "RecommendationProvided"
  data_frame[data_frame$levelN == 0, "Recommendation"] <- "RecommendationProvided"

  data_frame <- subset(data_frame, select = -c(dummy, AB, levelN))

  return(data_frame)
}

filter_informed_only <- function(data_frame){
  data_frame <- droplevels(data_frame[data_frame$Informed == "Informed",])
  data_frame <- subset(data_frame, select = -c(Informed))
  return(data_frame)
}

filter_uninformed_only <- function(data_frame){
  data_frame <- droplevels(data_frame[data_frame$Informed == "NotInformed",])
  data_frame <- subset(data_frame, select = -c(Informed))
  return(data_frame)
}

get_4x2_informed <- function(data_frame){
  data_frame <- droplevels(data_frame[data_frame$condition != "A0",])
  return(filter_informed_only(data_frame))
}


get_4x2_uninformed <- function(data_frame){
  data_frame <- droplevels(data_frame[data_frame$condition != "A0",])
  return(filter_uninformed_only(data_frame))
}

get_4x2 <- function(data_frame, informed=TRUE){

  if (informed == TRUE){
    return(get_4x2_informed(data_frame))
  } else{
    return(get_4x2_uninformed(data_frame))
  }


}


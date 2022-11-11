get_conditions_overview <- function() {
  condition_overview <- c("A0" = "Density info (A0)",
                          "A1" = "Density info \n+ Direction (A1)",
                          "A2" = "Density info \n+ Direction \n+ Route (A2)",
                          "A3" = "Density info \n+ Direction \n+ Motivation (A3)",
                          "A4" = "Density info \n+ Direction \n+ Route \n+ Motivation (A4)",
                          #"B0" = "No information \n(default behavior)",
                          "B1" = "Direction (B1)",
                          "B2" = "Direction \n+ Route (B2)",
                          "B3" = "Direction \n+ Motivation (B3)",
                          "B4" = "Direction \n+ Route \n+ Motivation (B4)")
  return(condition_overview)
}

get_route_choice_names <- function() {
  return(c(A = "Long route (A)", B = "Route (B)", C = "Short route (C)"))
}

get_likert_scaled_variable_names <- function() {
  names_ <- c("VarManipulationcheck.ImagineSituation.",
              "VarSocialNorms.SupportTeam.",
              "VarSocialNorms.CareFriends.",
              "VarSocialNorms.SupportFans.",
              "VarSocialNorms.TravelSafely.",
              "VarSocialNorms.WalkTogether.",
              "VarConditionB0Reason.UndestandEnv.",
              "VarConditionB0Reason.UnderstandRoutes.",
              "VarConditionB0Reason.PreferShortRoutes.",
              "VarConditionInfoReas.AmountInfo.",
              "VarConditionInfoReas.AmountInfoCounter.",
              "VarConditionInfoReas.Efficiency.",
              "VarConditionInfoReas.Comprehensivenss.",
              "VarConditionInfoReas.CondCompliance.",
              "VarConditionInfoReas.Marker.",
              "VarConditionInfoReas.Density.",
              "VarConditionInfoReas.Photographs.",
              "VarFaith.Reliability.",
              "VarFaith.Fairness.",
              "VarFaith.Motivation.",
              "VarFaith.Faith1.",
              "VarFaith.Faith2.",
              "VarFaith.SharedIdentity.",
              "VarConditionB0.NoInfoRouteA.",
              "VarConditionB0.NoInfoRouteB.",
              "VarConditionB0.NoInfoRouteC.",
              "VarConditionInfo.InformedRouteA.",
              "VarConditionInfo.InformedRouteB.",
              "VarConditionInfo.InformedRouteC.")
  return(names_)
}


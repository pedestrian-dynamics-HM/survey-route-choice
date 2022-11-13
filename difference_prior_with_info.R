library(Hmisc)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(writexl)
library(xtable)
library(crank)

set.seed(1234)

source("src/read_data.R")
source("src/constants.R")
source("src/derived_quantities.R")

MEANPREC <- 3
PVALPRECISION <- 4

mannWhitneyUTest <- function(variables, condition, group, route) {


  output <- droplevels(variables[variables$condition == condition,])
  variables_ <- droplevels(output[output$group == group,])

  # Otherwise, if both x and y are given and paired is FALSE, a Wilcoxon rank sum test
  # (equivalent to the Mann-Whitney test: see the Note) is carried out.
  # In this case, the null hypothesis is that the distributions of x and y differ by a
  # location shift of mu and the alternative is that they differ by some other location shift
  # (and the one-sided alternative "greater" is that x is shifted to the right of y).
  # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/wilcox.test
  mann_whit_results <- wilcox.test(eval(as.symbol(route)) ~ Informed, data = variables_, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)

  UnInformed <- droplevels(variables_[variables_$Informed == "PriorToInformation",])
  Informed <- droplevels(variables_[variables_$Informed == "InformationProvided",])

  mann_whit_results$meanUninformed <- round(mean(UnInformed[, c(route)]), MEANPREC)
  mann_whit_results$meanInformed <- round(mean(Informed[, c(route)]), MEANPREC)

  return(mann_whit_results)
}


# extract variables from data
survey_results <- get_survey_results("data/Table-S6-Survey-Raw-data.xlsx", transform_likert = TRUE)
route_attractiveness <- get_route_attractiveness_long_format(survey_results)


# 1 normality check
route_attractiveness %>%
  group_by(Informed, group, condition) %>%
  shapiro_test(RouteAttractivenessLong)
route_attractiveness %>%
  group_by(Informed, group, condition) %>%
  shapiro_test(RouteAttractivenessMedium)
route_attractiveness %>%
  group_by(Informed, group, condition) %>%
  shapiro_test(RouteAttractivenessShort)
# none of the attractivenesses follows a normal distribution


df <- data.frame(Group = c(),
                 Route = c(),
                 Condition = c(),
                 pValue = c(),
                 W = c(),
                 isDifferent = c(),
                 meanValUnInformed = c(),
                 meanValInformed = c())

# check whether information changes the route attractivenss (for each group and each condition)

for (group_ in unique(route_attractiveness$group)) {
  for (route_ in c("RouteAttractivenessLong", "RouteAttractivenessMedium", "RouteAttractivenessShort")) {
    for (cond_ in unique(route_attractiveness$condition)) {
      m1 <- mannWhitneyUTest(route_attractiveness, condition = cond_, group = group_, route = route_)
      val <- m1$p.value
      W <- m1$statistic[[1]]

      df_ <- data.frame(Group = c(group_),
                        Route = c(route_),
                        Condition = c(cond_),
                        pValue = c(round(val, PVALPRECISION)),
                        W = round(W, 1),
                        meanValUnInformed = m1$meanUninformed,
                        meanValInformed = m1$meanInformed
      )
      df <- rbind(df, df_)


    }
  }
}

print(xtable(df,
             type = "latex",
             digits = PVALPRECISION), floating = FALSE,
      file = "output/effect_of_information_on_route_attractiveness.tex",
      include.rownames = FALSE)


print("Export finished.")
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
library(xtable)

set.seed(1234)

alpha <- 0.05
source("survey_results/src/read_data.R")
source("survey_results/src/constants.R")
source("survey_results/src/derived_quantities.R")

condition_short <- c("B1", "B2", "B3", "B4", "A1", "A2", "A3", "A4")
condition_long <- c("Arrow",
                    "Arrow + top down view",
                    "Arrow + team spirit ",
                    "Arrow + top down view + team spirit",
                    "Congestion info + arrow",
                    "Congestion info + arrow + top down view",
                    "Congestion info + arrow + team spirit",
                    "Congestion info + arrow + top down view + team spirit"
)

kruskal_wallis <- function(variables, dep_var_name, indep_var_name, order = NULL) {

  if (is.null(order) == FALSE) {
    variables <- variables %>% reorder_levels(indep_var_name, order = order)
  }

  res <- variables %>% kruskal_test(eval(as.symbol(dep_var_name)) ~ eval(as.symbol(indep_var_name)))
  stats <- variables %>%
    group_by(eval(as.symbol(indep_var_name))) %>%
    get_summary_stats(dep_var_name, type = "full" ) #"common"
  names(stats)[names(stats) == 'eval(as.symbol(indep_var_name))'] <- "Condition"

  stats <- cbind(pValue = res$p, KruskalWallisChiSquared = res$statistic, DegreeOfFreedom =  res$df, stats )

  stats$meanrank <- -111

  rankSummary <- data.frame()

  for (condition in stats$Condition) {
     routeAttrativeness <- droplevels(variables[variables$condition == condition,])
     routeAttrativeness <- routeAttrativeness %>% mutate(rank = rank(eval(as.symbol(dep_var_name))))
     routeAttrativeness <- routeAttrativeness[,c(dep_var_name,"rank")]

     routeAttrativeness$Condition <- condition
     routeAttrativeness$n <- length(routeAttrativeness$rank)
     routeAttrativeness$meanrank <- mean(routeAttrativeness$rank)

     rankSummary<-rbind(rankSummary,routeAttrativeness)

     stats[stats$Condition == condition, "meanrank"] <- mean(routeAttrativeness$rank)
  }


  write_xlsx(rankSummary, paste("survey_results/output/MeanRank_", dep_var_name, ".xlsx"))

  print(stats)

  return(stats)
}


write_mean_and_kruskal_wallis <- function(informed = FALSE) {

  likelihood_routes <- c("RouteA", "RouteB", "RouteC")
  conds <- c("condition")
  likelihoods <- data_frame()

  ## students
  variablesstudents <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = (1:500))
  variablesstudents <- get_4x2(variablesstudents, informed = informed)
  for (route_ in likelihood_routes) {
    for (ind_var in conds) {
      df_ <- kruskal_wallis(variables = variablesstudents,
                            dep_var_name = route_,
                            indep_var_name = ind_var)
      df_ <- cbind(Group = "Students", df_)
      likelihoods <- rbind(likelihoods, df_)
    }
  }

  ## football fans
  variablesfans <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = -(1:500))
  variablesfans <- get_4x2(variablesfans, informed = informed)
  for (route_ in likelihood_routes) {
    for (ind_var in conds) {
      df_ <- kruskal_wallis(variables = variablesfans,
                            dep_var_name = route_,
                            indep_var_name = ind_var)
      df_ <- cbind(Group = "Fans", df_)
      likelihoods <- rbind(likelihoods, df_)
    }
  }


  likelihoods$Condition <- plyr::mapvalues(likelihoods$Condition, from = condition_short, to = condition_long)
  routes <- plyr::mapvalues(likelihoods$variable, from = c("RouteA", "RouteB", "RouteC"), to = c("Long", "Medium", "Short"))

  likelihoods <- cbind(Route = routes, likelihoods)
  likelihoods <- cbind(likelihoods, SampleSize = likelihoods$n)


  kruskal_wallis <- unique(likelihoods[c("Route", "Group", "pValue", "KruskalWallisChiSquared", "DegreeOfFreedom")])
  filename <- "survey_results/final_table/KruskalWallisInformed"
  print(xtable(kruskal_wallis, type = "latex", digits = 4), floating = FALSE, file = paste(filename, informed, ".tex", sep = ""), include.rownames = FALSE)


  stats <- likelihoods[c("Route", "Group", "Condition", "mean", "median", "sd", "SampleSize")]
  filename <- "survey_results/final_table/StatsInformed"
  print(xtable(stats, type = "latex", digits = 2), floating = FALSE, file = paste(filename, informed, ".tex", sep = ""), include.rownames = FALSE)
  return(stats)
}

write_lumped_mean <- function(informed = FALSE) {
  fans <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = -(1:500))
  fans <- get_4x2(fans, informed)
  fansA <- fans %>% get_summary_stats("RouteA", type = "common")
  fansB <- fans %>% get_summary_stats("RouteB", type = "common")
  fansC <- fans %>% get_summary_stats("RouteC", type = "common")
  fansA$Group <- "Fans"
  fansB$Group <- "Fans"
  fansC$Group <- "Fans"

  students <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = (1:500))
  students <- get_4x2(students, informed)
  studentsA <- students %>% get_summary_stats("RouteA", type = "common")
  studentsB <- students %>% get_summary_stats("RouteB", type = "common")
  studentsC <- students %>% get_summary_stats("RouteC", type = "common")
  studentsA$Group <- "Students"
  studentsB$Group <- "Students"
  studentsC$Group <- "Students"

  meansInformed <- rbind(fansA, fansB, fansC, studentsA, studentsB, studentsC)
  return(meansInformed)


}


stats <- write_mean_and_kruskal_wallis(informed = TRUE)
ggplot(stats, aes(x = Condition, y = mean, shape = Group, color = Group)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(.5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), strip.text = element_text(color = "black")) +
  facet_wrap(~Route, nrow = 1) +
  xlab("") +
  ylab("Route attractiveness \n (5-Point Likert scale)")
ggsave("survey_results/figs/RouteLikelihoodsInformed.pdf", width = 25, height = 16, units = "cm")
#dev.off()

stats <- write_mean_and_kruskal_wallis(informed = FALSE)
ggplot(stats, aes(x = Condition, y = mean, shape = Group, color = Group)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(.5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), strip.text = element_text(color = "black")) +
  facet_wrap(~Route, nrow = 1) +
  xlab("") +
  ylab("Route attractiveness \n (5-Point Likert scale)") +

ggsave("survey_results/figs/RouteLikelihoodsUnInformed.pdf", width = 25, height = 16, units = "cm")
#dev.off()

# compare absolute values
variablesstudents <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = (1:500))
variablesstudents <- get_4x2_uninformed(variablesstudents)
variablesstudents$condition <- paste(variablesstudents$condition, "Students", sep = "-")

variablesfans <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = -(1:500))
variablesfans <- get_4x2_uninformed(variablesfans)
variablesfans$condition <- paste(variablesfans$condition, "Fans", sep = "-")

variables <- rbind(variablesfans, variablesstudents)

likelihood_routes <- c("RouteA", "RouteB", "RouteC")
conds <- c("condition")
likelihoods <- data_frame()

for (route_ in likelihood_routes) {
  for (ind_var in conds) {
    df_ <- kruskal_wallis(variables = variables,
                          dep_var_name = route_,
                          indep_var_name = ind_var)
    likelihoods <- rbind(likelihoods, df_)
  }
}

likelihoods <- unique(likelihoods[, c("variable", "pValue")])
likelihoods$variable <- plyr::mapvalues(likelihoods$variable, from = c("RouteA", "RouteB", "RouteC"), to = c("Long", "Medium", "Short"))
likelihoods <- likelihoods %>% pivot_wider(names_from = c("variable"), values_from = pValue)

filename <- "survey_results/final_table/KruskalWallisConditionsAndGroupsUninformed.tex"
print(xtable(likelihoods, type = "latex", digits = 4), floating = FALSE, file = filename, include.rownames = FALSE)


# medium route: different behavior for fans and football fans.
dunntest.routeA <- FSA::dunnTest(variables$RouteA ~ condition, data = variables, method = "bh")
dunntest.routeB <- FSA::dunnTest(variables$RouteB ~ condition, data = variables)
routeB <- dunntest.routeB$res
routeB <- routeB[order(routeB$P.unadj),]

a <- transpose(str_split(routeB$Comparison, "-"))

routeB$Condition1 <- a[[1]]
routeB$Group1 <- a[[2]]
routeB$Condition2 <- a[[3]]
routeB$Group2 <- a[[4]]

routeB <- routeB[, c("Condition1", "Group1", "Condition2", "Group2", "P.unadj", "P.adj")]

filename <- "survey_results/final_table/MediumRouteDunnsTestUninformedBahavior.tex"
print(xtable(routeB, type = "latex", digits = 4), floating = FALSE, file = filename, include.rownames = FALSE)


# compare absolute values

variables <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = -(1:500))
variables <- get_4x2_informed(variables)
variables <- variables[variables$condition == "A2" |
                         variables$condition == "A3" |
                         variables$condition == "A4",]
variables$condition <- plyr::mapvalues(variables$condition, from = condition_short, to = condition_long)


kk <- kruskal_wallis(variables, "RouteA", "condition")
filename <- "survey_results/final_table/pValueInformedFansA2A3A4.tex"
print(xtable(kk, type = "latex", digits = 4), floating = FALSE, file = filename, include.rownames = FALSE)


uninformed <- write_lumped_mean(informed = FALSE)
uninformed$InfoState <- "UnInformed"

informed <- write_lumped_mean(informed = TRUE)
informed$InfoState <- "Informed"


means <- rbind(uninformed, informed)


filename <- "survey_results/final_table/LongRouteLumped.tex"
print(xtable(means, type = "latex", digits = 4), floating = FALSE, file = filename, include.rownames = FALSE)
print("")
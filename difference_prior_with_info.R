library(Hmisc)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(writexl)
library(xtable)
library(crank)

source("src/read_data.R")
source("src/constants.R")

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

  mann_whit_results$meanUninformed <- round(mean(UnInformed[, c(route)]), PRECISION_VAR)
  mann_whit_results$meanInformed <- round(mean(Informed[, c(route)]), PRECISION_VAR)

  return(mann_whit_results)
}

print(" -- Script started - Compute route choice prior to information vs. info provided --")

# 1 read data
path_to_survey_file <-  file.path(getwd(), "data", "Table S1.xlsx") # see sub-dir data
survey_results <- get_survey_results(path_to_survey_file, transform_likert = TRUE)
route_attractiveness <- get_route_attractiveness_long_format(survey_results)

route_attractiveness_L <- pivot_longer(route_attractiveness,
                                       -c(Informed, group, condition, id),
                                       values_to = "Attractiveness",
                                       names_to = "Route",
                                       names_prefix = "RouteAttractiveness")


# 2 normality check
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

# 3 use Mann Whitney U tests to investiage whether information has an effect
mannWhitneyUResultsInfoHasEffect <- data.frame(Group = c(),
                                               Route = c(),
                                               Condition = c(),
                                               pValue = c(),
                                               W = c(),
                                               isDifferent = c(),
                                               meanValUnInformed = c(),
                                               meanValInformed = c())


for (group_ in unique(route_attractiveness$group)) {
  for (route_ in c("RouteAttractivenessLong", "RouteAttractivenessMedium", "RouteAttractivenessShort")) {
    for (cond_ in unique(route_attractiveness$condition)) {
      m1 <- mannWhitneyUTest(route_attractiveness, condition = cond_, group = group_, route = route_)
      val <- m1$p.value
      W <- m1$statistic[[1]]
      df_ <- data.frame(Group = c(group_),
                        Route = c(route_),
                        Condition = c(cond_),
                        meanValUnInformed = m1$meanUninformed,
                        meanValInformed = m1$meanInformed,
                        pValue = c(round(val, PRECISION_PVAL)),
                        W = round(W, PRECISION_TEST_STAT)
      )
      mannWhitneyUResultsInfoHasEffect <- rbind(mannWhitneyUResultsInfoHasEffect, df_)
    }
  }
}

# 3 compute statistics for visual comparison

statisticsRouteAttractiveness <- subset(route_attractiveness, select = -c(id))  %>% group_by(Informed, group, condition) %>% get_summary_stats(type = "full")
statisticsRouteAttractiveness$variable <- plyr::mapvalues(statisticsRouteAttractiveness$variable,
                                                          from = c("RouteAttractivenessLong", "RouteAttractivenessMedium", "RouteAttractivenessShort"),
                                                          to = c("Long", "Medium", "Short"))

statisticsRouteAttractiveness <- statisticsRouteAttractiveness %>% dplyr::rename("Route" = "variable", "Condition" = "condition", "Group" = "group")
RouteAttractivenessStats.uninformed <- subset(statisticsRouteAttractiveness, Informed == "PriorToInformation")
RouteAttractivenessStats.informed <- subset(statisticsRouteAttractiveness, Informed == "InformationProvided")

# 4 export results
print("Start export ...")

# table
filenametable <- file.path(getwd(), "output", "supplements_table_S4_S5.tex")
print(xtable(mannWhitneyUResultsInfoHasEffect,
             type = "latex",
             digits = PRECISION_PVAL), floating = FALSE,
      file = filenametable,
      include.rownames = FALSE)

print(filenametable)

# figure
filenamefigure1 <- file.path(getwd(), "output", "manuscript_Figure_4-part1.pdf")
ggplot2::ggplot(RouteAttractivenessStats.uninformed, aes(x = Condition, y = mean, shape = Group, color = Group)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(.5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), strip.text = element_text(color = "black")) +
  facet_wrap(~Route, nrow = 1) +
  xlab("") +
  ylab("Route attractiveness \n (5-Point Likert scale)")

ggsave(filenamefigure1, width = 25, height = 16, units = "cm")
print(filenamefigure1)

# figure
filenamefigure2 <- file.path(getwd(), "output", "manuscript_Figure_4-part2.pdf")
ggplot2::ggplot(RouteAttractivenessStats.informed, aes(x = Condition, y = mean, shape = Group, color = Group)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(.5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), strip.text = element_text(color = "black")) +
  facet_wrap(~Route, nrow = 1) +
  xlab("") +
  ylab("Route attractiveness \n (5-Point Likert scale)")

ggsave(filenamefigure2, width = 25, height = 16, units = "cm")
print(filenamefigure2)

print("... export finished.")

print(" -------------------- Script finished -------------------------")

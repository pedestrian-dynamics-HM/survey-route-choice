# Title     : Plot results of the survey
# Objective : Get a first impression of the effect of communication strategies
# Created by: Christina Mayr
# Created on: 11.01.22
library(ggplot2)
library(Hmisc)
library(hrbrthemes)
library(viridis)


source("survey_results/src/read_data.R")
source("survey_results/src/constants.R")
source("survey_results/src/derived_quantities.R")

variables <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv",transform_likert = TRUE)
variables_numeric <- variables[, get_likert_scaled_variable_names()] # exclude meta info
variables_numeric <- variables_numeric %>% dplyr::select(-starts_with("Route", ignore.case = TRUE))

png(filename = "survey_results/figs/dependent_variables_1.png", width = 1000, height = 1000, res = 100)
hist.data.frame(variables_numeric[, 1:12])
dev.off()
#
png(filename = "survey_results/figs/dependent_variables_2.png", width = 1000, height = 1000, res = 100)
hist.data.frame(variables_numeric[, 13:23])
dev.off()

png(filename = "survey_results/figs/Age.png")
barplot(table(get_within_factors(variables)$VarAge), main = "Age")
dev.off()

png(filename = "survey_results/figs/Condition.png")
barplot(table(get_within_factors(variables)$condition), main = "Condition")
dev.off()

png(filename = "survey_results/figs/Gender.png", width = 500, height = 300)
barplot(table(get_within_factors(variables)$VarGender), main = "Gender")
dev.off()

png(filename = "survey_results/figs/Device.png", width = 500, height = 300)
barplot(table(get_within_factors(variables)$VarDevice), main = "Device")
dev.off()

# Groupwise comparison
title <- "Likelihood: route (A) (long) - groupwise"
png(filename = sprintf("survey_results/figs/%s.png", title), width = 1000, height = 600)
ggplot(variables, aes(fill = condition, x = RouteA)) +
  geom_bar(position = "stack") +
  ggtitle(title) +
  facet_wrap(~Informed) +
  theme_ipsum() +
  xlab("")
dev.off()

title <- "Likelihood: route (B) - groupwise"
png(filename = sprintf("survey_results/figs/%s.png", title), width = 1000, height = 600)
ggplot(variables, aes(fill = condition, x = RouteB)) +
  geom_bar(position = "stack") +
  ggtitle(title) +
  facet_wrap(~Informed) +
  theme_ipsum() +
  xlab("")
dev.off()

title <- "Likelihood: route (C) (short) - groupwise"
png(filename = sprintf("survey_results/figs/%s.png", title), width = 1000, height = 600)
ggplot(variables, aes(fill = condition, x = RouteC)) +
  geom_bar(position = "stack") +
  ggtitle(title) +
  facet_wrap(~Informed) +
  theme_ipsum() +
  xlab("")
dev.off()


# Shortest route A
title <- "Likelihood: route (A) (long) not Informed"
png(filename = sprintf("survey_results/figs/%s.png", title), width = 1000, height = 600)
ggplot(variables[variables$Informed == "NotInformed",], aes(fill = Informed, x = RouteA)) +
  geom_bar(position = "dodge", fill = "blue") +
  ggtitle(title) +
  facet_wrap(~condition, labeller = labeller(condition = get_conditions_overview()), nrow = 2) +
  theme_ipsum() +
  xlab("")
dev.off()

title <- "Likelihood: route (A) (long) informed"
png(filename = sprintf("survey_results/figs/%s.png", title), width = 1000, height = 600)
ggplot(variables[variables$Informed == "Informed",], aes(fill = Informed, x = RouteA)) +
  geom_bar(position = "dodge") +
  ggtitle(title) +
  facet_wrap(~condition, labeller = labeller(condition = get_conditions_overview()), nrow = 2) +
  theme_ipsum() +
  xlab("")
dev.off()

# Shortest route B
title <- "Likelihood: route (B) not Informed"
png(filename = sprintf("survey_results/figs/%s.png", title), width = 1000, height = 600)
ggplot(variables[variables$Informed == "NotInformed",], aes(fill = Informed, x = RouteB)) +
  geom_bar(position = "dodge", fill = "blue") +
  ggtitle(title) +
  facet_wrap(~condition, labeller = labeller(condition = get_conditions_overview()), nrow = 2) +
  theme_ipsum() +
  xlab("")
dev.off()

title <- "Likelihood: route (B) informed"
png(filename = sprintf("survey_results/figs/%s.png", title), width = 1000, height = 600)
ggplot(variables[variables$Informed == "Informed",], aes(fill = Informed, x = RouteB)) +
  geom_bar(position = "dodge") +
  ggtitle(title) +
  facet_wrap(~condition, labeller = labeller(condition = get_conditions_overview()), nrow = 2) +
  theme_ipsum() +
  xlab("")
dev.off()


# Shortest route C
title <- "Likelihood: route (C) (short) not Informed"
png(filename = sprintf("survey_results/figs/%s.png", title), width = 1000, height = 600)
ggplot(variables[variables$Informed == "NotInformed",], aes(fill = Informed, x = RouteC)) +
  geom_bar(position = "dodge", fill = "blue") +
  ggtitle(title) +
  facet_wrap(~condition, labeller = labeller(condition = get_conditions_overview()), nrow = 2) +
  theme_ipsum() +
  xlab("")
dev.off()

title <- "Likelihood: route (C) (short) informed"
png(filename = sprintf("survey_results/figs/%s.png", title), width = 1000, height = 600)


ggplot(variables[variables$Informed == "Informed",], aes(fill = Informed, x = RouteC)) +
  geom_bar(position = "dodge") +
  ggtitle(title) +
  facet_wrap(~condition, labeller = labeller(condition = get_conditions_overview()), nrow = 2) +
  theme_ipsum() +
  xlab("")
dev.off()

# actual route choice

png(filename = "survey_results/figs/RouteChoiceShortestPreferred.png", width = 1000, height = 600)
ggplot(variables, aes(fill = Informed, x = fav_routes.shortest_pref)) +
  geom_bar(position = "dodge") +
  ggtitle("Route choice (shortest preferred)") +
  facet_wrap(~condition, labeller = labeller(condition = get_conditions_overview()), nrow = 2) +
  theme_ipsum() +
  xlab("")
dev.off()

png(filename = "survey_results/figs/RouteChoiceSLongestPrefered.png", width = 1000, height = 600)
ggplot(variables, aes(fill = Informed, x = fav_routes.longest_pref)) +
  geom_bar(position = "dodge") +
  ggtitle("Route choice (longest preferred)") +
  facet_wrap(~condition, labeller = labeller(condition = get_conditions_overview()), nrow = 2) +
  theme_ipsum() +
  xlab("")
dev.off()

png(filename = "survey_results/figs/RouteChoices.png", width = 1600, height = 1000, res = 150)
ggplot(variables, aes(fill = Informed, x = fav_routes.choice)) +
  geom_bar(position = "dodge") +
  ggtitle("Route choice") +
  facet_wrap(~condition, labeller = labeller(condition = get_conditions_overview()), nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("")
dev.off()

print("Plots succesfully exported.")

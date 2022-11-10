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
library(gmodels)
library(DescTools)
library(dplyr)
library(writexl)
library(xtable)

set.seed(1234)

alpha <- 0.05
source("survey_results/src/read_data.R")
source("survey_results/src/constants.R")
source("survey_results/src/derived_quantities.R")
source("survey_results/src/statTests.R")
source("survey_results/final_table_export_1.R")




# compare absolute values
variablesstudents <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = (1:500))
variablesstudents <- get_4x2_uninformed(variablesstudents)
variablesstudents$group <- "students"

variablesfans <- get_survey_results("survey_results/data/results-survey4-20220301_abbr.csv", transform_likert = TRUE, subpopulation = -(1:500))
variablesfans <- get_4x2_uninformed(variablesfans)
variablesfans$group <- "fans"

variables <- rbind(variablesfans, variablesstudents)

gender <- variables %>% dplyr::count(group, VarGender) %>% pivot_wider(names_from = VarGender, values_from = n)
cols <- c("Female", "Male", "Prefer not to answer", "See my comment (please provide a comment)" )
gender$n <- rowSums(gender[,cols], dims = 1, na.rm = TRUE)
gender[,cols] <- gender[,cols] / gender$n

filename <- "survey_results/final_table/gender.tex"
print(xtable(gender, type = "latex", digits = 2), floating = FALSE, file = filename, include.rownames = FALSE)
print("finished")


age <- variables %>% dplyr::count(group, VarAge) %>% pivot_wider(names_from = VarAge, values_from = n) %>% relocate('>65', .after = last_col())
cols <- c("<18", "18-25", "26-35", "36-50", "51-65", ">65")
age$n <- rowSums(age[,cols], dims = 1, na.rm = TRUE)
age[,cols] <- age[,cols] / age$n

filename <- "survey_results/final_table/age.tex"
print(xtable(age, type = "latex", digits = 2), floating = FALSE, file = filename, include.rownames = FALSE)
print("finished")







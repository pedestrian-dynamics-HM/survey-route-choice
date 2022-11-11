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
source("src/read_data.R")
source("src/constants.R")



survey_results <- get_survey_results("data/Table-S6-Survey-Raw-data.xlsx" )

# gender distribution
gender <- survey_results %>% dplyr::count(group, VarGender) %>% pivot_wider(names_from = VarGender, values_from = n)
cols <- c("Female", "Male", "Prefer not to answer", "See my comment (please provide a comment)" )
gender$n <- rowSums(gender[,cols], dims = 1, na.rm = TRUE)
gender[,cols] <- gender[,cols] / gender$n
print(xtable(gender, type = "latex", digits = 2), floating = FALSE, file = "output/gender.tex", include.rownames = FALSE)

# age distribution
age <- survey_results %>% dplyr::count(group, VarAge) %>% pivot_wider(names_from = VarAge, values_from = n) %>% relocate('>65', .after = last_col())
cols <- c("<18", "18-25", "26-35", "36-50", "51-65", ">65")
age$n <- rowSums(age[,cols], dims = 1, na.rm = TRUE)
age[,cols] <- age[,cols] / age$n
print(xtable(age, type = "latex", digits = 2), floating = FALSE, file = "output/table_1_age.tex", include.rownames = FALSE)


# dropouts
survey_results_all <- get_survey_results("data/Table-S6-Survey-Raw-data.xlsx", completed_only = FALSE)
dropoutrate <- nrow(survey_results)/nrow(survey_results_all)

dropped <- subset(survey_results_all, lastpage < 9)
percentageDroppedBeforePage3 <- nrow(subset(dropped, lastpage < 3))/ nrow(dropped)

dropoutAnalysis <- data_frame( finished = nrow(survey_results),
                               total = nrow(survey_results_all),
                               dropoutrate = dropoutrate,
                               info = "A0 excluded from analysis",
                               percentageDroppedBeforePage3 = percentageDroppedBeforePage3
)
print(xtable(dropoutAnalysis, type = "latex", digits = 4), floating = FALSE, file = "output/dropouts.tex", include.rownames = FALSE)

print("Export finished.")







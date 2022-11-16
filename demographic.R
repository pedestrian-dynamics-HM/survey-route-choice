
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


source("src/read_data.R")
source("src/constants.R")

print(" ---- Script started - Analyze gender, age and dropout rate ----")

# read data

path_to_survey_file <-  file.path(getwd(), "data", "Table S1.xlsx") # see sub-dir data
survey_results <- get_survey_results(path_to_survey_file) # excluding incomplete samples
survey_results_all <- get_survey_results(path_to_survey_file, completed_only = FALSE) # include incomplete samples


# 1 gender distribution (only completed surveys are considered)
gender <- survey_results %>% dplyr::count(group, VarGender) %>% pivot_wider(names_from = VarGender, values_from = n)
cols <- c("Female", "Male", "Prefer not to answer", "See my comment (please provide a comment)" )
gender$n <- rowSums(gender[,cols], dims = 1, na.rm = TRUE)
gender[,cols] <- gender[,cols] / gender$n

# 2 age distribution (only completed surveys are considered)
age <- survey_results %>% dplyr::count(group, VarAge) %>% pivot_wider(names_from = VarAge, values_from = n) %>% relocate('>65', .after = last_col())
cols <- c("<18", "18-25", "26-35", "36-50", "51-65", ">65")
age$n <- rowSums(age[,cols], dims = 1, na.rm = TRUE)
age[,cols] <- age[,cols] / age$n

# dropouts
dropoutrate <- nrow(survey_results)/nrow(survey_results_all)

dropped <- subset(survey_results_all, lastpage < 9)
percentageDroppedBeforePage3 <- nrow(subset(dropped, lastpage < 3))/ nrow(dropped)

dropoutAnalysis <- data_frame( finished = nrow(survey_results),
                               total = nrow(survey_results_all),
                               dropoutrate = dropoutrate,
                               info = "A0 excluded from analysis",
                               percentageDroppedBeforePage3 = percentageDroppedBeforePage3
)

# write results

print("Start export ...")

filename <- file.path(getwd(), "output", "manuscript_table_1.tex")
print(xtable(age, type = "latex", digits = PRECISION_PVAL), floating = FALSE, file = filename, include.rownames = FALSE)
print(filename)

filename <- file.path(getwd(), "output", "manuscript__dropouts.tex")
print(xtable(dropoutAnalysis, type = "latex", digits = PRECISION_PVAL), floating = FALSE, file = filename, include.rownames = FALSE)
print(filename)

filename <- file.path(getwd(), "output", "manuscript__gender.tex")
print(xtable(gender, type = "latex", digits = PRECISION_PVAL), floating = FALSE, file = filename, include.rownames = FALSE)
print(filename)


print("... export finished.")

print(" -------------------- Script finished -------------------------")


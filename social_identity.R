# Title     : Shared social identity and motivation
# Created by: Christina Mayr
# Created on: 29th June, 2022

library(ggplot2)
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


# extract variables from data
variables <- get_survey_results("data/Table-S6-Survey-Raw-data.xlsx", transform_likert = TRUE)

# 1 normality
## check for each condition whether data is normality distributed or not
## if not normality distributed: use kruskal wallis test in next step

normal_test.1 <- variables %>% group_by(group, condition) %>% shapiro_test(VarSocialNorms.SupportFans.)
normal_test.2 <- variables %>% group_by(group, condition) %>% shapiro_test(VarSocialNorms.SupportTeam.)
normal_test.3 <- variables %>% group_by(group, condition) %>% shapiro_test(VarFaith.Motivation.)
normal_test.4 <- variables %>% group_by(group, condition) %>% shapiro_test(VarFaith.SharedIdentity.)

normal_test <- rbind(normal_test.1, normal_test.2, normal_test.3, normal_test.4)
normal_test <- normal_test %>% dplyr::select(-c("statistic"))
normal_test <- normal_test %>% pivot_wider(names_from = c("variable") , values_from = c("p"))
normal_test <- round_df(normal_test)
print(normal_test)

# 2 make sure that in between conditions behavior is equal
## is difference between conditions?
fans <- subset(variables, group == "Fan")
res.fans.fans <- fans %>% kruskal_test(VarSocialNorms.SupportFans. ~ condition)
res.fans.team <- fans %>% kruskal_test(VarSocialNorms.SupportTeam. ~ condition)
res.fans.motivation <- fans %>% kruskal_test(VarFaith.Motivation. ~ condition)
res.fans.sharedid <- fans %>% kruskal_test(VarFaith.SharedIdentity. ~ condition)

students <- subset(variables, group == "Student & faculty associate")
res.students.fans <- students %>% kruskal_test(VarSocialNorms.SupportFans. ~ condition)
res.students.team <- students %>% kruskal_test(VarSocialNorms.SupportTeam. ~ condition)
res.students.motivation <- students %>% kruskal_test(VarFaith.Motivation. ~ condition)
res.students.sharedid <- students %>% kruskal_test(VarFaith.SharedIdentity. ~ condition)


kruskal_wallis_test <- data.frame(group = c("students", "fans"),
                                  suppertFans = c(res.students.fans$p, res.fans.fans$p),
                                  supportTeam = c(res.students.team$p, res.fans.team$p),
                                  motivation = c(res.students.motivation$p, res.fans.motivation$p),
                                  sharedId = c(res.students.sharedid$p, res.fans.sharedid$p))
kruskal_wallis_test <- round_df(kruskal_wallis_test)
print(kruskal_wallis_test)


# 3 compare social identity related variables between students and football fans
# we lump the conditions, because there is no difference (see results of step 2)

variables <- variables %>% dplyr::select(c("group", "condition", "VarSocialNorms.SupportFans.", "VarSocialNorms.SupportTeam.", "VarFaith.SharedIdentity." ))
var_stats <- variables %>% group_by(group) %>% get_summary_stats(type = "full")
var_stats <- var_stats %>% dplyr::select(c("group", "variable", "mean", "sd"))
var_stats <- var_stats %>% pivot_wider(names_from = c("group") , values_from = c("mean", "sd"))

var_stats$p <- -1
res.sf <- wilcox.test(VarSocialNorms.SupportFans. ~ group, data = variables, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)
res.st<- wilcox.test(VarSocialNorms.SupportTeam. ~ group, data = variables, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)
res.id <- wilcox.test(VarFaith.SharedIdentity. ~ group, data = variables, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)


var_stats$p[var_stats$variable == "VarSocialNorms.SupportFans."] <- res.sf$p.value
var_stats$p[var_stats$variable == "VarSocialNorms.SupportTeam."] <- res.st$p.value
var_stats$p[var_stats$variable == "VarFaith.SharedIdentity."] <- res.id$p.value
var_stats$W[var_stats$variable == "VarSocialNorms.SupportFans."] <- res.sf$statistic[[1]]
var_stats$W[var_stats$variable == "VarSocialNorms.SupportTeam."] <- res.st$statistic[[1]]
var_stats$W[var_stats$variable == "VarFaith.SharedIdentity."] <- res.id$statistic[[1]]

var_stats <- round_df(var_stats)
print(var_stats)


# 4 Write data
## social identity
DIGITS <- 4
print(xtable(var_stats, type = "latex", digits=DIGITS), floating = FALSE, file = "output/SocialIdentityVarsDifferenceFansStudents.tex", include.rownames=FALSE)
print(xtable(kruskal_wallis_test, type = "latex", digits=DIGITS), floating = FALSE, file = "output/SocialIdentityVarsKruskalWallisConditions.tex", include.rownames=FALSE)

print("Export finished.")
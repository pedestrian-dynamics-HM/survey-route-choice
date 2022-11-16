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

source("src/read_data.R")
source("src/constants.R")

print(" ---- Script started - Analyze level of social identification ----")


# extract variables from data
path_to_survey_file <-  file.path(getwd(), "data", "Table-S6-Survey-Raw-data.xlsx") # see sub-dir data
variables <- get_survey_results(path_to_survey_file, transform_likert = TRUE)

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



# 3 compare social identity related variables between students and football fans
# we lump the conditions, because there is no difference (see results of step 2)

variables <- variables %>% dplyr::select(c("group", "condition", "VarSocialNorms.SupportFans.", "VarSocialNorms.SupportTeam.", "VarFaith.SharedIdentity." ))
var_stats <- variables %>% group_by(group) %>% get_summary_stats(type = "full")
var_stats <- var_stats %>% dplyr::select(c("group", "variable", "mean", "sd"))
var_stats <- var_stats %>% pivot_wider(names_from = c("group") , values_from = c("mean", "sd"))

var_stats$p <- -1 # initalize
var_stats$W <- -1 # initalize
res.sf <- wilcox.test(VarSocialNorms.SupportFans. ~ group, data = variables, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)
res.st<- wilcox.test(VarSocialNorms.SupportTeam. ~ group, data = variables, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)
res.id <- wilcox.test(VarFaith.SharedIdentity. ~ group, data = variables, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)


var_stats$p[var_stats$variable == "VarSocialNorms.SupportFans."] <- res.sf$p.value
var_stats$p[var_stats$variable == "VarSocialNorms.SupportTeam."] <- res.st$p.value
var_stats$p[var_stats$variable == "VarFaith.SharedIdentity."] <- res.id$p.value
var_stats$W[var_stats$variable == "VarSocialNorms.SupportFans."] <- res.sf$statistic[[1]]
var_stats$W[var_stats$variable == "VarSocialNorms.SupportTeam."] <- res.st$statistic[[1]]
var_stats$W[var_stats$variable == "VarFaith.SharedIdentity."] <- res.id$statistic[[1]]

# 4 Write data
print("Start export ...")
filename <- file.path(getwd(), "output", "manuscript__social_identity_salient_fans.tex")
print(xtable(var_stats, type = "latex", digits=PRECISION_PVAL), floating = FALSE, file = filename, include.rownames=FALSE)
print(filename)


print("... export finished.")

print(" -------------------- Script finished -------------------------")

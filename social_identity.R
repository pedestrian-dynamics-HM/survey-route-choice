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
path_to_survey_file <-  file.path(getwd(), "data", "Table S1.xlsx") # see sub-dir data
survey_results <- get_survey_results(path_to_survey_file, transform_likert = TRUE)

# 1 normality
## check for each condition whether data is normality distributed or not
## if not normality distributed: use kruskal wallis test in next step

normal_test.1 <- survey_results %>% group_by(group, condition) %>% shapiro_test(VarSocialNorms.SupportFans.)
normal_test.2 <- survey_results %>% group_by(group, condition) %>% shapiro_test(VarSocialNorms.SupportTeam.)
normal_test.3 <- survey_results %>% group_by(group, condition) %>% shapiro_test(VarFaith.Motivation.)
normal_test.4 <- survey_results %>% group_by(group, condition) %>% shapiro_test(VarFaith.SharedIdentity.)

normal_test <- rbind(normal_test.1, normal_test.2, normal_test.3, normal_test.4)
normal_test <- normal_test %>% dplyr::select(-c("statistic"))
normal_test <- normal_test %>% pivot_wider(names_from = c("variable") , values_from = c("p"))


# 2 make sure that in between conditions behavior is equal
## is difference between conditions?
fans <- subset(survey_results, group == "Fan")
res.fans.fans <- fans %>% kruskal_test(VarSocialNorms.SupportFans. ~ condition)
res.fans.team <- fans %>% kruskal_test(VarSocialNorms.SupportTeam. ~ condition)
res.fans.motivation <- fans %>% kruskal_test(VarFaith.Motivation. ~ condition)
res.fans.sharedid <- fans %>% kruskal_test(VarFaith.SharedIdentity. ~ condition)

students <- subset(survey_results, group == "Student & faculty associate")
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

survey_results <- survey_results %>% dplyr::select(c("group", "condition", "VarSocialNorms.SupportFans.", "VarSocialNorms.SupportTeam.", "VarFaith.SharedIdentity." ))
socialIdentity_var_statistics <- survey_results %>% group_by(group) %>% get_summary_stats(type = "full")
socialIdentity_var_statistics <- socialIdentity_var_statistics %>% dplyr::select(c("group", "variable", "mean", "sd"))
socialIdentity_var_statistics <- socialIdentity_var_statistics %>% pivot_wider(names_from = c("group") , values_from = c("mean", "sd"))

socialIdentity_var_statistics$p <- -1 # initalize
socialIdentity_var_statistics$W <- -1 # initalize
res.sf <- wilcox.test(VarSocialNorms.SupportFans. ~ group, data = survey_results, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)
res.st<- wilcox.test(VarSocialNorms.SupportTeam. ~ group, data = survey_results, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)
res.id <- wilcox.test(VarFaith.SharedIdentity. ~ group, data = survey_results, na.rm = TRUE, paired = FALSE, exact = FALSE, conf.int = TRUE)


socialIdentity_var_statistics$p[socialIdentity_var_statistics$variable == "VarSocialNorms.SupportFans."] <- res.sf$p.value
socialIdentity_var_statistics$p[socialIdentity_var_statistics$variable == "VarSocialNorms.SupportTeam."] <- res.st$p.value
socialIdentity_var_statistics$p[socialIdentity_var_statistics$variable == "VarFaith.SharedIdentity."] <- res.id$p.value
socialIdentity_var_statistics$W[socialIdentity_var_statistics$variable == "VarSocialNorms.SupportFans."] <- res.sf$statistic[[1]]
socialIdentity_var_statistics$W[socialIdentity_var_statistics$variable == "VarSocialNorms.SupportTeam."] <- res.st$statistic[[1]]
socialIdentity_var_statistics$W[socialIdentity_var_statistics$variable == "VarFaith.SharedIdentity."] <- res.id$statistic[[1]]

# 4 Write data
print("Start export ...")
filename <- file.path(getwd(), "output", "manuscript__social_identity_salient_fans.tex")
print(xtable(socialIdentity_var_statistics, type = "latex", digits=PRECISION_PVAL), floating = FALSE, file = filename, include.rownames=FALSE)
print(filename)


print("... export finished.")

print(" -------------------- Script finished -------------------------")

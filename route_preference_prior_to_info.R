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

print(" -- Script started - Investigate route preference prior to information --")

# extract variables from data
path_to_survey_file <-  file.path(getwd(), "data", "Table S1.xlsx") # see sub-dir data
survey_results <- get_survey_results(path_to_survey_file, transform_likert = TRUE)
route_attractiveness <- get_route_attractiveness_long_format(survey_results)
route_attractiveness_priorToInfo <- subset(route_attractiveness, Informed == "PriorToInformation")


# 1 normality check
normality.long <- route_attractiveness_priorToInfo %>% group_by(group, condition) %>% shapiro_test(RouteAttractivenessLong)
normality.medium <- route_attractiveness_priorToInfo %>% group_by(group, condition) %>% shapiro_test(RouteAttractivenessMedium)
normality.short <- route_attractiveness_priorToInfo %>% group_by(group, condition) %>% shapiro_test(RouteAttractivenessShort)
normality <- rbind(normality.long, normality.medium, normality.short)

# 2 make sure that in between conditions behavior is equal
## is difference between conditions?

route_attractiveness_priorToInfo$conditionANDGroup <- paste(route_attractiveness_priorToInfo$condition, route_attractiveness_priorToInfo$group)
## Prior to information: no sifinificant difference between fans and students regarding short and long route
route_attractiveness_priorToInfo %>% kruskal_test(RouteAttractivenessLong ~ conditionANDGroup)
route_attractiveness_priorToInfo %>% kruskal_test(RouteAttractivenessShort ~ conditionANDGroup)

## the medium route
## Prior to information: differences between fans and students regarding medium route
route_attractiveness_priorToInfo %>% kruskal_test(RouteAttractivenessMedium ~ conditionANDGroup)
## within the groups no significant difference
route_attractiveness_priorToInfo %>% group_by(group) %>% kruskal_test(RouteAttractivenessMedium ~ condition)
## thus, students and fans need to be analyzed seperately

## compute stats
stats <- route_attractiveness_priorToInfo %>% dplyr::select(c("group", "condition", "RouteAttractivenessLong", "RouteAttractivenessMedium", "RouteAttractivenessShort" ))
stats <- stats %>% group_by(group) %>% get_summary_stats(type = "full")
results.stats <- stats[, c( "group", "variable",  "mean", "median", "sd", "n")]

kruskal.long <- route_attractiveness_priorToInfo %>% group_by(group) %>% kruskal_test(RouteAttractivenessLong ~ condition)
kruskal.medium <- route_attractiveness_priorToInfo %>% group_by(group) %>% kruskal_test(RouteAttractivenessMedium ~ condition)
kruskal.short <- route_attractiveness_priorToInfo %>% group_by(group) %>% kruskal_test(RouteAttractivenessShort ~ condition)

kruskal <- rbind(kruskal.long, kruskal.medium, kruskal.short)
kruskal <- kruskal[order(kruskal$group), ]


# 3 investigate route preferences
# we lump the conditions group-wise, because there is no difference (see results of step 2)

route_attractiveness_L <- pivot_longer(route_attractiveness_priorToInfo,
                                       -c(Informed, group, condition, id, conditionANDGroup),
                                       values_to = "RouteAttractiveness",
                                       names_to = "Route")

results.kruskal <- route_attractiveness_L %>% group_by(group) %>% kruskal_test(RouteAttractiveness ~ Route)

## unfortunately group_by does not work here
students <- subset(route_attractiveness_L, group == "Student & faculty associate")
results.dunntest.students <- invisible(capture.output(print(FSA::dunnTest(students$RouteAttractiveness ~ Route, data=students))))
results.dunntest.students <- cbind(Group = "Student & faculty associate", results.dunntest.students )

fans <- subset(route_attractiveness_L, group == "Fan")
results.dunntest.fans <- invisible(capture.output(print(FSA::dunnTest(fans$RouteAttractiveness ~ Route, data=fans))))
results.dunntest.fans <- cbind(Group = "Fan", results.dunntest.fans)

results.dunntest <- rbind(results.dunntest.fans, results.dunntest.students)

# 4 Write data
print("Start export ...")
filename1 <- file.path(getwd(), "output", "supplements_table_S6.tex")
print(xtable(results.stats, type = "latex", digits=PRECISION_PVAL), floating = FALSE, file = filename1, include.rownames=FALSE)
print(filename1)

filename2 <- file.path(getwd(), "output", "supplements_table_S7.tex")
print(xtable(results.dunntest, type = "latex", digits=PRECISION_PVAL), floating = FALSE, file = filename2, include.rownames=FALSE)
print(filename2)
print("... export finished.")

print(" -------------------- Script finished -------------------------")
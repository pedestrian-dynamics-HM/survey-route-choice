
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
survey_results <- get_survey_results("data/Table-S6-Survey-Raw-data.xlsx", transform_likert = TRUE)
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


# 3 investigate route preferences
# we lump the conditions group-wise, because there is no difference (see results of step 2)

route_attractiveness_L <- pivot_longer(route_attractiveness_priorToInfo, -c(Informed, group, condition, id, conditionANDGroup), values_to = "RouteAttractiveness", names_to = "Route")

results.kruskal <- route_attractiveness_L %>% group_by(group) %>% kruskal_test(RouteAttractiveness ~ Route)

## unfortunately group_by does not work here
students <- subset(route_attractiveness_L, group == "Student & faculty associate")
results.dunntest.students <- print(FSA::dunnTest(students$RouteAttractiveness ~ Route, data=students))
results.dunntest.students <- cbind(Group = "Student & faculty associate", results.dunntest.students )

fans <- subset(route_attractiveness_L, group == "Fan")
results.dunntest.fans <- print(FSA::dunnTest(fans$RouteAttractiveness ~ Route, data=fans))
results.dunntest.fans <- cbind(Group = "Fan", results.dunntest.fans)

results.dunntest <- rbind(results.dunntest.fans, results.dunntest.students)

# 4 Write data
## social identity
DIGITS <- 4
print(xtable(results.stats, type = "latex", digits=DIGITS), floating = FALSE, file = "output/RoutePreferencePriorToInfoStats.tex", include.rownames=FALSE)
print(xtable(results.dunntest, type = "latex", digits=DIGITS), floating = FALSE, file = "output/RoutePreferencePriorToInfoDunntest.tex", include.rownames=FALSE)

print("Export finished.")
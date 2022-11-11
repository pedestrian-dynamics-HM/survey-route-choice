# Survey analysis & traffic assignment model


## Description

In this repo, we store the survey raw data that
* we assess using statistical tests
* which we use to compute route choice proportions that we feed into our simulation model


## Dependencies

* R


## Overview

| Purpose                                                                                         | Corresponding section       | Script                                      | Corresponding output files                       |
|-------------------------------------------------------------------------------------------------|-----------------------------|---------------------------------------------|--------------------------------------------------|
| Compute route choice proportions based on survey data                                           | Traffic assignment model    | comp_route_choice_prop_traffic_assignment.R | table_6_RouteChoiceProportions.tex               |
| Report dropouts                                                                                 | Survey setup                | demographic.R                               | dropouts.tex                                     |
| Report age distribution                                                                         | Survey setup                | demographic.R                               | table_1_age.tex                                  |
| Report gender                                                                                   | Survey setup                | demographic.R                               | gender.tex                                       |
| Report feeling of in-group-membership                                                           | Survey results & discussion | social_identity.R                           | SocialIdentityVarsDifferenceFansStudents.tex     |
| Ensure there is no difference  between in-subjects                                              | Survey results & discussion | social_identity.R                           | SocialIdentityVarsKruskalWallisConditions.tex    |
| Report statistical difference when adding message  components (students only)                   | Survey results & discussion | effect_of_message_components.               | table_3_students.tex                             |
| Report statistical difference when adding message  components (fans only)                       | Survey results & discussion | effect_of_message_components.               | table_4_5_fans.tex                               |
| Investigate of adding two conditions together  makes a difference compared to adding them alone | Survey results & discussion | effect_of_message_components.R              | fans_long_route_no_diff_when_adding_together.tex |
| Route attractiveness: statistics (mean, std, ...)                                               | Supplements                 | effect_of_message_components.               | supplements_table_S1_S2_stats.tex                |
| Route attractiveness: difference between all designs                                            | Supplements                 | effect_of_message_components.               | supplements_table_S3_kruskal.tex                 |
| Report differences when adding message components  (students only)                              | Supplements                 | effect_of_message_components.               | supplements_table_S4_students.tex                |
| Report differences when adding message components  (fans only)                                  | Supplements                 | effect_of_message_components.               | supplements_table_S5_fans.tex                    |

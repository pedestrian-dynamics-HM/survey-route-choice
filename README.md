# Survey analysis & traffic assignment model

## Description

In this repo, we provide
* the survey raw data (data/Table 1.xlsx)
* the R-scripts with which we perform statistical tests
* the traffic assignment model with which we compute route choice proportions based on the survey data


## Dependencies

* R


## Overview

The following gives an overview how figures and tables of the manuscript and the supplementary material were generated.

| Objective                                                                           | Corresponding section       | Script                                      | Corresponding output files                         |
|-------------------------------------------------------------------------------------|-----------------------------|---------------------------------------------|----------------------------------------------------|
| Compute route choice proportions based on survey data                               | Traffic assignment model    | comp_route_choice_prop_traffic_assignment.R | manuscript_table_6.tex                             |
| Report age distribution                                                             | Survey setup                | demographic.R                               | manuscript_table_1.tex                             |
| Report dropouts                                                                     | Survey setup                | demographic.R                               | manuscript__dropouts.tex                           |
| Report gender                                                                       | Survey setup                | demographic.R                               | manuscript__gender.tex                             |
| Visualize route attractiveness (for each condition, prior to and with information)  | Survey results & discussion | difference_prior_with_info.R                | manuscript_Figure_4-part*.pdf                      |
| Report differences when providing information (students only)                       | Supplements                 | difference_prior_with_info.R                | supplements_table_S4_S5.tex                        |
| Report statistical difference when adding message components (students only)        | Survey results & discussion | effect_of_message_components.R              | manuscript_table_3.tex                             |
| Report statistical difference when adding message components (fans only)            | Survey results & discussion | effect_of_message_components.R              | manuscript_table_4_5.tex                           |
| Investigate of adding components together makes a difference                        | Survey results & discussion | effect_of_message_components.R              | manuscript_adding_comp_together_no_effect_fans.tex |
| Route attractiveness: difference between all designs                                | Supplements                 | effect_of_message_components.R              | supplements_table_S8.tex                           |
| Route attractiveness: statistics (mean, std, ...)                                   | Supplements                 | effect_of_message_components.R              | supplements_table_S9.tex                           |
| Report differences when adding message components (students only)                   | Supplements                 | effect_of_message_components.R              | supplements_table_S2.tex                           |
| Report differences when adding message components (fans only)                       | Supplements                 | effect_of_message_components.R              | supplements_table_S3.tex                           |
| Report social identity salient among fans                                           | Survey results & discussion | social_identity.R                           | manuscript__social_identity_salient_fans.tex       |
| Lumped route attractiveness (prior to information)                                  | Supplements                 | route_preference_prior_to_info.R            | supplements_table_S6.tex                           |
| Route preferences prior to information                                              | Supplements                 | route_preference_prior_to_info.R            | supplements_table_S7.tex                           |



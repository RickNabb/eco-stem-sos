library(foreign)
library(lavaan)

experience_data <- read.csv("data/fall-25/experiences-cfa-cleaned.csv")
values_data <- read.csv("data/fall-25/values-cfa-cleaned.csv")
# nrow(experience_data)
# colnames(experience_data)
View(experience_data)

# Clean the experience data for CFA
experience_data[experience_data == "Strongly agree"] <- 5.0
experience_data[experience_data == "Somewhat agree"] <- 4.0
experience_data[experience_data == "Neutral"] <- 3.0
experience_data[experience_data == "Somewhat disagree"] <- 2.0
experience_data[experience_data == "Strongly disagree"] <- 1.0
View(experience_data)

# Clean the values data for CFA
values_data[values_data == "Extremely important"] <- 5.0
values_data[values_data == "Very important"] <- 4.0
values_data[values_data == "Slightly important"] <- 3.0
values_data[values_data == "Moderately important"] <- 2.0
values_data[values_data == "Not at all important"] <- 1.0
View(values_data)

# Run the CFA for values
values_uncorrelated_model <- "climate =~ Q1_1 + Q1_2 + Q1_3 + Q1_4
          structure =~ Q5_1 + Q5_2 + Q5_3 + Q5_4 + Q5_5
         vibrancy =~ Q10_1 + Q10_2 + Q10_3 + Q10_4 + Q10_5 + Q10_6
         climate ~~ 0 * structure
         climate ~~ 0 * vibrancy
         structure ~~ 0 * vibrancy"
values_correlated_model <- "climate =~ Q1_1 + Q1_2 + Q1_3 + Q1_4
          structure =~ Q5_1 + Q5_2 + Q5_3 + Q5_4 + Q5_5
         vibrancy =~ Q10_1 + Q10_2 + Q10_3 + Q10_4 + Q10_5 + Q10_6"
values_uncorr_analysis <- cfa(values_uncorrelated_model, data = values_data, std.lv = TRUE)
values_corr_analysis <- cfa(values_correlated_model, data = values_data, std.lv = TRUE)
summary(values_uncorr_analysis, fit.measures = TRUE, standardized = TRUE)
summary(values_corr_analysis, fit.measures = TRUE, standardized = TRUE)

# Run the CFA for experiences
exp_uncorrelated_model <- "climate =~ q_climate_1 + q_climate_2 + q_climate_3 + q_climate_4
          structure =~ q_structure_1 + q_structure_2 + q_structure_3 + q_structure_4 + q_structure_5 + q_structure_6
          vibrancy =~ q_vibrancy_1 + q_vibrancy_2 + q_vibrancy_3 + q_vibrancy_4 + q_vibrancy_5 + q_vibrancy_6
         climate ~~ 0 * structure
         climate ~~ 0 * vibrancy
         structure ~~ 0 * vibrancy"
exp_correlated_model <- "climate =~ q_climate_1 + q_climate_2 + q_climate_3 + q_climate_4
          structure =~ q_structure_1 + q_structure_2 + q_structure_3 + q_structure_4 + q_structure_5 + q_structure_6
          vibrancy =~ q_vibrancy_1 + q_vibrancy_2 + q_vibrancy_3 + q_vibrancy_4 + q_vibrancy_5 + q_vibrancy_6"
exp_uncorr_analysis <- cfa(exp_uncorrelated_model, data = experience_data, std.lv = TRUE)
exp_corr_analysis <- cfa(exp_correlated_model, data = experience_data, std.lv = TRUE)
summary(exp_uncorr_analysis, fit.measures = TRUE, standardized = TRUE)
summary(exp_corr_analysis, fit.measures = TRUE, standardized = TRUE)

# I couldn't find an easy way to write these results to file
# so I just copy/paste it instead
# exp_uncorr_file_conn <- file("experiences-uncorrelated-results.txt")
# exp_corr_file_conn <- file("experiences-correlated-results.txt")
# exp_uncorr_res <- summary(exp_uncorr_analysis, fit.measures = TRUE, standardized = TRUE)
# exp_corr_res <- summary(exp_corr_analysis, fit.measures = TRUE, standardized = TRUE)
# lapply(exp_uncorr_res, write, exp_uncorr_file_conn, append=FALSE)
# writeLines(c(exp_corr_res), exp_corr_file_conn)
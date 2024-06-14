library(tidyverse)
library(gtsummary)
library(gt)

data = read.csv("data/AMR_Parental_KAP_New.csv")


data = data |> 
  mutate(Practice_Level_binary = case_when(
    Practice_Level == 2 ~ "1",
    Practice_Level == 1 ~ "2"
  ))


data$Practice_Level_binary = as.factor(data$Practice_Level_binary)

# multiple logistic regression
mv_log_reg = glm(Practice_Level_binary ~ Parents_age_years + Parents_sex + Parents_education_level + Employment_status + 
                   Family_type + Your_average_household_income_per_month_USD + 
                   Childs_sex + Childs_age + Knowledge_Level_cat +
                 Attitude_Level_cat,
                 data = data,
                 family = binomial(link = "logit"))           
summary(mv_log_reg)

# tbl_regression
mv_log_reg |> 
  tbl_regression(exponentiate = TRUE) |>
  bold_p(t = 0.05) |>
  as_gt() |>
  gtsave("tables/Table_6_Factors_level_of practice.docx")


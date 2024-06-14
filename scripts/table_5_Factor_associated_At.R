library(tidyverse)
library(gtsummary)
library(gt)
library(easystats)

data = read.csv("data/AMR_Parental_KAP_New.csv")

data = data |> 
  mutate(Attitude_Level_binary = case_when(
    Attitude_Level %in% c(1,3) ~ "0",
    Attitude_Level == 2 ~ "1"
))

data$Attitude_Level_binary = as.factor(data$Attitude_Level_binary)

# multiple logistic regression
mv_log_reg = glm(Attitude_Level_binary ~ Parents_age_years + Parents_sex + Parents_education_level + Employment_status + 
                   Family_type + Your_average_household_income_per_month_USD + 
                   Childs_sex + Childs_age + Number_of_children,
                 data = data,
                 family = binomial(link = "logit"))                    
summary(mv_log_reg)
report(mv_log_reg)

# tbl_regression
mv_log_reg |> 
  tbl_regression(exponentiate = T) |> 
  bold_p(t = 0.05) |>
  as_gt() |>
  gtsave("tables/Table_5_Factors_level_of attitude.docx")

library(tidyverse)
library(gtsummary)
library(gt)
library(easystats)


data = read.csv("data/AMR_Parental_KAP_New.csv")

# create new column
data = data |> 
  mutate(Knowledge_Level_binary = case_when(
    Knowledge_Level %in% c(1,3) ~ "1",
    Knowledge_Level == 2 ~ "0"
))

# convert response to factor
data$Knowledge_Level_binary = as.factor(data$Knowledge_Level_binary)

# multiple logistic regression
model1 = glm(Knowledge_Level_binary ~ Parents_age_years + Parents_sex + Parents_education_level + Employment_status + 
                   Family_type + Your_average_household_income_per_month_USD + 
                  Childs_sex + Childs_age + Number_of_children,
                 data = data,
                 family = binomial(link = "logit"))                  

summary(model1)

# tbl_regression
model1 |>
  tbl_regression(exponentiate = T) |>
  bold_p(t = 0.05) |> 
  as_gt() |>
  gtsave("tables/Table_4_Factors_level_of knowledge.docx")
  
table(data$Knowledge_Level_binary)
table(data$Knowledge_Level_cat)

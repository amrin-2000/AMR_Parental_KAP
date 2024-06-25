library(tidyverse)
library(gtsummary)
library(gt)

data = read_excel("data/AMR_Parental_KAP.xlsx")
sum(is.na(data))

# 1
data = data |> 
  mutate(Parents_age_years = case_when(
    `Parent’s age (years)` == 1 ~ "< 25",
    `Parent’s age (years)` == 2 ~ "25-35",
    `Parent’s age (years)` == 3 ~ "36-45",
    `Parent’s age (years)` == 4 ~ "> 45"
))

# 2
data = data |> 
  mutate(Parents_sex = case_when(
    `Parent’s sex` == 2 ~ "Female",
    `Parent’s sex` == 1 ~ "Male"
))
table(data$Parents_sex)

# 3
data = data |> 
  mutate(Parents_education_level = case_when(
    `Parent’s education level` == 6 ~ "Postgraduate",
    `Parent’s education level` %in% c(1, 7) ~ "Primary",
    `Parent’s education level` %in% c(2, 3, 4) ~ "Secondary",
    `Parent’s education level` == 5 ~ "Undergraduate"
  ))
table(data$Parents_education_level)

# 4
data = data |> 
  mutate(Employment_status = case_when(
    `Employment status` == 1 ~ "Employed",
    `Employment status` == 3 ~ "Not Employed",
    `Employment status` == 2 ~ "Self Employed"
))
table(data$Employment_status)

# 5
data = data |> 
  mutate(Family_type = case_when(
    `Family type` == 3 ~ "Extended Family",
    `Family type` == 1 ~ "Nuclear Family",
    `Family type` == 2 ~ "Single Family",
    
  ))

table(data$Family_type)

#6
data = data |> 
  mutate(Your_average_household_income_per_month_USD = case_when(
    `Your average household income per month (BDT)` == 3 ~ "High (greater than 550 USD)",
    `Your average household income per month (BDT)` == 1 ~ "Low (less than 350 USD)",
    `Your average household income per month (BDT)` == 2 ~ "Middle (less than 550 USD)"
))
table(data$Your_average_household_income_per_month_USD)

#7
data = data |> 
  mutate(Childs_sex = case_when(
    `Child’s sex` == 2 ~ "Female",
    `Child’s sex` == 1 ~ "Male"
  ))
table(data$Childs_sex)


#8
data = data |> 
  mutate(Childs_age = case_when(
    `Child’s age (years)` == 1 ~ "<5",
    `Child’s age (years)` == 2 ~ "5-9",
    `Child’s age (years)` == 3 ~ ">10"
  ))
table(data$Childs_age)


#9
data = data |> 
  mutate(Number_of_children = case_when(
    `Number of children` == 1 ~ "1",
    `Number of children` == 2 ~ "2",
    `Number of children` == 3 ~ ">=3"
  ))
table(data$Number_of_children)

#10
data = data |> 
  mutate(Who_is_the_leading_child_caregiver_at_home = case_when(
    `Who is the leading child caregiver at home?` == 1 ~ "Father",
    `Who is the leading child caregiver at home?`== 4 ~ "Grandmother",
    `Who is the leading child caregiver at home?` == 2 ~ "Mother",
    `Who is the leading child caregiver at home?` == 5 ~ "Others" 
  ))
table(data$Who_is_the_leading_child_caregiver_at_home)

#11
data = data |> 
  mutate(Are_grandparents_at_home_involved_in_treatment_decisions_when_your_child_is_ill. = 
           case_when(
    `Are grandparents at home involved in treatment decisions when your child is ill?` == 1 ~ "Never",
    `Are grandparents at home involved in treatment decisions when your child is ill?`== 4 ~ "Always",
    `Are grandparents at home involved in treatment decisions when your child is ill?` %in% c(2,3) ~ "Often"
  ))
table(data$Are_grandparents_at_home_involved_in_treatment_decisions_when_your_child_is_ill.)

#12
data = data |> 
  mutate(Knowledge_Level_cat = case_when(
    Knowledge_Level == 1 ~ "Poor",
    Knowledge_Level == 2 ~ "Moderate",
    Knowledge_Level == 3 ~ "Good"
  ))
table(data$Knowledge_Level_cat)


#13
data = data |> 
  mutate(Attitude_Level_cat = case_when(
    Attitude_Level == 1 ~ "Negative",
    Attitude_Level == 2 ~ "Uncertain",
    Attitude_Level == 3 ~ "Positive"
  ))
table(data$Attitude_Level_cat)

#14
data = data |> 
  mutate(Practice_Level_cat = case_when(
    Practice_Level == 1 ~ "Misuse",
    Practice_Level == 2 ~ "Good"
  ))
table(data$Practice_Level_cat)


library(openxlsx)
write.xlsx(data, "data/AMR_Parental_KAP.xlsx", rowNames = FALSE)



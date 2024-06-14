library(tidyverse)
library(gtsummary)
library(gt)

data = read.csv("data/AMR_Parental_KAP_New.csv")
View(data)

data |> 
  select(74:84) |> 
  tbl_summary()|> 
  as_gt() |> 
  gtsave("tables/Table_1_Demographic_characteristics.docx")


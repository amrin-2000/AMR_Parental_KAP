library(tidyverse)
library(gtsummary)
library(gt)

data = read.csv("data/AMR_Parental_KAP_New.csv")
View(data)

data |> 
  select(85:87) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("tables/Table_3_Level_of_KAP.docx")


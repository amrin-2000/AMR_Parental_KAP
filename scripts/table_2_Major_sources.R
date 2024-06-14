library(tidyverse)
library(gtsummary)
library(gt)

data = read.csv("data/AMR_Parental_KAP_New.csv")
View(data)
dim(data)
sum(is.na(data))

data |> 
  select(47:55) |>
  tbl_summary() |> 
  as_gt() |> 
  gtsave("tables/Table_2_Major_sources.docx")
  
    
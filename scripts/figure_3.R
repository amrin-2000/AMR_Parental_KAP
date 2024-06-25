library(tidyverse)
library(likert)
library(ggpubr)
library(ggthemes)
library(RColorBrewer)
library(readxl)

# Set working directory and read data
data <- read_excel("data/AMR_Parental_KAP.xlsx")

# Create a new data-frame with 38:43 columns
pr_dist = data |> 
  select(38:43)

colnames(pr_dist)

# Define the levels for all factors
levels <- c("Yes", "No")

# Convert to factors and ensure all columns have the same levels
pr_dist <- pr_dist |> 
  mutate(` I give my children antibiotics(No)` = factor(ifelse(` I give my children antibiotics(No)` == 1, "Yes", "No"), levels = levels),
         ` I check expiring date of antibiotic before giving to children(Yes)` = factor(ifelse(` I check expiring date of antibiotic before giving to children(Yes)` == 1, "Yes", "No"), levels = levels),
         ` I seek medical advice before giving antibiotic to my children(Yes)` = factor(ifelse(` I seek medical advice before giving antibiotic to my children(Yes)` == 1, "Yes", "No"), levels = levels),
         ` I give my children antibiotics when they get cough(No)` = factor(ifelse(` I give my children antibiotics when they get cough(No)` == 1, "Yes", "No"), levels = levels),
         ` I like to take antibiotic from pharmacy instead of taking from doctor(No)` = factor(ifelse(` I like to take antibiotic from pharmacy instead of taking from doctor(No)` == 1, "Yes", "No"), levels = levels),
         ` My child should complete a given dose, even he improve after 2 dose(Yes)` = factor(ifelse(` My child should complete a given dose, even he improve after 2 dose(Yes)` == 1, "Yes", "No"), levels = levels)
  )


# Convert data-frame to the appropriate format for likert plot
pr_dist = as.data.frame(pr_dist)


# Plot in likert scale 
p3 = plot(likert(pr_dist),
          ordered = FALSE,
          group.order = names(pr_dist),
          center = 1.5)

p3 = p3 + theme_pubr()
print(p3)

ggsave("figures/figure_3.png", p3, width = 12, height = 6, units = "in", dpi = 300) 

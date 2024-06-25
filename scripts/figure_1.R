library(tidyverse)
library(likert)
library(ggpubr)
library(ggthemes)
library(RColorBrewer)
library(readxl)

# Set working directory and read data
data <- read_excel("data/AMR_Parental_KAP.xlsx")

# Create a new data-frame with 12:23 columns
knowledge_dist <- data |> 
  select(12:23)

colnames(knowledge_dist)

# Convert to factors and ensure all columns have the same levels
knowledge_dist <- knowledge_dist |> 
  mutate(
    `Antibiotic kills the bacteria(Yes)` = factor(ifelse(`Antibiotic kills the bacteria(Yes)` == 1, "Yes", "No"), levels = c("Yes", "No")),
    ` Amoxicillin is an antibiotic (Yes)`= factor(ifelse(` Amoxicillin is an antibiotic (Yes)` == 1, "Yes", "No"), levels = c("Yes", "No")),
    ` Azithromycin is an antibiotic(Yes)`= factor(ifelse(` Azithromycin is an antibiotic(Yes)`== 1, "Yes", "No"), levels = c("Yes", "No")),
    ` Paracetamol is an antibiotic(No)` = factor(ifelse(` Paracetamol is an antibiotic(No)`== 1, "Yes", "No"), levels = c("Yes", "No")),
    ` Antibiotic kills the virus(No)`= factor(ifelse(` Antibiotic kills the virus(No)` == 1, "Yes", "No"), levels = c("Yes", "No")),
    ` Antibiotics used to treat diarrhoea(Yes)`= factor(ifelse(` Antibiotics used to treat diarrhoea(Yes)` == 1, "Yes", "No"), levels = c("Yes", "No")),
    ` Antibiotics are useful for flu and cough(No)` = factor(ifelse(` Antibiotics are useful for flu and cough(No)` == 1, "Yes", "No"), levels = c("Yes", "No")),
    ` Antibiotic resistant bacteria are difficult to treat(Yes)`= factor(ifelse(` Antibiotic resistant bacteria are difficult to treat(Yes)` == 1, "Yes", "No"), levels = c("Yes", "No")),
    ` Misuse of antibiotics can lead to antibiotic resistant bacteria(Yes)` = factor(ifelse(` Misuse of antibiotics can lead to antibiotic resistant bacteria(Yes)` == 1, "Yes", "No"), levels = c("Yes", "No")),
    ` Antibiotics can cause allergic reactions(Yes)` = factor(ifelse(` Antibiotics can cause allergic reactions(Yes)`== 1, "Yes", "No"), levels = c("Yes", "No")),
    ` Antibiotics can kill 0rmal flora(Yes)`= factor(ifelse(` Antibiotics can kill 0rmal flora(Yes)`== 1, "Yes", "No"), levels = c("Yes", "No")),
    ` Infectious disease are becoming difficult to treat with antibiotics(Yes)` = factor(ifelse(` Infectious disease are becoming difficult to treat with antibiotics(Yes)` == 1, "Yes", "No"), levels = c("Yes", "No"))
  )

View(knowledge_dist)

# Convert data-frame to the appropriate format for likert plot
knowledge_dist <- as.data.frame(knowledge_dist)


# Plot in likert scale 
p1 <- plot(likert(knowledge_dist), 
           ordered = FALSE, 
           group.order = names(knowledge_dist), 
           center = 1.5)
p1 = p1 + theme_pubr()
print(p1)

ggsave("figures/figure_1.png", p1, width = 12, height = 6, units = "in", dpi = 300)

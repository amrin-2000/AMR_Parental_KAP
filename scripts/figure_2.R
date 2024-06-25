library(likert)
library(ggpubr)
library(ggthemes)
library(RColorBrewer)
library(readxl)

# Set working directory and read data
data <- read_excel("data/AMR_Parental_KAP.xlsx")

# Create a new data-frame with columns 26 to 35
attitude_dist <- data |> 
  select(26:35)

# Define the levels for all factors
levels <- c("Agree", "Disagree")

# Convert to factors and ensure all columns have the same levels
attitude_dist <- attitude_dist |> 
  mutate(` I will see another doctor if the first one has not been prescribed antibiotics(Disagree)` = factor(ifelse(` I will see another doctor if the first one has not been prescribed antibiotics(Disagree)` == 1, "Agree", "Disagree"), levels = levels),
         ` I am not satisfied if the doctor does not prescribe an antibiotic to me(Disagree)` = factor(ifelse(` I am not satisfied if the doctor does not prescribe an antibiotic to me(Disagree)` == 1, "Agree", "Disagree"), levels = levels),
         ` Antibiotics are safe and hence can be used commonly(Disagree)` = factor(ifelse(` Antibiotics are safe and hence can be used commonly(Disagree)` == 1, "Agree", "Disagree"), levels = levels),
         ` Sick child is given antibiotics, even there is no indication(Disagree)` = factor(ifelse(` Sick child is given antibiotics, even there is no indication(Disagree)` == 1, "Agree", "Disagree"), levels = levels),
         ` Antibiotics can improve fever in children(Disagree)` = factor(ifelse(` Antibiotics can improve fever in children(Disagree)` == 1, "Agree", "Disagree"), levels = levels),
         `A child with cold is given antibiotics(Disagree)` = factor(ifelse(`A child with cold is given antibiotics(Disagree)` == 1, "Agree", "Disagree"), levels = levels),
         `I stop antibiotics when my child condition improves(Disagree)` = factor(ifelse(`I stop antibiotics when my child condition improves(Disagree)` == 1, "Agree", "Disagree"), levels = levels),
         `I reusing the same antibiotics for similar symptoms(Disagree)` = factor(ifelse(`I reusing the same antibiotics for similar symptoms(Disagree)` == 1, "Agree", "Disagree"), levels = levels),
         `Leftover antibiotics are good to keep at home in case I might need them for my child later on(Disagree)` = factor(ifelse(`Leftover antibiotics are good to keep at home in case I might need them for my child later on(Disagree)` == 1, "Agree", "Disagree"), levels = levels),
         `Doctors often take time to inform parents how antibiotics should be used for their children(Disagree)` = factor(ifelse(`Doctors often take time to inform parents how antibiotics should be used for their children(Disagree)` == 1, "Agree", "Disagree"), levels = levels)
  )

# Convert data-frame to the appropriate format for likert plot
attitude_dist <- as.data.frame(attitude_dist)

# Plot in likert scale 
p2 <- plot(likert(attitude_dist), 
           ordered = FALSE, 
           group.order = names(attitude_dist), 
           center = 1.5)
p2 <- p2 + theme_pubr()
print(p2)

ggsave("figures/figure_2.png", p2, width = 12, height = 6, units = "in", dpi = 300)

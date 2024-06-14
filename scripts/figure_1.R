library(tidyverse)
library(ggthemes)

data = read.csv("data/AMR_Parental_KAP_New.csv")

# Create a new data-frame with 12:25 columns
knowledge_dist = data |> 
  select(14:25)

View(knowledge_dist)
names(knowledge_dist)

# rename
knowledge_dist <- knowledge_dist |> 
  rename(
    Antibiotic_kills_the_bacteria_Yes = Antibiotic.kills.the.bacteria.Yes.,
    Amoxicillin_is_an_antibiotic_Yes = X.Amoxicillin.is.an.antibiotic..Yes.,
    Azithromycin_is_an_antibiotic_Yes = X.Azithromycin.is.an.antibiotic.Yes.,
    Paracetamol_is_an_antibiotic_No = X.Paracetamol.is.an.antibiotic.No.,
    Antibiotic_kills_the_virus_No = X.Antibiotic.kills.the.virus.No.,
    Antibiotics_used_to_treat_diarrhoea_Yes = X.Antibiotics.used.to.treat.diarrhoea.Yes.,
    Antibiotics_are_useful_for_flu_and_cough_No = X.Antibiotics.are.useful.for.flu.and.cough.No.,
    Antibiotic_resistant_bacteria_are_difficult_to_treat_Yes = X.Antibiotic.resistant.bacteria.are.difficult.to.treat.Yes.,
    Misuse_of_antibiotics_can_lead_to_antibiotic_resistant_bacteria_Yes = X.Misuse.of.antibiotics.can.lead.to.antibiotic.resistant.bacteria.Yes.,
    Antibiotics_can_cause_allergic_reactions_Yes = X.Antibiotics.can.cause.allergic.reactions.Yes.,
    Antibiotics_can_kill_normal_flora_Yes = X.Antibiotics.can.kill.0rmal.flora.Yes.,
    Infectious_disease_are_becoming_difficult_to_treat_with_antibiotics_Yes = X.Infectious.disease.are.becoming.difficult.to.treat.with.antibiotics.Yes.
  )

# Transform the data to long format
long_data = knowledge_dist |> 
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") |> 
  mutate(Response = ifelse(Response == 1, "Yes", "No"))


# Count the responses and calculate percentages
k_summary = long_data |> 
  group_by(Question, Response) |> 
  summarise(Count = n(), .groups = "drop") |> 
  group_by(Question) |> 
  mutate(Total = sum(Count)) |> 
  ungroup() |> 
  mutate(Percentage = Count/Total * 100)



# Create bar plot
plot1 = ggplot(k_summary, aes(x = Question, y = Percentage, fill = Response, label = paste0( " (", round(Percentage), "%)"))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Yes" = "skyblue", "No" = "lightcoral")) +
  labs(title = "Distribution of knowledge of antibiotic resistance among parents of school-going children (N=704)",
       x = "",
       y = "Percentage") +
  theme_economist() +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12))
 
ggsave("figures/figure_1.png", plot1, width = 18, height = 6, units = "in", dpi = 300)

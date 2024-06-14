library(tidyverse)
library(ggthemes)

data = read.csv("data/AMR_Parental_KAP_New.csv")

# Create a new data-frame with 12:25 columns
attitude_dist = data |> 
  select(28:37)

View(attitude_dist)
names(attitude_dist)

# rename
attitude_dist = attitude_dist |> 
  rename(
    I_will_see_another_doctor_if_the_first_one_has_not_been_prescribed_antibiotics_Disagree = X.I.will.see.another.doctor.if.the.first.one.has.not.been.prescribed.antibiotics.Disagree.,
    I_am_not_satisfied_if_the_doctor_does_not_prescribe_an_antibiotic_to_me_Disagree = X.I.am.not.satisfied.if.the.doctor.does.not.prescribe.an.antibiotic.to.me.Disagree.,
    Antibiotics_are_safe_and_hence_can_be_used_commonly_Disagree = X.Antibiotics.are.safe.and.hence.can.be.used.commonly.Disagree.,
    Sick_child_is_given_antibiotics_even_there_is_no_indication_Disagree = X.Sick.child.is.given.antibiotics..even.there.is.no.indication.Disagree.,
    Antibiotics_can_improve_fever_in_children_Disagree = X.Antibiotics.can.improve.fever.in.children.Disagree.,
    A_child_with_cold_is_given_antibiotics_Disagree = A.child.with.cold.is.given.antibiotics.Disagree.,
    I_stop_antibiotics_when_my_child_condition_improves_Disagree = I.stop.antibiotics.when.my.child.condition.improves.Disagree.,
    I_reusing_the_same_antibiotics_for_similar_symptoms_Disagree = I.reusing.the.same.antibiotics.for.similar.symptoms.Disagree.,
    Leftover_antibiotics_are_good_to_keep_at_home_in_case_I_might_need_them_for_my_child_later_on_Disagree = Leftover.antibiotics.are.good.to.keep.at.home.in.case.I.might.need.them.for.my.child.later.on.Disagree.,
    Doctors_often_take_time_to_inform_parents_how_antibiotics_should_be_used_for_their_children_Disagree = Doctors.often.take.time.to.inform.parents.how.antibiotics.should.be.used.for.their.children.Disagree.
  )

# Transform the data to long format
long_data = attitude_dist |> 
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
plot2 = ggplot(k_summary, aes(x = Question, y = Percentage, fill = Response, label = paste0( " (", round(Percentage), "%)"))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Yes" = "skyblue", "No" = "lightcoral")) +
  labs(title = "Attitude towards antibiotic resistance and the misuse of antibiotics among parents of school-going children (N=704)",
       x = "",
       y = "Percentage") +
  theme_economist()

ggsave("figures/figure_2.png", plot2, width = 22, height = 6, units = "in", dpi = 300)


  


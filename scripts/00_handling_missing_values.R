library(tidyverse)
library(readxl)
library(naniar)

data = read_excel("data/AMR_Parental_KAP.xlsx")
View(data)
glimpse(data)
dim(data)

# check missing value
miss_var_summary(data)
gg_miss_var(data)
sum(is.na(data))
colSums(is.na(data))


# replacing missing value
#1
data = data |> 
  mutate(`Child’s age (years)` = replace_na(`Child’s age (years)`, 2))

#2
data |> 
  count(`Why would you give your child antibiotics without a physician's advice? (You can choose more than one)`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`Why would you give your child antibiotics without a physician's advice? (You can choose more than one)` = 
           replace_na(`Why would you give your child antibiotics without a physician's advice? (You can choose more than one)`,
                      "I don't give antibiotics without doctor's consultation"))

#3
data |> 
  count(`I thought that my child's condition was not severe enough`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`I thought that my child's condition was not severe enough` = 
           replace_na(`I thought that my child's condition was not severe enough`, 0))


#4
data |> 
  count(`I didn't have enough time to visit a doctor`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`I didn't have enough time to visit a doctor` = 
           replace_na(`I didn't have enough time to visit a doctor`, 0))

#5
data |> 
  count(`I didn't have enough money to pay for the hospital visit`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`I didn't have enough money to pay for the hospital visit` = 
           replace_na(`I didn't have enough money to pay for the hospital visit`, 0))

#6
data |> 
  count(`Lack of hospitals in the nearest place`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`Lack of hospitals in the nearest place` = 
           replace_na(`Lack of hospitals in the nearest place`, 0))


#7
data |> 
  count(`It is convenient to purchase antibiotics from retail pharmacies`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`It is convenient to purchase antibiotics from retail pharmacies` = 
           replace_na(`It is convenient to purchase antibiotics from retail pharmacies` , 0))

#8
data |> 
  count(`Lack of trust in medical service`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`Lack of trust in medical service` = 
           replace_na(`Lack of trust in medical service` , 0))

#9
data |> 
  count(`No confidence with doctor's medication`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`No confidence with doctor's medication`= 
           replace_na(`No confidence with doctor's medication`, 0))


#10
data |> 
  count(`Easier to apply previous prescription`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`Easier to apply previous prescription` = 
           replace_na(`Easier to apply previous prescription`, 0))

#11
data |> 
  count(`Suggestions from others`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`Suggestions from others` = 
           replace_na(`Suggestions from others`, 0))

#12
data |> 
  count(`Knowledge of antibiotics`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`Knowledge of antibiotics` = 
           replace_na(`Knowledge of antibiotics`, 0))

#13
data |> 
  count(`I don't give antibiotics without doctor's consultation`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`I don't give antibiotics without doctor's consultation` = 
           replace_na(`I don't give antibiotics without doctor's consultation`, 1))

#14
data |> 
  count(`Others...68`) |> 
  arrange(desc(n))

data = data |> 
  mutate(`Others...68` = 
           replace_na(`Others...68`, 0))

miss_var_summary(data)
gg_miss_var(data)

data = data |> 
  mutate_if(is.character, as.factor)

write.csv(data, "data/AMR_Parental_KAP_New.csv", row.names = F)

library(tidyverse)
library(psycho)

#path reference
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot1/combinedData_Anxiety_Cognitive_Maps.csv' 

#load data
df <- read_csv(data_path)
drop_df <- df %>% 
  filter(sectionType == 'dragTaskDropEvent') %>% 
  select(subject, trialCount, trialAttempt, dragThreat, dragAcc)
drop_df

# group by subject and trial count, filter for trialAttempt == 1 and grab the first row
drop_combined_trials_df <- drop_df %>%
  group_by(subject, trialCount) %>%
  filter(trialAttempt == 1) %>%
  slice(1) %>%
  ungroup()
drop_combined_trials_df


# calculate percentage of threat being first vs. neutral being first drag out of all the trials for each subject
percentage_drop_threat_df <- drop_combined_trials_df %>%
  group_by(subject) %>%
  summarize(
    percentage_threat = sum(dragThreat == "threat") / n() * 100,
    percentage_neutral = sum(dragThreat == "neutral") / n() * 100
  )
percentage_drop_threat_df



#ACCURACY INCLUDED

#filter where the first image participants dragged & dropped was accurate/they put it in the right spot, and observe based off of those trials

# group by subject and trial count, filter for trialAttempt == 1 and grab the first row, then filter for dragAcc == 1
drop_combined_trials_acc_df <- drop_df %>%
  group_by(subject, trialCount) %>%
  filter(trialAttempt == 1) %>%
  slice(1) %>%
  ungroup() %>%
  filter(dragAcc == 1)
drop_combined_trials_acc_df

# calculate percentage of threat being first vs. neutral being first drag out of all the trials for each subject WHERE THEY WERE ACCURATE
percentage_drop_threat_acc_df <- drop_combined_trials_acc_df %>%
  group_by(subject) %>%
  summarize(
    percentage_threat = sum(dragThreat == "threat") / n() * 100,
    percentage_neutral = sum(dragThreat == "neutral") / n() * 100
  )
percentage_drop_threat_acc_df

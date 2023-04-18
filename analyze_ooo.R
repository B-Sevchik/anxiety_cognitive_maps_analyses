library(tidyverse)

#path reference
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot1/combinedData_Anxiety_Cognitive_Maps.csv' 

#load data
df <- read_csv(data_path)
ooo_df <- df %>%
  filter(taskName == 'oddOneOutTest')

mistake_df <- ooo_df %>% 
  filter(acc == 0) %>% 
  mutate(otherNode = case_when(partResp == 1 ~ ifelse(option2CommunityNumber == chosenCommunityNumber, 2, 3),
                               partResp == 2 ~ ifelse(option1CommunityNumber == chosenCommunityNumber, 1, 3),
                               partResp == 3 ~ ifelse(option1CommunityNumber == chosenCommunityNumber, 1, 2))) %>% 
  mutate(otherNodeThreatStatus = case_when(otherNode == 1 ~ option1ThreatStatus,
                                           otherNode == 2 ~ option2ThreatStatus,
                                           otherNode == 3 ~ option3ThreatStatus)) %>% 
  select(subject, trialCount, acc, RT, partResp, chosenThreatStatus, otherNode, otherNodeThreatStatus) %>% 
  filter(chosenThreatStatus != otherNodeThreatStatus)
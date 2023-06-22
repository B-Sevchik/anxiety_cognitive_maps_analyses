library(tidyverse)

#path reference
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot2/combinedData_Anxiety_Cognitive_Maps.csv' 

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
mistake_df

###
#include STAI scores in ooo analyses

df_STAI_ooo <- read_csv('/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot2/STAI_scores_calculated.csv')
df_STAI_ooo

df_pilot1_STAI_simple <- df_STAI_ooo %>%
  select(subjectID, anxiety_level)
df_pilot1_STAI_simple

names(df_pilot1_STAI_simple) <- c('subject', 'anxiety_level')
df_pilot1_STAI_simple

ooo_anxiety_df <- merge(mistake_df, df_pilot1_STAI_simple, by = "subject", all = TRUE)
ooo_anxiety_df


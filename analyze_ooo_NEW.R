library(tidyverse)

#path reference
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/all_participants/combinedData_Anxiety_Cognitive_Maps.csv' 

#load data
df <- read_csv(data_path)
ooo_df <- df %>%
  filter(taskName == 'oddOneOutTest')
ooo_df

all_same_threat_ooo_df <- ooo_df %>%
  select(acc, subject, chosenNode, chosenImageFile, chosenCommunityNumber, chosenThreatStatus,
         option1Node, option1ImageFile, option1CommunityNumber, option1ThreatStatus,
         option2Node, option2ImageFile, option2CommunityNumber, option2ThreatStatus,
         option3Node, option3ImageFile, option3CommunityNumber, option3ThreatStatus) %>%
  mutate(all_threat_status = case_when(
    (option1ThreatStatus == 'threat' & option2ThreatStatus == 'threat' & option3ThreatStatus == 'threat') ~ 'all_threat',
    (option1ThreatStatus == 'neutral' & option2ThreatStatus == 'neutral' & option3ThreatStatus == 'neutral') ~ 'all_neutral'
  )) %>%
  filter(all_threat_status == 'all_threat' | all_threat_status == 'all_neutral')
all_same_threat_ooo_df


#compare accuracies of all_threat vs. all_neutral
all_same_ooo_acc_df <- all_same_threat_ooo_df %>%
  group_by(subject, all_threat_status) %>%
  summarise(percentage_acc_1 = sum(acc == 1) / sum(all_threat_status %in% c('all_threat', 'all_neutral')) * 100)
all_same_ooo_acc_df

#across subjects plot comparing accuracy
all_same_ooo_mean_acc_df <- all_same_ooo_acc_df %>%
  group_by(all_threat_status) %>%
  summarise(mean_acc = mean(percentage_acc_1))
all_same_ooo_mean_acc_df

###
#include STAI scores in ooo analyses

df_STAI_ooo <- read_csv('/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/all_participants/STAI_scores_calculated.csv')
df_STAI_ooo

df_pilot1_STAI_simple <- df_STAI_ooo %>%
  select(subjectID, anxiety_level)
df_pilot1_STAI_simple

names(df_pilot1_STAI_simple) <- c('subject', 'anxiety_level')
df_pilot1_STAI_simple

ooo_anxiety_new_df <- merge(all_same_ooo_acc_df, df_pilot1_STAI_simple, by = "subject", all = TRUE)
ooo_anxiety_new_df

setwd("~/Documents/GitHub/anxiety_cognitive_maps_analyses")

library(tidyverse)
df <- read_csv('/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot12/combinedData_Anxiety_Cognitive_Maps.csv')

#analyze drop events
drop_df <- df %>% 
  filter(sectionType == 'dragTaskDropEvent') %>% 
  select(subject, trialCount, trialAttempt, RT, nCorrect, dragThreat, dragAcc, swappedThreat, swappedAcc)
write_csv(drop_df, 'data/dropEvent.csv')
drop_df

#analyze check answer events
check_answer_df <- df %>%
  filter(sectionType == 'dragTaskCheckAnswerEvent') %>%
  select(subject, trialCount, trialAttempt, RT, nCorrect, slot0Acc, slot0CurrentType, slot0CorrectType, slot0CurrentSRC, slot0CorrectSRC, slot1Acc, 
         slot1CurrentType, slot1CorrectType,slot1CurrentSRC, slot1CorrectSRC,
         slot2Acc, slot2CurrentType, slot2CorrectType, slot2CurrentSRC, slot2CorrectSRC,
         slot3Acc, slot3CurrentType, slot3CorrectType, slot3CurrentSRC, slot3CorrectSRC,
         slot4Acc, slot4CurrentType, slot4CorrectType, slot4CurrentSRC, slot4CorrectSRC,
         slot5Acc, slot5CurrentType, slot5CorrectType, slot5CurrentSRC, slot5CorrectSRC,
         slot6Acc, slot6CurrentType, slot6CorrectType, slot6CurrentSRC, slot6CorrectSRC,
         slot7Acc, slot7CurrentType, slot7CorrectType, slot7CurrentSRC, slot7CorrectSRC,
         slot8Acc, slot8CurrentType, slot8CorrectType, slot8CurrentSRC, slot8CorrectSRC,
         slot9Acc, slot9CurrentType, slot9CorrectType, slot9CurrentSRC, slot9CorrectSRC)
write_csv(check_answer_df, 'data/checkAnswer.csv')
check_answer_df

#analyze illegal transition data
illegal_transition_df <- df %>%
  filter(sectionType == 'mainTask' & taskName == 'illegalTransitionTask') %>%
  select(subject, trialCount, blockTrialCount, block, RT, acc, activeNodeCommunityCongruency:transitionThreatKind)
write_csv(illegal_transition_df, 'data/illegalTransition.csv')
illegal_transition_df

#analyze odd one out data
ooo_df <- df %>%
  filter(sectionType == 'mainTask' & taskName == 'oddOneOutTest') %>%
  select(subject, trialCount, RT, acc, partResp, chosenNode:chosenThreatStatus,
         option1CommunityNumber, option1ThreatStatus,
         option2CommunityNumber, option2ThreatStatus,
         option3CommunityNumber, option3ThreatStatus)
write_csv(ooo_df, 'data/oddOneOut.csv')

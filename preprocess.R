setwd("~/Documents/GitHub/anxiety_cognitive_maps_analyses")

library(tidyverse)
df <- read_csv('data/combinedData_Anxiety_Cognitive_Maps.csv')

#analyze drop events
drop_df <- df %>% 
  filter(sectionType == 'dragTaskDropEvent') %>% 
  select(subject, trialAttempt, RT, nCorrect, dragThreat, dragAcc, swappedThreat, swappedAcc)
write_csv(drop_df, 'data/dropEvent.csv')

#analyze check answer events
check_answer_df <- df %>%
  filter(sectionType == 'dragTaskCheckAnswerEvent') %>%
  select(subject, trialCount, trialAttempt, RT, nCorrect, slot0Acc, slot0CurrentType, slot0CorrectType, slot1Acc, slot1CurrentType, slot1CorrectType,
         slot2Acc, slot2CurrentType, slot2CorrectType,
         slot3Acc, slot3CurrentType, slot3CorrectType,
         slot4Acc, slot4CurrentType, slot4CorrectType,
         slot5Acc, slot5CurrentType, slot5CorrectType,
         slot6Acc, slot6CurrentType, slot6CorrectType,
         slot7Acc, slot7CurrentType, slot7CorrectType,
         slot8Acc, slot8CurrentType, slot8CorrectType,
         slot9Acc, slot9CurrentType, slot9CorrectType)
write_csv(check_answer_df, 'data/checkAnswer.csv')

#analyze illegal transition data
illegal_transition_df <- df %>%
  filter(sectionType == 'mainTask' & taskName == 'illegalTransitionTask') %>%
  select(subject, trialCount, blockTrialCount, block, RT, acc, activeNodeCommunityCongruency:transitionThreatKind)
write_csv(illegal_transition_df, 'data/illegalTransition.csv')

#analyze odd one out data
ooo_df <- df %>%
  filter(sectionType == 'mainTask' & taskName == 'oddOneOutTest') %>%
  select(subject, trialCount, RT, acc, partResp, chosenNode:chosenThreatStatus,
         option1CommunityNumber, option1ThreatStatus,
         option2CommunityNumber, option2ThreatStatus,
         option3CommunityNumber, option3ThreatStatus)
write_csv(ooo_df, 'data/oddOneOut.csv')

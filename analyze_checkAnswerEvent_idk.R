library(tidyverse)
library(psycho)

check_answer_df

#for trial one, attempt one, for first subject
check_answer_acc_df <- check_answer_df %>%
  filter(trialCount == 1 & trialAttempt == 1 & subject == 'a273wocs111dh0' ) %>%
  select(subject, slot0CurrentType, slot0Acc, slot1CurrentType, slot1Acc, slot2CurrentType, slot2Acc, slot3CurrentType, slot3Acc, slot4CurrentType, slot4Acc,
         slot5CurrentType, slot5Acc, slot6CurrentType, slot6Acc, slot7CurrentType, slot7Acc, slot8CurrentType, slot8Acc, slot9CurrentType, slot9Acc)
check_answer_acc_df


counts_check_answer_df <- check_answer_acc_df %>%
  group_by(subject, , transitionThreatKind) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = acc_descriptor, values_from = count, values_fill = 0)
counts_transitionType_df




acc_illegal_df <- illegal_df %>%
  select(subject, transitionType, acc, transitionThreatKind) %>%
  group_by(transitionType, acc)
acc_illegal_df

transition_type_df <- acc_illegal_df %>%
  mutate(acc_descriptor = case_when(
    (transitionType == 'i' & acc == 1) ~ "hits",
    (transitionType == 'i' & acc == 0) ~ "misses",
    (transitionType == 'l' & acc == 0) ~ "false_alarms",
    (transitionType == 'l' & acc == 1) ~ "correct_rejections"
  ))
transition_type_df

transition_type_df <- transition_type_df %>%
  mutate(transition_threat = case_when(
    (transitionThreatKind == 'threat-threat' | transitionThreatKind == 'threat-neutral' | transitionThreatKind == 'neutral-threat') ~ "contains_threat",
    (transitionThreatKind == 'neutral-neutral') ~ "no_threat"
  ))
transition_type_df

counts_transitionType_df <- transition_type_df %>%
  group_by(subject, acc_descriptor, transition_threat) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = acc_descriptor, values_from = count, values_fill = 0)
counts_transitionType_df

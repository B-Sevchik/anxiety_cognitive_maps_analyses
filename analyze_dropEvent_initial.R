library(tidyverse)
library(psycho)

drop_df

drop_df <- drop_df %>%
  mutate(involving_threat = case_when(
    (dragThreat == 'threat' | swappedThreat == 'threat') ~ 'involves_threat',
    TRUE ~ 'does_not_involve_threat'
  ))
drop_df

drop_acc_df <- drop_df %>%
  select(subject, dragAcc, swappedAcc, involving_threat) %>%
  group_by(subject, involving_threat, dragAcc)
drop_acc_df



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
  
  
  

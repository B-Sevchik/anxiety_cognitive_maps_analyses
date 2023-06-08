library(tidyverse)
library(psycho)

#path reference
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot1/combinedData_Anxiety_Cognitive_Maps.csv' 

#load data
df <- read_csv(data_path)
illegal_df <- df %>%
  filter(taskName == 'illegalTransitionTask')

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

counts_transitionType_df <- transition_type_df %>%
  group_by(subject, acc_descriptor, transitionThreatKind) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = acc_descriptor, values_from = count, values_fill = 0)
counts_transitionType_df

transition_threat_kind_df <- counts_transitionType_df %>%
  mutate(threat_transition = case_when(
    (transitionThreatKind == 'neutral-neutral') ~ 0,
    (transitionThreatKind == 'neutral-threat') ~ 1,
    (transitionThreatKind == 'threat-neutral') ~ 1,
    (transitionThreatKind == 'threat-threat') ~ 1
  ))
transition_threat_kind_df

dprime_df <- transition_threat_kind_df
dprime.stats <-psycho::dprime(new_df$hits, new_df$false_alarms, new_df$misses, new_df$correct_rejections)
dprime_df$dprime <- dprime.stats$dprime
dprime_df

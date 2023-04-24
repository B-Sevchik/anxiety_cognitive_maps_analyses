library(tidyverse)
library(psycho)

#path reference
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot1/combinedData_Anxiety_Cognitive_Maps.csv' 

#load data
df <- read_csv(data_path)
illegal_df <- df %>%
  filter(taskName == 'illegalTransitionTask')

acc_illegal_df <- illegal_df %>%
  select(subject, transitionType, acc) %>%
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
  group_by(subject, acc_descriptor) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = acc_descriptor, values_from = count)
counts_transitionType_df


  
dprime.stats <-psycho::dprime(counts_transitionType_df$hits, counts_transitionType_df$false_alarms, counts_transitionType_df$misses, counts_transitionType_df$correct_rejections)
dprime_df$dprime <- dprime.stats$dprime
dprime_df

library(tidyverse)
library(psycho)

#SAME PLOT AS BEFORE, BUT GROUPING BASED ON EACH KIND OF THREAT TRANSITION (THREAT-NEUTRAL, NEUTRAL-NEUTRAL, ETC., INSTEAD OF BASED ON IF IT CONTAINS THREAT VS. NO IN A BINARY)

#path reference
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot6/combinedData_Anxiety_Cognitive_Maps.csv' 

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

dprime_df <- counts_transitionType_df
dprime.stats <-psycho::dprime(counts_transitionType_df$hits, counts_transitionType_df$false_alarms, counts_transitionType_df$misses, counts_transitionType_df$correct_rejections)
dprime_df$dprime <- dprime.stats$dprime
dprime_df


###
### include STAI

#include STAI scores in ooo analyses

df_STAI_illegal <- read_csv('/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot6/STAI_scores_calculated.csv')
df_STAI_illegal

df_pilot1_STAI_simple <- df_STAI_illegal %>%
  select(subjectID, anxiety_level)
df_pilot1_STAI_simple

names(df_pilot1_STAI_simple) <- c('subject', 'anxiety_level')
df_pilot1_STAI_simple

illegal_anxiety_df <- merge(dprime_df, df_pilot1_STAI_simple, by = "subject", all = TRUE)
illegal_anxiety_df
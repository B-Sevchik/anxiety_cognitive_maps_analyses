#analyze drop first tries/experimenting on how the code will work:


###SUBJECT SPECIFIC VERSION

library(tidyverse)
library(psycho)

#path reference
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot1/combinedData_Anxiety_Cognitive_Maps.csv' 

#load data
df <- read_csv(data_path)
drop_df <- df %>% 
  filter(sectionType == 'dragTaskDropEvent') %>% 
  select(subject, trialCount, trialAttempt, dragThreat, dragAcc)
drop_df

#subject specific
drop_combined_trials_df <- data.frame()
for (trial_count in seq(1,500)) {
  filtered_drop_df <- drop_df %>%
    filter(subject == 'a273wocs111dh0', trialCount == trial_count, trialAttempt == 1) %>%
    slice(1)
  drop_combined_trials_df <- rbind(drop_combined_trials_df, filtered_drop_df)
}
drop_combined_trials_df


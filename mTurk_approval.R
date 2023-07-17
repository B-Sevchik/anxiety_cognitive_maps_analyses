df_subject <- df %>%
  filter(subject == 'a15sb9kagzmj4g', taskName == 'oddOneOutTest')
df_subject

mean_acc <- mean(df_subject$acc)
mean_acc


#checking for duplicate participants
check_df <- read_csv('/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/all_participants/STAI_scores_calculated.csv')

check_df

check_df <- check_df %>%
  group_by(subjectID) %>%
  mutate(row_count = n()) %>%
  select(subjectID, row_count)
check_df
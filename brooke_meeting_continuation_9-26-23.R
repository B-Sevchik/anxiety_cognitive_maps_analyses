library(plotrix)

# finding trial attempts by trial and subject
trialAttemptsByTrial <- check_answer_df %>% 
  group_by(subject, trialCount) %>% 
  summarize(n_attempts = max(trialAttempt))

included_sub_trials <- trialAttemptsByTrial %>% 
  filter(trialCount != 1 & n_attempts != 1) %>% 
  mutate(combined = paste(subject, trialCount, sep="_"))
#25 subjects usable

#list of column names for easy reference
correct_type_columns <- colnames(check_answer_df)[grepl("CorrectType", colnames(check_answer_df))]
correct_src_columns <- colnames(check_answer_df)[grepl("CorrectSRC", colnames(check_answer_df))]
current_src_columns <- colnames(check_answer_df)[grepl("CurrentSRC", colnames(check_answer_df))]
current_type_columns <- colnames(check_answer_df)[grepl("CurrentType", colnames(check_answer_df))]
acc_columns <- colnames(check_answer_df)[grepl("Acc", colnames(check_answer_df))]

#remove columns we don't need
new_check_answer_df <- check_answer_df  %>% 
  select(-correct_type_columns) %>% 
  select(-correct_src_columns) %>% 
  select(-RT) %>% 
  select(-nCorrect) %>% 
  filter(paste(subject, trialCount, sep="_") %in% included_sub_trials$combined)

slot_accuracies <- new_check_answer_df %>% 
  select(-current_src_columns) %>% 
  select(-current_type_columns) %>%
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "acc") %>% 
  mutate(slot = gsub("Acc", "", slot))

slot_srcs <- new_check_answer_df %>% 
  select(-acc_columns) %>% 
  select(-current_type_columns) %>% 
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "src") %>% 
  mutate(slot = gsub("CurrentSRC", "", slot))

slot_threat_types <- new_check_answer_df %>% 
  select(-acc_columns) %>% 
  select(-current_src_columns) %>% 
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "threatType") %>% 
  mutate(slot = gsub("CurrentType", "", slot))

#join everything together
slot_images <- slot_srcs %>% 
  left_join(slot_threat_types, by=c("subject", "trialCount", "trialAttempt", "slot")) %>% 
  left_join(slot_accuracies, by=c("subject", "trialCount", "trialAttempt", "slot")) %>% 
  mutate(src = gsub(".jpg", "", src)) %>% 
  mutate(src = paste(src, "_", threatType, ".jpg", sep="")) %>% 
  select(-threatType)

# image_new_names = tibble(
#   src = unique(slot_images$src),
#   new_names = c(1:14) # <- this needs to be a handtyped list of e.g., snake.jpg, dog.jpg, etc, that corresponds to each row of images
# )
# 
# slot_images <- slot_images %>% 
#   left_join(image_new_names, by="src") %>% 
#   mutate(src = new_names) %>% 
#   select(-new_names)

slot_images

#next step: use the summarise function on slot_images to calculate, for each (group by) unique subject, trialCount, and src,
#the sum of correct placements (sum of acc column) across trial attempts. 
sum_correct_trials_df <- slot_images %>%
  group_by(subject, trialCount, src) %>%
  summarise(sum_correct_trials = sum(acc))
sum_correct_trials_df

#next step after that, group by subject and src (collapse across trials) to find the average accuracy score for each trial
average_trial_accuracy_df <- sum_correct_trials_df %>%
  group_by(subject, src) %>%
  summarise(average_accuracy = mean(sum_correct_trials))
average_trial_accuracy_df

#last step, group by just src to find mean accuracy score for each 
mean_accuracy_src_df <- average_trial_accuracy_df %>%
  group_by(src) %>%
  summarise(mean_accuracy = mean(average_accuracy), sem = std.error(average_accuracy))
mean_accuracy_src_df


#SCORE 2

#create data frame - this is just because I was spending too much time trying to manipulate the existing data frames I had, will replace
df_score2_images <- data.frame(
  subject = rep(1, 9),        
  trial = rep(2, 9),          
  attempt = 1:9,              
  `slot0_bear` = c(0, 0, 1, 1, 1, 1, 0, 1, 1)  
)
df_score2_images

#score2
score_2_attempt <- first(which(df_score2_images$`slot0_bear` == 1))
score_2 <- df_score2_images$attempt[score_2_attempt]
score_2




#SCORE 3
#find the first attempt that it is correct
first_attempt_with_slot1 <- which(df_score2_images$`slot0_bear` == 1)[1]

while (!is.na(first_attempt_with_slot1)) {
  #check if any of the next attempts are incorrect 
  subsequent_attempts <- df_score2_images$`slot0_bear`[(first_attempt_with_slot1 + 1):nrow(df_score2_images)]
  
  if (sum(subsequent_attempts == 0) == 0) { #check to make sure that all the following attempts are correct
    break  #exit the loop
  } else {
    # if not, update first attempt to the next time they got it correct
    first_attempt_with_slot1 <- which(df_score2_images$`slot0_bear` == 1)[which(df_score2_images$`slot0_bear` == 1) > first_attempt_with_slot1][1]
  }
}

if (!is.na(first_attempt_with_slot1)) {
  print(df_score2_images$attempt[first_attempt_with_slot1])
} else {
  print("No attempt found where slot0_bear is equal to 1 and never equal to 0 again.")
}






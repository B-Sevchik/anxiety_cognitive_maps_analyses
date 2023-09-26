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
  select(-nCorrect)

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

image_new_names = tibble(
  src = unique(slot_images$src),
  new_names = c(1:14) # <- this needs to be a handtyped list of e.g., snake.jpg, dog.jpg, etc, that corresponds to each row of images
)

slot_images <- slot_images %>% 
  left_join(image_new_names, by="src") %>% 
  mutate(src = new_names) %>% 
  select(-new_names)

slot_images

#next step: use the summarise function on slot_images to calculate, for each (group by) unique subject, trialCount, and src,
#the sum of correct trials (sum of acc column) across trial attempts. 
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
  summarise(mean_accuracy = mean(average_accuracy))
mean_accuracy_src_df

#standard error of the mean for each src...
library(plotrix)

#find standard error of the mean for each src
sem_values <- tapply(average_trial_accuracy_df$average_accuracy, average_trial_accuracy_df$src, std.error)
sem_df <- data.frame(src = names(sem_values), sem = unlist(sem_values))
sem_df

#merge sem_df with mean_accuracy_src_df
mean_acc_sem_df <- merge(mean_accuracy_src_df, sem_df, by = "src")
mean_acc_sem_df


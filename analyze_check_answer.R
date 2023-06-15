library(tidyverse)
library(psycho)

check_answer_df

check_answer_df_1 <- check_answer_df %>%
  group_by(subject) %>%
  slice(1)
check_answer_df_1

#clipr::write_clip(check_answer_df_1)   

#CREATE LISTS OF THE THREAT AND NEUTRAL SLOTS FOR EACH SUBJECT

# initialize an empty list for each subject for which of the slots are threat slots and which are neutral slots
threat_slots_list <- list()
neutral_slots_list <- list()
for (subject in unique(check_answer_df$subject)) {
  slots_with_threat <- c()
  slots_with_neutral <- c()
  
  #look through correct type column for each slot
  for (slot_col in grep("slot[0-9]+CorrectType", colnames(check_answer_df), value = TRUE)) {
    CorrectType <- unique(check_answer_df[check_answer_df$subject == subject, slot_col])
    #if value is threat, add to threat vector
    if (CorrectType == 'threat') {
      slot_name <- sub("CorrectType", "", slot_col)
      slots_with_threat <- c(slots_with_threat, slot_name)
    #if value is neutral, add to neutral vector
    } else if (CorrectType == 'neutral') {
      slot_name <- sub("CorrectType", "", slot_col)
      slots_with_neutral <- c(slots_with_neutral, slot_name)
    }
  }
  threat_slots_list[[subject]] <- slots_with_threat
  neutral_slots_list[[subject]] <- slots_with_neutral
}
threat_slots_list
neutral_slots_list


#MANIPULATE THE DATA FRAME SO IT IS IN THE DESIRED FORMAT

# Calculate the percentage of accuracy, for one slot
#accuracy_df <- check_answer_df %>%
  #group_by(subject, trialCount) %>%
  #summarize(accuracy = sum(slot0Acc == 1) / max(trialAttempt) * 100)
#accuracy_df

#find the accuracy values for each slot for each trial for each subject
check_answer_df_2 <- check_answer_df %>%
  select(subject, trialCount, trialAttempt, slot0Acc, slot1Acc, slot2Acc, slot3Acc, slot4Acc, slot5Acc, slot6Acc, slot7Acc, slot8Acc, slot9Acc)

accuracy_df <- check_answer_df_2 %>%
  group_by(subject, trialCount) %>%
  summarize(across(starts_with('slot'), ~sum(. == 1) / max(trialAttempt) * 100, .names = "accuracy_slot{str_extract(.col, '\\\\d+')}"))
accuracy_df


#make a list of separate data frames in a list for each subject
#unique subjects
unique_subjects <- unique(accuracy_df$subject)

accuracy_df_list <- list()

#iterate through each subject and make a data frame of accuracy just for that subject, then append it to the final list
for (subject in unique_subjects) {
  accuracy_df_subject <- accuracy_df[accuracy_df$subject == subject, ]
  accuracy_df_list[[subject]] <- accuracy_df_subject
}
accuracy_df_list


#CHANGE THE NAMES OF THE COLUMNS TO THREAT OR NEUTRAL CORRESPONDING TO LISTS OF THREAT AND NEUTRAL SLOTS FOR EACH SUBJECT GENERATED EARLIER

#iterate through each subject's accuracy data frame
for (subject in unique_subjects) {
  accuracy_df_subject <- accuracy_df[accuracy_df$subject == subject, ]
  #access the corresponding threat and neutral slots for each subject
  threat_slots <- threat_slots_list[[subject]]
  neutral_slots <- neutral_slots_list[[subject]]
  #iterate through the columns 3-12 of the accuracy data frame and replace the column names with threat or neutral corresponding to the threat and neutral slots for that subject
  for (i in 3:12) {
    col_name <- names(accuracy_df_subject)[i]
    #check if that column (that slot) is a threat slot
    if (any(grepl(paste0("slot", i - 3), threat_slots))) {
      #replace the column name with 'threat'if so
      names(accuracy_df_subject)[i] <- "threat"
    }
    #check if that column (that slot) is a neutral slot
    else if (any(grepl(paste0("slot", i - 3), neutral_slots))) {
      # replace the column name with 'neutral' if so
      names(accuracy_df_subject)[i] <- "neutral"
    }
  }
  #add the data frame that results from each subject to a list of the data frames for all subjects, making accuracy_df_list have the correct column names
  accuracy_df_list[[subject]] <- accuracy_df_subject
}
accuracy_df_list


#AVERAGE THE THREAT & NEUTRAL SLOTS TOGETHER

#initilaize empty data frame
avg_check_answer_acc_df <- data.frame(subject = character(),
                       trialCount = integer(),
                       threat = numeric(),
                       neutral = numeric(),
                       stringsAsFactors = FALSE)

#iterate through the subjects
for (subject in unique_subjects) {
  accuracy_df_subject <- accuracy_df_list[[subject]]
  #calculate average for threat and neutral columns
  avg_threat <- rowMeans(accuracy_df_subject[, grepl("threat", names(accuracy_df_subject))])
  avg_neutral <- rowMeans(accuracy_df_subject[, grepl("neutral", names(accuracy_df_subject))])
  trialCount <- accuracy_df_subject$trialCount
  #initialize a data frame for the current subject
  subject_df <- data.frame(subject = subject,
                           trialCount = trialCount,
                           threat = avg_threat,
                           neutral = avg_neutral,
                           stringsAsFactors = FALSE)
  #add the current subject's data frame to the final data frame
  avg_check_answer_acc_df <- rbind(avg_check_answer_acc_df, subject_df)
}
avg_check_answer_acc_df

#split the avg_check_answer_acc_df into different data frames for each subject
#initialize empty list
avg_check_answer_acc_df_list <- list()
#iterate through each subject & add their data frame to the list
for (subject in unique_subjects) {
  subject_df <- avg_check_answer_acc_df[avg_check_answer_acc_df$subject == subject, ]
  avg_check_answer_acc_df_list[[subject]] <- subject_df
}
avg_check_answer_acc_df_list






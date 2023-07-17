library(tidyverse)
library(psycho)

check_answer_df

check_answer_df_1 <- check_answer_df %>%
  group_by(subject) %>%
  slice(1)
check_answer_df_1


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

###works
# Iterate through each subject and replace the column names in accuracy_df_list
for (subject in unique_subjects) {
  accuracy_df_subject <- accuracy_df_list[[subject]]
  
  # Get the corresponding slot CorrectSRC values for the subject
  slot_correct_src <- check_answer_df[check_answer_df$subject == subject, paste0("slot", seq(0, 9), "CorrectSRC")]
  
  # Rename the columns in accuracy_df_subject with the corresponding slot CorrectSRC values
  for (i in seq_along(slot_correct_src)) {
    col_name <- paste0("accuracy_slot", i - 1)
    new_col_name <- paste0("slot", i - 1, slot_correct_src[i])
    colnames(accuracy_df_subject)[colnames(accuracy_df_subject) == col_name] <- new_col_name
  }
  
  accuracy_df_list[[subject]] <- accuracy_df_subject
}

accuracy_df_list

# Iterate through each subject and modify the column names in accuracy_df_list
for (subject in unique_subjects) {
  accuracy_df_subject <- accuracy_df_list[[subject]]
  colnames(accuracy_df_subject)[3:length(colnames(accuracy_df_subject))] <- sapply(colnames(accuracy_df_subject)[3:length(colnames(accuracy_df_subject))], function(col_name) {
    slot <- sub(".*slot(\\d+).*", "slot\\1", col_name)
    picture <- sub('.*\\("([^"]+)".*', "\\1", col_name)
    paste0(slot, "_", picture)
  })
  accuracy_df_list[[subject]] <- accuracy_df_subject
}

accuracy_df_list


#create a data frame that has the first trialCount in which each subject got each image correct 100% across all trial attempts for the first time

# Create an empty list to store the new data frames
new_df_list <- list()

# Iterate over each data frame in accuracy_df_list
for (i in seq_along(accuracy_df_list)) {
  df <- accuracy_df_list[[i]]
  subject <- df$subject[1]  # Get the subject name
  
  # Create a new data frame for the subject
  new_df <- data.frame(trialCount_accurate = numeric(),
                       image = character(),
                       stringsAsFactors = FALSE)
  
  # Iterate over the columns starting from the third column
  for (col in colnames(df)[3:length(df)]) {
    image <- col  # Get the column name
    row_index <- which(df[[col]] == 100)[1]  # Find the first row index where the value is 100
    
    # Get the value for 'trialCount' in the same row index
    trial_count_accurate <- df$trialCount[row_index]
    
    # Append a new row to the new data frame
    new_row <- data.frame(trialCount_accurate = trial_count_accurate,
                          image = image,
                          stringsAsFactors = FALSE)
    new_df <- rbind(new_df, new_row)
  }
  
  # Add the new data frame to the list
  new_df_list[[i]] <- new_df
  
  # Print the new data frame for the subject
  cat("Subject:", subject, "\n")
  print(new_df)
  cat("\n")
}

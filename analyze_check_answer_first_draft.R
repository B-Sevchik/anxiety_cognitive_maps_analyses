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




#TRY TO CREATE LIST YOU WANT

# Calculate the percentage of accuracy, for one slot
accuracy_df <- check_answer_df %>%
  group_by(subject, trialCount) %>%
  summarize(accuracy = sum(slot0Acc == 1) / max(trialAttempt) * 100)

# Print the accuracy data frame
print(accuracy_df)

#### THIS SEEMS TO BE WORKING BUT CHECK IT BY HAND

check_answer_df_2 <- check_answer_df %>%
  select(subject, trialCount, trialAttempt, slot0Acc, slot1Acc, slot2Acc, slot3Acc, slot4Acc, slot5Acc, slot6Acc, slot7Acc, slot8Acc, slot9Acc)

accuracy_df <- check_answer_df_2 %>%
  group_by(subject, trialCount) %>%
  summarize(across(starts_with('slot'), ~sum(. == 1) / max(trialAttempt) * 100, .names = "accuracy_slot{str_extract(.col, '\\\\d+')}"))
accuracy_df
###

#separate out into different data frames for each subject
# Get unique subjects
unique_subjects <- unique(accuracy_df$subject)

# Create a list to store the separated dataframes
accuracy_df_list <- list()

# Iterate through each subject
for (subject in unique_subjects) {
  # Subset accuracy_df for the current subject
  accuracy_df_subject <- accuracy_df[accuracy_df$subject == subject, ]
  
  # Assign the subsetted dataframe to the list
  accuracy_df_list[[subject]] <- accuracy_df_subject
}
accuracy_df_list
###

#iterate through columns 3 - 12, corresponding to accuracy_slot#

#if the ending of the column name (slot0, for example) is in the threat_slots_list for that subject, replace the column name with threat

#if the ending of the column name is in the neutral_slots_list for that subject, replace the column name with neutral


# Iterate through each subject
for (subject in unique_subjects) {
  # Subset accuracy_df for the current subject
  accuracy_df_subject <- accuracy_df[accuracy_df$subject == subject, ]
  
  # Get the corresponding threat and neutral slots for the current subject
  threat_slots <- threat_slots_list[[subject]]
  neutral_slots <- neutral_slots_list[[subject]]
  
  # Iterate through the columns 3-12 and replace the column names
  for (i in 3:12) {
    # Get the current column name
    col_name <- names(accuracy_df_subject)[i]
    
    # Check if the ending of the column name is in threat_slots
    if (any(grepl(paste0("slot", i - 3), threat_slots))) {
      # Replace the column name with "threat"
      names(accuracy_df_subject)[i] <- "threat"
    }
    # Check if the ending of the column name is in neutral_slots
    else if (any(grepl(paste0("slot", i - 3), neutral_slots))) {
      # Replace the column name with "neutral"
      names(accuracy_df_subject)[i] <- "neutral"
    }
  }
  
  # Assign the modified dataframe to the list
  accuracy_df_list[[subject]] <- accuracy_df_subject
}
accuracy_df_list


####WORKS UP FROM HERE



#for each subject, average the threat columns together and the neutral columns together for each trial such that the final data frame has the columns subject, trialCount, threat, and neutral 

# Create an empty data frame to store the final result
final_df <- data.frame(subject = character(),
                       trialCount = integer(),
                       threat = numeric(),
                       neutral = numeric(),
                       stringsAsFactors = FALSE)

# Iterate through each subject
for (subject in unique_subjects) {
  # Subset accuracy_df_list for the current subject
  accuracy_df_subject <- accuracy_df_list[[subject]]
  
  # Calculate the average for the threat and neutral columns
  avg_threat <- rowMeans(accuracy_df_subject[, grepl("threat", names(accuracy_df_subject))])
  avg_neutral <- rowMeans(accuracy_df_subject[, grepl("neutral", names(accuracy_df_subject))])
  
  # Get the trialCount column
  trialCount <- accuracy_df_subject$trialCount
  
  # Create a data frame for the current subject
  subject_df <- data.frame(subject = subject,
                           trialCount = trialCount,
                           threat = avg_threat,
                           neutral = avg_neutral,
                           stringsAsFactors = FALSE)
  
  # Append the subject data frame to the final data frame
  final_df <- rbind(final_df, subject_df)
}

# Print the final data frame
final_df


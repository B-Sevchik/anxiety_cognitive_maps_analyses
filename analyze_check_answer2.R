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
check_answer_df_2


#make a list of separate data frames in a list for each subject
#unique subjects
unique_subjects <- unique(check_answer_df_2$subject)

check_answer_df_list <- list()

#iterate through each subject and make a data frame just for that subject, then append it to the final list
for (subject in unique_subjects) {
  check_answer_df_subject <- check_answer_df_2[check_answer_df_2$subject == subject, ]
  check_answer_df_list[[subject]] <- check_answer_df_subject
}
check_answer_df_list
#following code commented out, but to view a data frame within the list (in this case, the first data frame), for testing purposes
#options(max.print = Inf)
#View(check_answer_df_list[[2]])


#CHANGE THE NAMES OF THE COLUMNS TO THREAT OR NEUTRAL CORRESPONDING TO LISTS OF THREAT AND NEUTRAL SLOTS FOR EACH SUBJECT GENERATED EARLIER
#iterate through each subject's check answer data frame
for (subject in unique_subjects) {
  check_answer_df_subject <- check_answer_df_2[check_answer_df_2$subject == subject, ]
  #access the corresponding threat and neutral slots for each subject
  threat_slots <- threat_slots_list[[subject]]
  neutral_slots <- neutral_slots_list[[subject]]
  #iterate through the columns 4-13 of the check answer data frame and replace the column names with threat or neutral corresponding to the threat and neutral slots for that subject
  for (i in 4:13) {
    col_name <- names(check_answer_df_subject)[i]
    #check if that column (that slot) is a threat slot
    if (any(grepl(paste0("slot", i - 4), threat_slots))) {
      #replace the column name with 'threat' if so
      names(check_answer_df_subject)[i] <- "threat"
    }
    #check if that column (that slot) is a neutral slot
    else if (any(grepl(paste0("slot", i - 4), neutral_slots))) {
      # replace the column name with 'neutral' if so
      names(check_answer_df_subject)[i] <- "neutral"
    }
  }
  #add the data frame that results from each subject to a list of the data frames for all subjects, making check_answer_df_list have the correct column names
  check_answer_df_list[[subject]] <- check_answer_df_subject
}
check_answer_df_list

#CREATE NEW DATA FRAME SHOWING, FOR EACH SUBJECT, WHICH WAS FIRST TO BE ALL CORRECT (THREAT OR NEUTRAL)?

# Initialize an empty list for the resulting data frames
first_correct_df_list <- list()

#iterate through each subject
for (subject in unique_subjects) {
  check_answer_df_subject <- check_answer_df_list[[subject]]
  # Initialize an empty data frame for the current subject
  first_correct_df_subject <- data.frame(trial = integer(),
                                  which_was_first = character(),
                                  stringsAsFactors = FALSE)
  
  # Iterate through each trial
  for (trial in unique(check_answer_df_subject$trialCount)) {
    check_answer_trial_df <- check_answer_df_subject[check_answer_df_subject$trialCount == trial, ]
    row_threat <- which(rowSums(check_answer_trial_df[, grepl("threat", names(check_answer_trial_df))]) == sum(grepl("threat", names(check_answer_trial_df))))[1]
    row_neutral <- which(rowSums(check_answer_trial_df[, grepl("neutral", names(check_answer_trial_df))]) == sum(grepl("neutral", names(check_answer_trial_df))))[1]
    
    # Determine the value of 'which was first' based on the conditions
    if (row_threat < row_neutral) {
      which_was_first <- "threat"
    } else if (row_threat > row_neutral) {
      which_was_first <- "neutral"
    } else {
      which_was_first <- "equal"
    }
    
    # Add the current trial information to the subject's data frame
    first_correct_df_subject <- rbind(first_correct_df_subject, data.frame(trial = trial, which_was_first = which_was_first))
  }
  
  # Add the subject's data frame to the list of resulting data frames
  first_correct_df_list[[subject]] <- first_correct_df_subject
}
first_correct_df_list


#CALCULATE PERCENTAGE OF THREAT FIRST ALL CORRECT VS. NEUTRAL FIRST ALL CORRECT FOR EACH SUBJECT

#initialize empty vectors
percent_threat <- numeric()
percent_neutral <- numeric()
percent_equal <- numeric()

#iterate through each subject
for (subject in unique_subjects) {
  first_correct_df_subject <- first_correct_df_list[[subject]]

  #calculate number of trials for each subject
  num_rows_subject <- nrow(first_correct_df_subject)
  
  #count number, then percent of times participant got threat or neutral all correct first, or equal
  num_threat_subject <- sum(first_correct_df_subject$which_was_first == 'threat')
  percent_threat_subject <- ((num_threat_subject/num_rows_subject) * 100)
  num_neutral_subject <- sum(first_correct_df_subject$which_was_first == 'neutral')
  percent_neutral_subject <- ((num_neutral_subject/num_rows_subject) * 100)
  num_equal_subject <- sum(first_correct_df_subject$which_was_first == 'equal')
  percent_equal_subject <- ((num_equal_subject/num_rows_subject) * 100)
  
  #add percent results to vectors
  percent_threat <- c(percent_threat, percent_threat_subject)
  percent_neutral <- c(percent_neutral, percent_neutral_subject)
  percent_equal <- c(percent_equal, percent_equal_subject)
}

#create a data frame with subject and percent of times each subject got threat or neutral all correct first or equal
check_answer_percent_first_df <- data.frame(subject = unique_subjects, percent_threat = percent_threat, percent_neutral = percent_neutral, percent_equal = percent_equal)

check_answer_percent_first_df



#SECOND VERSION JUST CONSIDERING THE FIRST TRIAL

first_correct_df_list

#initialize new list
first_row_df_list <- list()

#iterate through each subject, take just the data from the first trial
for (subject in unique_subjects) {
  df <- first_correct_df_list[[subject]]
  first_row_df <- slice(df, 1)  # Slice the data frame to include only the first row
  first_row_df_list[[subject]] <- first_row_df  # Add the sliced data frame to the new list
}

first_row_df_list


#combine sliced data frames into a single data frame
combined_df <- bind_rows(first_row_df_list, .id = "subject")

combined_first_row_df

#CALCULATE PERCENTAGE

#calculate number of subjects
num_subjects <- nrow(combined_first_row_df)

num_threat <- sum(combined_first_row_df$which_was_first == 'threat')
percent_threat <- ((num_threat/num_subjects) * 100)
num_neutral <- sum(combined_first_row_df$which_was_first == 'neutral')
percent_neutral <- ((num_neutral/num_subjects) * 100)
num_equal <- sum(combined_first_row_df$which_was_first == 'equal')
percent_threat <- ((num_equal/num_subjects) * 100)

check_answer_percent_first_df_trial1 <- data.frame(percent_threat = percent_threat, percent_neutral = percent_neutral, percent_equal = percent_equal)

check_answer_percent_first_df_trial1








library(tidyverse)
library(psycho)
library(dplyr)

check_answer_df

#make a new df that has the columns 'subject', 'slot_key', and 'correct_object' so that you have a record of which slot corresponds to which 
slot_key_df <- check_answer_df %>%
  select('subject', 'slot0CorrectType', 'slot0CorrectSRC', 'slot1CorrectType', 'slot1CorrectSRC', 'slot2CorrectType', 'slot2CorrectSRC',
         'slot3CorrectType', 'slot3CorrectSRC', 'slot4CorrectType', 'slot4CorrectSRC', 'slot5CorrectType', 'slot5CorrectSRC', 'slot6CorrectType',
         'slot6CorrectSRC', 'slot7CorrectType', 'slot7CorrectSRC', 'slot8CorrectType', 'slot8CorrectSRC', 'slot9CorrectType', 'slot9CorrectSRC') %>%
  filter(!duplicated(subject))
slot_key_df


new_slot_key_1_df <- slot_key_df %>%
  select(c('subject', ends_with("CorrectSRC"))) %>%
  pivot_longer(cols = starts_with("slot"), names_to = 'slot_key', values_to = 'correct_pic') %>%
  mutate(slot_key = substr(slot_key, 1, 5))
new_slot_key_1_df


new_slot_key_2_df <- slot_key_df %>%
  select(c('subject', ends_with("CorrectType"))) %>%
  pivot_longer(cols = starts_with("slot"), names_to = 'slot_key', values_to = 'correct_type') %>%
  mutate(slot_key = substr(slot_key, 1, 5))
new_slot_key_2_df


new_slot_key_combined_df <- new_slot_key_1_df %>%
  full_join(new_slot_key_2_df, by=c('subject', 'slot_key')) %>%
  mutate(correct_object = paste(correct_pic, correct_type, sep = "_")) %>%
  select('subject', 'slot_key', 'correct_object')
new_slot_key_combined_df


###now, we need to combine that with what we got before as in the 'subject', 'trial when it was accurate', and 'image by slot'

check_answer_df

check_answer_df_1 <- check_answer_df %>%
  group_by(subject) %>%
  slice(1)
check_answer_df_1

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

# Create a new list to store the combined DataFrames with subject information
combined_df_list <- list()

# Iterate over each subject and corresponding new_df
for (i in seq_along(new_df_list)) {
  subject <- unique_subjects[i]
  new_df <- new_df_list[[i]]
  
  # Add a 'subject' column to the new_df
  new_df$subject <- subject
  
  # Change the column name 'image' to 'slot_key'
  colnames(new_df)[colnames(new_df) == "image"] <- "slot_key"
  
  # Transform values in the 'slot_key' column
  new_df$slot_key <- sub(".*_", "", new_df$slot_key)
  
  # Add the modified new_df to the combined_df_list
  combined_df_list[[i]] <- new_df
}

# Combine the separate DataFrames with subject information into one
combine_df_trial_acc <- do.call(rbind, combined_df_list)

# Print the combined DataFrame
print(combine_df_trial_acc)



###

print(new_slot_key_combined_df)

#once we have this data frame with the columns 'subject', 'trialCount_accurate' (the trial count the subject first got that slot key 100% right the first time), and 'slot_key' (like slot0, slot1, etc.),
#we need to now change slot_key such that it is telling us the image & image type rather than the slot for each subject.

merged_correct_object_df <- combine_df_trial_acc %>%
  full_join(new_slot_key_combined_df, by=c('subject', 'slot_key'))
merged_correct_object_df

#add column object_type that tells if object was threat or neutral for comparisons
modified_merged_object_df <- merged_correct_object_df %>%
  mutate(object_type = str_extract(correct_object, "(?<=_)\\w+$"))
modified_merged_object_df

#changing up organization/naming

merged_object_df_organized <- subset(modified_merged_object_df, select = -slot_key)
merged_object_df_organized

names(merged_object_df_organized)[names(merged_object_df_organized) == "object_type"] <- "threat_type"
names(merged_object_df_organized)[names(merged_object_df_organized) == "correct_object"] <- "image"

merged_object_df_organized

### good up till here

average_merged_object_df <- merged_object_df_organized %>%
  group_by(image) %>%
  summarize(average_of_averages = mean(trialCount_accurate, na.rm = TRUE))
average_merged_object_df

#create data frame key to rename picture#.jpg values as recognizable names

image <- c('picture1.jpg_neutral', 'picture1.jpg_threat', 'picture10.jpg_neutral', 
           'picture2.jpg_neutral', 'picture2.jpg_threat', 
           'picture3.jpg_neutral', 'picture3.jpg_threat',
           'picture4.jpg_neutral', 'picture4.jpg_threat',
           'picture5.jpg_neutral', 'picture6.jpg_neutral',
           'picture7.jpg_neutral', 'picture8.jpg_neutral',
           'picture9.jpg_neutral')
image_name <- c('paper', 'knife', 'dog', 'water_bottles', 'gun', 'fire_hydrant', 'snake', 'mugs', 'spider', 'keyboard', 'stapler', 'acorns', 'pigeon', 'skyscraper')

key_image_name_df <- data.frame(image = image, image_name = image_name)
key_image_name_df


average_merged_name_df <- merge(average_merged_object_df, key_image_name_df, by = "image", all = TRUE)
average_merged_name_df

average_merged_name_df_organized <- subset(average_merged_name_df, select = -image)
average_merged_name_df_organized
















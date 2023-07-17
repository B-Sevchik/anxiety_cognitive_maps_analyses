##PAIRED T TEST
threat_percent_subjects_df

t.test(threat_percent_subjects_df$percentage_chosenThreatStatus ~ threat_percent_subjects_df$chosenThreatStatus, mu = 0,
       alternative = "greater",
       paired = FALSE,
       var.equal = FALSE,
       conf.level = 0.95)



##ANOVA TO ADD STAI SCORES IN
#*NOTE: run the second subject_dprime_df
threat_percent_anxiety_df

# Create a new data frame with the desired groups
grouped_data <- subset(threat_percent_anxiety_df, 
                       (chosenThreatStatus == "threat" & 
                          anxiety_level %in% c("high trait anxiety", "low trait anxiety", "moderate trait anxiety")) |
                         (chosenThreatStatus == "neutral" & 
                            anxiety_level %in% c("high trait anxiety", "low trait anxiety", "moderate trait anxiety")))

# Perform ANOVA
model <- aov(percentage_chosenThreatStatus ~ chosenThreatStatus * anxiety_level, data = grouped_data)

# Print the ANOVA table
summary(model)
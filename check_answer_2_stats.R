check_answer_percent_summary

#just include threat & neutral to compare, take out the equal
check_answer_percent_summary_tn <- check_answer_percent_summary %>%
  filter(threat_status != 'equal')
check_answer_percent_summary_tn


#group the data by threat_status and calculate the mean percentage that that threat type is 100% first
mean_percentage_first_df <- check_answer_percent_summary %>%
  group_by(threat_status) %>%
  summarize(mean_percentage_first = mean(percentage))
mean_percentage_first_df


#t-test comparing mean_percentage_first for threat vs. neutral
t.test(check_answer_percent_summary_tn$percentage ~ check_answer_percent_summary_tn$threat_status, mu = 0,
       alternative = "greater",
       paired = FALSE,
       var.equal = FALSE,
       conf.level = 0.95)



##ANOVA TO ADD STAI SCORES IN
check_answer_percent_summary_STAI

#just include threat & neutral to compare, take out the equal
check_answer_percent_summary_STAI_tn <- check_answer_percent_summary_STAI %>%
  filter(threat_status != 'equal')
check_answer_percent_summary_STAI_tn


# Create a new data frame with the desired groups
grouped_data <- subset(check_answer_percent_summary_STAI_tn, 
                       (threat_status == "threat" & 
                          anxiety_level %in% c("high trait anxiety", "low trait anxiety", "moderate trait anxiety")) |
                         (threat_status == "neutral" & 
                            anxiety_level %in% c("high trait anxiety", "low trait anxiety", "moderate trait anxiety")))

# Perform ANOVA
model <- aov(percentage ~ threat_status * anxiety_level, data = grouped_data)

# Print the ANOVA table
summary(model)





##PAIRED T TEST
percentage_drop_plot_acc_df


t.test(percentage_drop_plot_acc_df$percentage ~ percentage_drop_plot_acc_df$threat_status, mu = 0,
       alternative = "greater",
       paired = FALSE,
       var.equal = FALSE,
       conf.level = 0.95)


##ANOVA TO ADD STAI SCORES IN
#*NOTE: run the second subject_dprime_df
percentage_drop_plot_acc_df
# Create a new data frame with the desired groups
grouped_data <- subset(percentage_drop_plot_acc_df, 
                       (tthreat_status == "threatt" & 
                          anxiety_level %in% c("high trait anxiety", "low trait anxiety", "moderate trait anxiety")) |
                         (threat_status == "neutral" & 
                            anxiety_level %in% c("high trait anxiety", "low trait anxiety", "moderate trait anxiety")))

# Perform ANOVA
model <- aov(dprime ~ transition_threat * anxiety_level, data = grouped_data)

# Print the ANOVA table
summary(model)

library(tidyverse)
library(dplyr)

#BASIC MANIPULATION
STAI_correlation_df <- read_csv('/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/All participants/STAI_scores_calculated.csv')

# Create just the STAI score, subject, and anxiety level
STAI_correlation_df <- STAI_correlation_df %>%
  select(subjectID, sumVals, anxiety_level)
STAI_correlation_df

# Change column name 'subjectID' to 'subject' for merging
colnames(STAI_correlation_df)[1] <- "subject"
STAI_correlation_df

#DROP
# Load df with drop event stuff you need
percentage_drop_plot_df

#merge the two dfs together to get the df you need for this linear correlation
drop_anxiety_correlation_df <- merge(percentage_drop_plot_df, STAI_correlation_df, by = "subject", all = TRUE)
drop_anxiety_correlation_df

#create a df for just threat and a df for just neutral
drop_anxiety_correlation_df_threat <- drop_anxiety_correlation_df %>%
  filter(threat_status == 'threat')
drop_anxiety_correlation_df_threat

drop_anxiety_correlation_df_neutral <- drop_anxiety_correlation_df %>%
  filter(threat_status == 'neutral')
drop_anxiety_correlation_df_neutral

#linear model stuff for correlation - mean_acc and sumVals
#threat
#initial model
linear_model_drop_threat <- lm(percentage ~ sumVals, data = drop_anxiety_correlation_df_threat)
summary(linear_model_drop_threat)

linear_model_drop_neutral <- lm(percentage ~ sumVals, data = drop_anxiety_correlation_df_neutral)
summary(linear_model_drop_neutral)

#evaluating assumptions?????


#SCATTERPLOT THREAT
# Extract sumVals and percentage variables
x <- drop_anxiety_correlation_df_threat$sumVals
y <- drop_anxiety_correlation_df_threat$percentage

# Generate predicted values
predicted <- predict(linear_model_drop_threat)

# Create scatter plot with line of best fit
plot(x, y, main = "Threat Scatterplot", xlab = "sumVals", ylab = "percentage", pch = 16, col = "blue")
abline(linear_model_drop_threat, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = drop_anxiety_correlation_df_threat) +
  geom_point(aes(x = sumVals, y = percentage), color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = percentage), method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "Threat Scatterplot", x = "sumVals", y = "percentage")

#SCATTERPLOT NEUTRAL
# Extract sumVals and percentage variables
x <- drop_anxiety_correlation_df_neutral$sumVals
y <- drop_anxiety_correlation_df_neutral$percentage

# Generate predicted values
predicted <- predict(linear_model_drop_threat)

# Create scatter plot with line of best fit
plot(x, y, main = "Threat Scatterplot", xlab = "sumVals", ylab = "percentage", pch = 16, col = "blue")
abline(linear_model_drop_neutral, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = drop_anxiety_correlation_df_neutral) +
  geom_point(aes(x = sumVals, y = percentage), color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = percentage), method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "Neutral Scatterplot", x = "sumVals", y = "percentage")



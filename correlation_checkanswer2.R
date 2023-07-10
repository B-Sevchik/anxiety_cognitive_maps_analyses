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

#CHECK ANSWER
# Load df with check answer event stuff you need
check_answer_percent_summary

#merge the two dfs together to get the df you need for this linear correlation
check_answer_anxiety_correlation_df <- merge(check_answer_percent_summary, STAI_correlation_df, by = "subject", all = TRUE)
check_answer_anxiety_correlation_df

#create a df for each type of threat status (neutral, threat, and equal)
check_answer_anxiety_correlation_df_threat <- check_answer_anxiety_correlation_df %>%
  filter(threat_status == 'threat')
check_answer_anxiety_correlation_df_threat

check_answer_anxiety_correlation_df_neutral <- check_answer_anxiety_correlation_df %>%
  filter(threat_status == 'neutral')
check_answer_anxiety_correlation_df_neutral

check_answer_anxiety_correlation_df_equal <- check_answer_anxiety_correlation_df %>%
  filter(threat_status == 'equal')
check_answer_anxiety_correlation_df_equal

#linear model stuff for correlation - mean_acc and sumVals
#threat
#initial model
linear_model_check_answer_threat <- lm(percentage ~ sumVals, data = check_answer_anxiety_correlation_df_threat)
summary(linear_model_check_answer_threat)

linear_model_check_answer_neutral <- lm(percentage ~ sumVals, data = check_answer_anxiety_correlation_df_neutral)
summary(linear_model_check_answer_neutral)

linear_model_check_answer_equal <- lm(percentage ~ sumVals, data = check_answer_anxiety_correlation_df_equal)
summary(linear_model_check_answer_equal)

#evaluating assumptions?????


#SCATTERPLOT THREAT
# Extract sumVals and percentage variables
x <- check_answer_anxiety_correlation_df_threat$sumVals
y <- check_answer_anxiety_correlation_df_threat$percentage

# Generate predicted values
predicted <- predict(linear_model_check_answer_threat)

# Create scatter plot with line of best fit
plot(x, y, main = "Threat Scatterplot", xlab = "sumVals", ylab = "percentage", pch = 16, col = "blue")
abline(linear_model_check_answer_threat, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = check_answer_anxiety_correlation_df_threat) +
  geom_point(aes(x = sumVals, y = percentage), color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = percentage), method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "Threat Scatterplot", x = "sumVals", y = "percentage")

#SCATTERPLOT NEUTRAL
# Extract sumVals and percentage variables
x <- check_answer_anxiety_correlation_df_neutral$sumVals
y <- check_answer_anxiety_correlation_df_neutral$percentage

# Generate predicted values
predicted <- predict(linear_model_check_answer_neutral)

# Create scatter plot with line of best fit
plot(x, y, main = "Threat Scatterplot", xlab = "sumVals", ylab = "percentage", pch = 16, col = "blue")
abline(linear_model_check_answer_neutral, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = check_answer_anxiety_correlation_df_neutral) +
  geom_point(aes(x = sumVals, y = percentage), color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = percentage), method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "Neutral Scatterplot", x = "sumVals", y = "percentage")


#SCATTERPLOT EQUAL
# Extract sumVals and percentage variables
x <- check_answer_anxiety_correlation_df_equal$sumVals
y <- check_answer_anxiety_correlation_df_equal$percentage

# Generate predicted values
predicted <- predict(linear_model_check_answer_equal)

# Create scatter plot with line of best fit
plot(x, y, main = "Threat Scatterplot", xlab = "sumVals", ylab = "percentage", pch = 16, col = "blue")
abline(linear_model_check_answer_equal, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = check_answer_anxiety_correlation_df_equal) +
  geom_point(aes(x = sumVals, y = percentage), color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = percentage), method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "Equal Scatterplot", x = "sumVals", y = "percentage")



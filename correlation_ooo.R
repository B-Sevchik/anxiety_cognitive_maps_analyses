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

#OOO
# Load df with ooo stuff you need
subject_acc_ooo_df

#merge the two dfs together to get the df you need for this linear correlation
ooo_anxiety_correlation_df <- merge(subject_acc_ooo_df, STAI_correlation_df, by = "subject", all = TRUE)
ooo_anxiety_correlation_df

#create a df for just threat and a df for just neutral
ooo_anxiety_correlation_df_threat <- ooo_anxiety_correlation_df %>%
  filter(chosenThreatStatus == 'threat')
ooo_anxiety_correlation_df_threat

ooo_anxiety_correlation_df_neutral <- ooo_anxiety_correlation_df %>%
  filter(chosenThreatStatus == 'neutral')
ooo_anxiety_correlation_df_neutral

#linear model stuff for correlation - mean_acc and sumVals
#threat
#initial model
linear_model_ooo_threat <- lm(mean_acc ~ sumVals, data = ooo_anxiety_correlation_df_threat)
summary(linear_model_ooo_threat)

linear_model_ooo_neutral <- lm(mean_acc ~ sumVals, data = ooo_anxiety_correlation_df_neutral)
summary(linear_model_ooo_neutral)

#evaluating assumptions - I honestly don't think these are met!!!!
ggplot(data = mod1, mapping = aes(x = mod1$residuals)) + 
  geom_histogram(color = "darkblue", 
                 fill = "lightgray",
                 binwidth = 1) + 
  labs(x = "Residuals", y = "Count",
       title = "Residuals are slightly left-skewed") + 
  theme_bw()
ggplot(data = mod1, mapping = aes(x = mod1$fitted.values, y = mod1$residuals)) + 
  geom_point() + 
  labs(x = "Fitted values", y = "Residuals",
       title = "Residual plot shows no clear pattern") +
  geom_hline(yintercept = 0, color = "red")

#filtering accuracies below chance (< 0.3)
ooo_anxiety_correlation_df_threat_filtered <- ooo_anxiety_correlation_df_threat %>%
  filter(mean_acc >= 0.3)

ooo_anxiety_correlation_df_neutral_filtered <- ooo_anxiety_correlation_df_neutral %>%
  filter(mean_acc >= 0.3)

#SCATTERPLOT THREAT
# Extract sumVals and mean_acc variables
x <- ooo_anxiety_correlation_df_threat_filtered$sumVals
y <- ooo_anxiety_correlation_df_threat_filtered$mean_acc

# Generate predicted values
predicted <- predict(linear_model_ooo_threat)

# Create scatter plot with line of best fit
plot(x, y, main = "Threat Scatterplot", xlab = "sumVals", ylab = "mean_acc", pch = 16, col = "blue")
abline(linear_model_ooo_threat, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = ooo_anxiety_correlation_df_threat_filtered) +
  geom_point(aes(x = sumVals, y = mean_acc, color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = mean_acc, method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "Threat Scatterplot", x = "sumVals", y = "mean_acc")



#SCATTERPLOT NEUTRAL
# Extract sumVals and mean_acc variables
x <- ooo_anxiety_correlation_df_neutral_filtered$sumVals
y <- ooo_anxiety_correlation_df_neutral_filtered$mean_acc

# Generate predicted values
predicted <- predict(linear_model_ooo_neutral)

# Create scatter plot with line of best fit
plot(x, y, main = "Neutral Scatterplot", xlab = "sumVals", ylab = "mean_acc", pch = 16, col = "blue")
abline(linear_model_ooo_neutral, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = ooo_anxiety_correlation_df_neutral_filtered) +
geom_point(aes(x = sumVals, y = mean_acc, color = "blue", size = 3) +
geom_smooth(aes(x = sumVals, y = mean_acc, method = "lm", se = FALSE, color = "red", lwd = 2) +
labs(title = "Neutral Scatterplot", x = "sumVals", y = "mean_acc")
                           



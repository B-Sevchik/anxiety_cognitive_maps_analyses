###BASIC MANIPULATION

#create just the STAI score and subject and anxiety level
STAI_correlation_df <- STAI_correlation_df <- read_csv('/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/All participants/STAI_scores_calculated.csv') %>%
  select('subjectID', 'sumVals')
STAI_correlation_df

#change column name 'subjectID' to 'subject' so can merge eventually
colnames(STAI_correlation_df)[1] <- "subject"
STAI_correlation_df

####illegal transition 

#load df with ooo stuff you need
subject_dprime_df #make sure you run the NEW illegal transition analysis before this to separate by each threatKind

#merge the two dfs together to get the df you need for this linear correlation
illegal_anxiety_correlation_df <- merge(subject_dprime_df, STAI_correlation_df, by = "subject", all = TRUE)
illegal_anxiety_correlation_df

#create a df for each type of threat transition
illegal_anxiety_correlation_df_nn <- illegal_anxiety_correlation_df %>%
  filter(transitionThreatKind == 'neutral-neutral')
illegal_anxiety_correlation_df_nn

illegal_anxiety_correlation_df_nt <- illegal_anxiety_correlation_df %>%
  filter(transitionThreatKind == 'neutral-threat')
illegal_anxiety_correlation_df_nt

illegal_anxiety_correlation_df_tn <- illegal_anxiety_correlation_df %>%
  filter(transitionThreatKind == 'threat-neutral')
illegal_anxiety_correlation_df_tn

illegal_anxiety_correlation_df_tt <- illegal_anxiety_correlation_df %>%
  filter(transitionThreatKind == 'threat-threat')
illegal_anxiety_correlation_df_tt


#linear model stuff for correlation - mean_acc and sumVals
#initial model
#neutral-neutral
linear_model_illegal_nn <- lm(dprime ~ sumVals, data = illegal_anxiety_correlation_df_nn)
summary(linear_model_illegal_nn)

#neutral-threat
linear_model_illegal_nt <- lm(dprime ~ sumVals, data = illegal_anxiety_correlation_df_nt)
summary(linear_model_illegal_nt)

#threat-neutral
linear_model_illegal_tn <- lm(dprime ~ sumVals, data = illegal_anxiety_correlation_df_tn)
summary(linear_model_illegal_tn)

#threat-threat
linear_model_illegal_tt <- lm(dprime ~ sumVals, data = illegal_anxiety_correlation_df_tt)
summary(linear_model_illegal_tt)

#evaluating assumptions - I honestly don't think these are met!!!! - haven't looked at these
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


#SCATTERPLOT NEUTRAL-NEUTRAL
# Extract sumVals and dprime variables
x <- illegal_anxiety_correlation_df_nn$sumVals
y <- illegal_anxiety_correlation_df_nn$dprime

# Generate predicted values
predicted <- predict(linear_model_illegal_nn)

# Create scatter plot with line of best fit
plot(x, y, main = "Neutral-Neutral Scatterplot", xlab = "sumVals", ylab = "dprime", pch = 16, col = "blue")
abline(linear_model_illegal_nn, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = illegal_anxiety_correlation_df_nn) +
  geom_point(aes(x = sumVals, y = dprime), color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = dprime), method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "Neutral-Neutral Scatterplot", x = "sumVals", y = "dprime")



#SCATTERPLOT NEUTRAL-THREAT
# Extract sumVals and dprime variables
x <- illegal_anxiety_correlation_df_nt$sumVals
y <- illegal_anxiety_correlation_df_nt$dprime

# Generate predicted values
predicted <- predict(linear_model_illegal_nt)

# Create scatter plot with line of best fit
plot(x, y, main = "Neutral-Threat Scatterplot", xlab = "sumVals", ylab = "dprime", pch = 16, col = "blue")
abline(linear_model_illegal_nt, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = illegal_anxiety_correlation_df_nt) +
  geom_point(aes(x = sumVals, y = dprime), color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = dprime), method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "Neutral-Threat Scatterplot", x = "sumVals", y = "dprime")


#SCATTERPLOT THREAT-NEUTRAL
# Extract sumVals and dprime variables
x <- illegal_anxiety_correlation_df_tn$sumVals
y <- illegal_anxiety_correlation_df_tn$dprime

# Generate predicted values
predicted <- predict(linear_model_illegal_tn)

# Create scatter plot with line of best fit
plot(x, y, main = "Threat-Neutral Scatterplot", xlab = "sumVals", ylab = "dprime", pch = 16, col = "blue")
abline(linear_model_illegal_tn, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = illegal_anxiety_correlation_df_tn) +
  geom_point(aes(x = sumVals, y = dprime), color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = dprime), method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "Threat-Neutral Scatterplot", x = "sumVals", y = "dprime")




#SCATTERPLOT THREAT-THREAT
# Extract sumVals and mean_acc variables
x <- illegal_anxiety_correlation_df_tt$sumVals
y <- illegal_anxiety_correlation_df_tt$dprime

# Generate predicted values
predicted <- predict(linear_model_illegal_tt)

# Create scatter plot with line of best fit
plot(x, y, main = "Threat-Threat Scatterplot", xlab = "sumVals", ylab = "dprime", pch = 16, col = "blue")
abline(linear_model_illegal_nn, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = illegal_anxiety_correlation_df_tt) +
  geom_point(aes(x = sumVals, y = dprime), color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = dprime), method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "Threat-Threat Scatterplot", x = "sumVals", y = "dprime")



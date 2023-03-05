#set-up
setwd("~/Documents/GitHub/anxiety_cognitive_maps_analyses")
library(tidyverse)
library(dplyr)
library(tibble)

#path references
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot1/STAIscores.csv'

#load data
df_pilot1 <- read_csv(data_path)

#manipulate df to get it to look how you want
colnames(df_pilot1) <- c('not_included', 'subjectID', 's1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10', 's11', 's12', 's13', 's14', 's15', 's16', 's17', 's18', 's19', 's20')
num_rows = nrow(df_pilot1)
num_rows

df_pilot1 <- df_pilot1 %>%
  slice(2:num_rows) %>%
  select('subjectID', 's1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10', 's11', 's12', 's13', 's14', 's15', 's16', 's17', 's18', 's19', 's20')

#getting scores into numeric form so you can add them together later
print(sapply(df_pilot1, class)) #debugging
df_pilot1 <- transform(df_pilot1,s1 = as.numeric(s1))
print(sapply(df_pilot1$s1, class)) #debugging
df_pilot1 <- transform(df_pilot1,s2 = as.numeric(s2))
df_pilot1 <- transform(df_pilot1,s3 = as.numeric(s3))
df_pilot1 <- transform(df_pilot1,s4 = as.numeric(s4))
df_pilot1 <- transform(df_pilot1,s5 = as.numeric(s5))
df_pilot1 <- transform(df_pilot1,s6 = as.numeric(s6))
df_pilot1 <- transform(df_pilot1,s7 = as.numeric(s7))
df_pilot1 <- transform(df_pilot1,s8 = as.numeric(s8))
df_pilot1 <- transform(df_pilot1,s9 = as.numeric(s9))
df_pilot1 <- transform(df_pilot1,s10 = as.numeric(s10))
df_pilot1 <- transform(df_pilot1,s11 = as.numeric(s11))
df_pilot1 <- transform(df_pilot1,s12 = as.numeric(s12))
df_pilot1 <- transform(df_pilot1,s13 = as.numeric(s13))
df_pilot1 <- transform(df_pilot1,s14 = as.numeric(s14))
df_pilot1 <- transform(df_pilot1,s15 = as.numeric(s15))
df_pilot1 <- transform(df_pilot1,s16 = as.numeric(s16))
df_pilot1 <- transform(df_pilot1,s17 = as.numeric(s17))
df_pilot1 <- transform(df_pilot1,s18 = as.numeric(s18))
df_pilot1 <- transform(df_pilot1,s19 = as.numeric(s19))
df_pilot1 <- transform(df_pilot1,s20 = as.numeric(s20))

#sum STAI score
df_pilot1 <- as_tibble(df_pilot1) %>% 
  group_by(subjectID) %>%
  mutate(sumVals = (5-s1) + s2 + (5-s3) + s4 + s5 + (5-s6) + (5-s7) + s8 + s9 + (5-s10) + s11 + s12 + (5-s13) + (5-s14) + 
           s15 + (5-s16) + s17 + s18 + (5-s19) + s20)

df_pilot1 <- as_tibble(df_pilot1) %>%
  group_by(subjectID) %>%
  mutate(anxiety_level = case_when(
    sumVals < 37 ~ 'low trait anxiety',
    sumVals >= 38 & sumVals < 44 ~ 'moderate trait anxiety',
    sumVals > 44 ~ 'high trait anxiety'
  ))






#TESTING

#set-up
setwd("~/Documents/GitHub/anxiety_cognitive_maps_analyses")
library(tidyverse)
library(dplyr)

#BE SURE TO EDIT PATH NAMES EACH TIME YOU RUN SCRIPT


df_pilot2 <- tibble(subID = c('sub1', 'sub2', 'sub3'),
                    s1 = c(1,3,2),
                    s2 = c(3,4,5),
                    s3 = c(2,4,5))


#changing non-responses (5) into NaN
df_pilot2 <- df_pilot2 %>%
  mutate(across(paste0('s', 1:3), ~ case_when(. == 5 ~ NaN, 
                                               TRUE ~ .)))

#reverse score what needs to be reverse scored - causing an NA row rn! also j not working
df_pilot2 <- df_pilot2 %>%
  group_by(subID) %>%
  mutate(s1 = (5 - s1),
         s3 = (5 - s3))

str(df_pilot2)

#get the mean & replace NaNs w the mean - actually something wrong w this bc causing the error in the sum
#df_pilot2 <- df_pilot2 %>%
  #group_by(subID) %>%
  #mutate(meanVals = rowMeans(select(s1, s2, s3), na.rm = TRUE))


df_pilot2 <- df_pilot2 %>%
  mutate(meanVals = (s1 + s2 + s3)/ 3, na.rm = TRUE) %>%
  mutate(across(paste0('s', 1:3), ~ case_when(. == NaN ~ meanVals, 
                                               TRUE ~ .)))

#sum STAI score
df_pilot2 <- df_pilot2 %>%
  group_by(subID) %>%
  mutate(sumVals = s1 + s2 + s3)

#classify anxiety levels based on STAI sum
df_pilot1 <- df_pilot1 %>%
  group_by(subID) %>%
  mutate(anxiety_level = case_when(
    sumVals < 37 ~ 'low trait anxiety',
    sumVals >= 38 & sumVals < 44 ~ 'moderate trait anxiety',
    sumVals > 44 ~ 'high trait anxiety'
  ))






#save out the file
write.csv(df_pilot1, '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot1/STAI_scores_calculated.csv') #EDIT PATH NAME EACH TIME












######
df_pilot2 <- tibble(subID = c('sub1', 'sub2', 'sub3'),
                    s1 = c(1,3,2),
                    s2 = c(3,4,5),
                    s3 = c(2,4,5))

df_pilot2 <- df_pilot2 %>%
  mutate(across(paste0('s', 1:3), ~ case_when(. == 5 ~ NaN, 
                                               TRUE ~ .)))

df_pilot2 <- df_pilot2 %>%
  group_by(subjectID) %>%
  mutate(s1 = (5 - s1),
         s3 = (5 - s3))


df_pilot2 <- df_pilot2 %>%
  mutate(meanVals = ((s1 + s2 + s3) / 3) 
         %>%
  mutate(across(paste0('s', 1:3), ~ case_when(. == NaN ~ meanVals,
                                               TRUE ~ .)))






#RAPHAEL HELPFUL CODE

df <- tibble(subID = c('sub1', 'sub2', 'sub3'),
             s1 = c(1,3,2),
             s2 = c(3,4,5),
             s3 = c(2,4,5))

df <- df %>% 
  group_by(subID) %>% 
  mutate(sumVals = s1 + (5-s2) + s3)
#END OF RAPHAEL HELPFUL CODE


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





#transpose rows into columns so that the subjects are the columns
df_pilot1 <- t(df_pilot1)
colnames(df_pilot1) <- c('sub1', 'sub2', 'sub3')

#create variable that is the sum
num_rows = nrow(df_pilot1)
num_rows
df_pilot1 %>%
  slice(2)
colSums(df_pilot1_scores)




rowSums(df_pilot_1)

df_pilot1 <- df_pilot1 %>% 
  group_by(subject) %>% 
  summarize(sum_vals = sum(s1 + s2))





mutate(sumVals = (5-s1) + s2)


df_pilot1 %>%
  group_by('0') %>%
  mutate(sumVals = '1' + (5-'2') + '3')


#reverse score data frame
reverse_questions = c('1', '3', '6', '7', '10', '13', '14', '16', '19')
max_score = 4
max_num = (4 + 1)
df_pilot1[ , reverse_questions] = max_num - df_pilot1[ , reverse_questions]
df_pilot1

df_pilot1 <- tibble(subID = c('sub1', 'sub2', 'sub3'),
                    s1 = c(1,3,2),
                    s2 = c(3,4,5),
                    s3 = c(2,4,5))

df_pilot1 %>% 
  group_by(subID) %>% 
  mutate(sumVals = s1 + (5-s2) + s3)


#participant 1 scoring (sum)
participant_1 <- df[ ,c(1)]
participant_1
total = c(sum(participant_1))
total

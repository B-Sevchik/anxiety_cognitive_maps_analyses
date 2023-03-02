#set-up
setwd("~/Documents/GitHub/anxiety_cognitive_maps_analyses")
library(tidyverse)
library(dplyr)

#set up data frame
df <- data.frame(Q1 = c(1,2,3,4),
                 Q2 = c(1,2,3,4),
                 Q3 = c(2,5,4,3),
                 Q4 = c(1,2,4,5),
                 Q5 = c(2,4,5,6))
df

#reverse score data frame
reverse_questions = c("Q1", "Q3")
max_score = 4
max_num = (4 + 1)
df[ , reverse_questions] = max_num - df[ , reverse_questions]
df


#participant 1 scoring (sum)
participant_1 <- df[ ,c(1)]
participant_1
total = c(sum(participant_1))
total

    



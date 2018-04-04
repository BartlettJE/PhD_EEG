#  Load packages
require(tidyverse)
require(readbulk)

#  Read in raw data
rawdata <- read_opensesame(directory = "Raw_data/Behavioural/Go-NoGo/")

rawdata <- rawdata %>% 
  mutate(Subject = substring(File, 0, 4))

#  Streamline data and remove practice trials
rawdata <- rawdata %>%
  select(Block:Trial_N, correct, correct_response, response, response_time, subject_nr, Subject) %>% #Select only the important variables 
  filter(Block != "Practice") #Remove practice trials 

#  Calculate correct responses 
rawdata <- rawdata %>%
  mutate(corr.resp = ifelse(Stim_type == "Go" & response == "x", 1, 
                            ifelse(Stim_type == "NoGo" & response == "None", 1,
                                   0)))

# How many errors and what was the accuracy? 
rawdata %>% 
  group_by(Subject) %>% 
  summarise(Accuracy = mean(corr.resp) * 100,
            Correct = sum(corr.resp),
            Errors = 400 - sum(corr.resp))

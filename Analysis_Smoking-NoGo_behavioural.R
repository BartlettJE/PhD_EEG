#  Load packages
require(tidyverse)
require(readbulk)

#  Read in raw data
rawdata <- read_opensesame(directory = "Raw_data/Behavioural/Smoking-nogo/")

rawdata %>% 
  group_by(File, Block) %>% 
  summarise(N = n())

rawdata <- rawdata %>% 
  mutate(Subject = substring(File, 0, 4))

#  Streamline data and remove practice trials
rawdata <- rawdata %>%
  select(Block:Cue_type, Picture, Stimulus, correct, response, response_time, Subject) %>% #Select only the important variables 
  filter(Block != "Practice") #Remove practice trials 

# How many errors and what was the overall accuracy? 
rawdata %>% 
  group_by(Subject) %>% 
  summarise(Accuracy = mean(correct) * 100,
            Correct = sum(correct),
            N = n())

# Commision errors - responding to nogo trials
rawdata %>% 
  filter(Stimulus == "NoGo") %>% 
  group_by(Subject) %>% 
  summarise(correct_resp = sum(correct),
            N = n(),
            errors = N - correct_resp)

# Omission errors - missing go trials 
rawdata %>% 
  filter(Stimulus == "Go") %>% 
  group_by(Subject) %>% 
  summarise(correct_resp = sum(correct),
            N = n(),
            errors = N - correct_resp)

# RT to go trials 
rawdata %>% 
  filter(Stimulus == "Go" & correct == 1) %>% 
  group_by(Subject) %>% 
  summarise(mean_RT = mean(response_time),
            SD_RT = sd(response_time),
            N_trials = n())


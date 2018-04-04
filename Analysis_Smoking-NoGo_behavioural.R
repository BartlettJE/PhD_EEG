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

# How many errors and what was the accuracy? 
rawdata %>% 
  group_by(Subject, Cue_type) %>% 
  summarise(Accuracy = mean(correct) * 100,
            Correct = sum(correct),
            N = n())


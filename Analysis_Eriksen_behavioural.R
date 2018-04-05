#  Load packages 
require(tidyverse)
require(readbulk)

#  Load raw data 
raw <- read_opensesame(directory = "Raw_data/Behavioural/Eriksen/")

#  Create subject number from file name 
raw <- raw %>% 
  mutate(Subject = substring(File, 0, 4))

#  Process data
#  Make sure congruency and subject number are factors
raw$Congruency <- as.factor(raw$Congruency)
raw$Subject <- as.factor(raw$Subject)

# Calculate mean and SD response by congruency 
raw %>% 
  filter(Block != "Practice") %>% #  Filter practice trials and correct responses - only want incorrect 
  group_by(Congruency) %>% 
  summarise(mean_resp = mean(response_time),
            SD_resp = sd(response_time))

#  Calculate error rates 
raw %>% 
  group_by(Subject) %>% 
  filter(Block != "Practice") %>% 
  summarise(Accuracy = mean(correct) * 100,
            Correct = sum(correct),
            Errors = 400 - sum(correct))

#  Create violin plot 
raw %>% 
  filter(Block != "Practice" & response_time < 1000) %>% 
  ggplot(aes(x = Congruency, y = response_time, fill = Congruency)) + 
  geom_violin() + 
  geom_boxplot(width = 0.2) + 
  theme_classic() + 
  ylab("Mean Response Time (ms)")



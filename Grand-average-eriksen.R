require(R.matlab) #function to be able to read matlab files - EEG data saved from MNE Pythin 
require(tidyverse)
require(pracma) #function to allow the calculation of linear space for plotting 

# Functions to get the average amplitude for each trial type 
get_correct <- function(mat, csv, electrode){
  # Function to calculate the average amplitude for each Go trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for? 
  correct_trials <- csv$correct == 1 #this is only to select correct trials 
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  average_correct <- colMeans(dat_elec[1, correct_trials, ]) #create an average for each go trial 
  return(average_correct*1e6) #returns the average (in micro volts) of each go trial along the number of samples per epoch 
}

get_incorrect <- function(mat, csv, electrode){
  # Function to calculate the average amplitude for each NoGo trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for? 
  incorrect_trials <- csv$correct== 0 #this is only to select NoGo trials
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  average_incorrect <- colMeans(dat_elec[1, incorrect_trials, ]) #Create an average for each NoGo trial 
  return(average_incorrect*1e6) #returns the average (in micro volts) of each nogo trial along the number of samples per epoch 
}

electrode = "Cz"

dat <- readMat("Clean_data/mat_files/Eriksen/700_Eriksen.mat")
trial_info <- read.csv("Raw_data/Behavioural/Eriksen/kieron-eriksen.csv")

correct.erp <- get_correct(mat = dat, csv = trial_info, electrode = electrode)
incorrect.erp <- get_incorrect(mat = dat, csv = trial_info, electrode = electrode)


# all_go <- rbind(P_101_go, P_700_go, P_800_go, P_901_go)
# all_nogo <- rbind(P_101_nogo, P_700_nogo, P_800_nogo, P_901_nogo)
# grand_go <- colMeans(all_go) 
# grand_nogo <- colMeans(all_nogo)

x = linspace(-200,800,1025)

ggplot() + 
  geom_line(mapping = aes(x = x, y = correct.erp), color = "green") + 
  geom_line(mapping = aes(x = x, y = incorrect.erp), color = "red") + 
  scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  theme_classic() + 
  xlab("") + 
  ylab(expression("Mean Amplitude"~(mu*"V")))

  
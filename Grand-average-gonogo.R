require(R.matlab) #function to be able to read matlab files - EEG data saved from MNE Pythin 
require(tidyverse)
require(pracma) #function to allow the calculation of linear space for plotting 

# Functions to get the average amplitude for each trial type 
get_go <- function(mat, csv, electrode){
  # Function to calculate the average amplitude for each Go trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for? 
  Go_trials <- csv$Stim_type == "Go" #this is only to select Go trials 
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  average_go <- colMeans(dat_elec[1, Go_trials, ]) #create an average for each go trial 
  return(average_go*1e6) #returns the average (in micro volts) of each go trial along the number of samples per epoch 
}

get_nogo <- function(mat, csv, electrode){
  # Function to calculate the average amplitude for each NoGo trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for? 
  NoGo_trials <- csv$Stim_type == "NoGo" #this is only to select NoGo trials
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  average_nogo <- colMeans(dat_elec[1, NoGo_trials, ]) #Create an average for each NoGo trial 
  return(average_nogo*1e6) #returns the average (in micro volts) of each nogo trial along the number of samples per epoch 
}

electrode = "Pz"

dat <- readMat("Clean_data/EEG/Go-NoGo/800_GoNoGo.mat")
trial_info <- read.csv("Raw_data/Behavioural/Go-NoGo/800-gonogo.csv")
P_800_go <- get_go(mat = dat, csv = trial_info, electrode = electrode)
P_800_nogo <- get_nogo(mat = dat, csv = trial_info, electrode = electrode)

dat <- readMat("Clean_data/EEG/Go-NoGo/700_GoNoGo.mat")
trial_info <- read.csv("Raw_data/Behavioural/Go-NoGo/kieron-gonogo.csv")
P_700_go <- get_go(mat = dat, csv = trial_info, electrode = electrode)
P_700_nogo <- get_nogo(mat = dat, csv = trial_info, electrode = electrode)

dat <- readMat("Clean_data/EEG/Go-NoGo/901_GoNoGo.mat")
trial_info <- read.csv("Raw_data/Behavioural/Go-NoGo/subject-901-gonogo.csv")
P_901_go <- get_go(mat = dat, csv = trial_info, electrode = electrode)
P_901_nogo <- get_nogo(mat = dat, csv = trial_info, electrode = electrode)

dat <- readMat("Clean_data/EEG/Go-NoGo/101_GoNoGo.mat")
trial_info <- read.csv("Raw_data/Behavioural/Go-NoGo/subject-101-gonogo.csv")
P_101_go <- get_go(mat = dat, csv = trial_info, electrode = electrode)
P_101_nogo <- get_nogo(mat = dat, csv = trial_info, electrode = electrode)

all_go <- rbind(P_101_go, P_700_go, P_800_go, P_901_go)
all_nogo <- rbind(P_101_nogo, P_700_nogo, P_800_nogo, P_901_nogo)
grand_go <- colMeans(all_go) 
grand_nogo <- colMeans(all_nogo)

x = linspace(-200,800,1025)

ggplot() + 
  geom_line(mapping = aes(x = x, y = grand_go), color = "green") + 
  geom_line(mapping = aes(x = x, y = grand_nogo), color = "red") + 
  scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  theme_classic() + 
  xlab("") + 
  ylab(expression("Mean amplitude"~(mu*"V")))

# grand_diff <- grand_nogo - grand_go
# lower = grand_diff - std_err(grand_diff) * 1.96 #lower 95% CI 
# upper = grand_diff + std_err(grand_diff) * 1.96 #upper 95% CI  

ggplot() + 
  geom_line(mapping = aes(x = x, y = grand_diff), color = "red") + 
  scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  # geom_ribbon(aes(x = x, ymin = lower, ymax = upper), alpha = 0.5) + #add 95% CI band to plot 
  theme_classic() + 
  annotate("rect", xmin = 175, xmax = 250, ymin = -2, ymax = 3,
           alpha = .5) + 
  annotate("rect", xmin = 300, xmax = 500, ymin = -2, ymax = 3,
           alpha = .5) + 
  xlab("") + 
  ylab(expression("Mean amplitude"~(mu*"V")))

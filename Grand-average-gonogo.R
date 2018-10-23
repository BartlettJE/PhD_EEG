require(R.matlab) #function to be able to read matlab files - EEG data saved from MNE Pythin 
require(tidyverse)
require(pracma) #function to allow the calculation of linear space for plotting 
require(cowplot)

# Functions to get the average amplitude for each trial type 
get_go <- function(mat, csv, electrode){
  # Function to calculate the average amplitude for each Go trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for? 
  go_trials <- csv$Stim_type == "Go" & csv$Block != "Practice" #this is only to select correct trials 
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  average_correct <- colMeans(dat_elec[1, go_trials, ]) #create an average for each go trial 
  return(average_correct*1e6) #returns the average (in micro volts) of each go trial along the number of samples per epoch 
}

get_nogo <- function(mat, csv, electrode){
  # Function to calculate the average amplitude for each NoGo trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for? 
  nogo_trials <- csv$Stim_type== "NoGo" & csv$Block != "Practice" #this is only to select NoGo trials
  #incorrect_trials <- csv$Block != "Practice"
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  average_incorrect <- colMeans(dat_elec[1, nogo_trials, ]) #Create an average for each NoGo trial 
  return(average_incorrect*1e6) #returns the average (in micro volts) of each nogo trial along the number of samples per epoch 
}

# prepare batch processing 

# read a list of .csv (experiment data) to append later 
csv.files <- list.files(path = "Raw_data/Behavioural/Go-NoGo/",
                        pattern = "*.csv",
                        full.names = F)

# read a list of .mat (EEG data) to append later 
mat.files <- list.files(path = "Rdata/Go-NoGo/",
                        pattern = "*.mat",
                        full.names = F)

# Define which electrode I want to focus on out of the array of 33
electrode = "Fz"

# Create an empty object to append the data frame to 
amplitude.dat <- NULL

# Define the linear space for the x axis of the graphs 
x = linspace(-200,800,1025)

# Run a for loop to add the dataframe to the object above 
for (i in 1:length(csv.files)){
  # for each file, read in the .csv trial information and .mat EEG file
  trial_info <- read.csv(paste("Raw_data/Behavioural/Go-NoGo/", csv.files[i], sep = "")) 
  dat <- readMat(paste("Rdata/Go-NoGo/", mat.files[i], sep = ""))
  
  # Some defensive coding
  # Make sure the csv and mat files match up - breaks loop if they do not
  if (substr(csv.files[i], 0, 4) != substr(mat.files[i], 0, 4)){
    print(paste("The files of participant ", substr(current.csv, 0, 4), " do not match.", sep = ""))
    break
  }
  else{ #if all is good, start processing the files
    # apply functions from above to get erps for correct and incorrect trials
    go.erp <- get_go(mat = dat, csv = trial_info, electrode = electrode)
    nogo.erp <- get_nogo(mat = dat, csv = trial_info, electrode = electrode)
    
    # append each new matrix row to the previous one 
    amplitude.dat <- rbind(amplitude.dat, data.frame("subject" = substr(csv.files[i], 0, 4),
                                                     "condition" = "Go",
                                                     "amplitude" = go.erp,
                                                     "time" = x))
    amplitude.dat <- rbind(amplitude.dat, data.frame("subject" = substr(csv.files[i], 0, 4),
                                                     "condition" = "NoGo",
                                                     "amplitude" = nogo.erp,
                                                     "time" = x))

    # print out the progress and make sure the files match up.
    print(paste("participant:", substr(csv.files[i], 0, 4), "is complete."))
  }
}


# Create a plot with both go and nogo waves 
amplitude.dat %>% 
  ggplot(aes(x = time, y = amplitude)) + 
  stat_summary(aes(group = interaction(subject, condition), colour = condition),
               fun.y = mean,
               geom = "line",
               size = 1,
               alpha = 0.3) + 
  stat_summary(aes(group = condition, colour = condition),
               fun.y = mean,
               geom = "line",
               size = 1,
               alpha = 1) + 
  scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  xlab("Time (ms)") + 
  ylab(expression("Mean amplitude"~(mu*"V")))

# fix later to get difference plot 
# Create a plot with the difference wave
# ggplot() + 
#   geom_line(mapping = aes(x = x, y = grand_nogo - grand_go), color = "red") + 
#   scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
#   geom_hline(yintercept = 0, linetype = 2) + 
#   geom_vline(xintercept = 0, linetype = 2) + 
#   theme_classic() + 
#   annotate("rect", xmin = 175, xmax = 250, ymin = -2, ymax = 3,
#            alpha = .5) + 
#   annotate("rect", xmin = 300, xmax = 500, ymin = -2, ymax = 3,
#            alpha = .5) + 
#   xlab("") + 
#   ylab(expression("Mean amplitude"~(mu*"V")))

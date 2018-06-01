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
  go_trials <- csv$Stimulus == "Go" & csv$Block != "Practice" #this is only to select correct trials 
  
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
  nogo_trials <- csv$Stimulus== "NoGo" & csv$Block != "Practice" #this is only to select NoGo trials
  #incorrect_trials <- csv$Block != "Practice"
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  average_incorrect <- colMeans(dat_elec[1, nogo_trials, ]) #Create an average for each NoGo trial 
  return(average_incorrect*1e6) #returns the average (in micro volts) of each nogo trial along the number of samples per epoch 
}

# prepare batch processing 

# read a list of .csv (experiment data) to append later 
csv.files <- list.files(path = "Raw_data/Behavioural/Smoking-nogo/",
                        pattern = "*.csv",
                        full.names = F)

# read a list of .mat (EEG data) to append later 
mat.files <- list.files(path = "Rdata/Smoking-nogo/",
                        pattern = "*.mat",
                        full.names = F)

# Define which electrode I want to focus on out of the array of 33
electrode = "Cz"

# Create two empty matrices to append the voltage at each millisecond 
go.matrix <- matrix(ncol = 1025)
nogo.matrix <- matrix(ncol = 1025)

# Run a for loop to add the data to each matrix above 
for (i in 1:length(csv.files)){
  # for each file, read in the .csv trial information and .mat EEG file
  trial_info <- read.csv(paste("Raw_data/Behavioural/Smoking-nogo/", csv.files[i], sep = "")) 
  dat <- readMat(paste("Rdata/Smoking-nogo/", mat.files[1], sep = ""))
  
  # Some defensive coding
  # Make sure the csv and mat files match up - breaks loop if they do not
  current.mat <- mat.files[i]
  current.csv <- csv.files[i]
  
  if (substr(current.csv, 0, 4) != substr(current.mat, 0, 4)){
    print(paste("The files of participant ", substr(current.csv, 0, 4), " do not match.", sep = ""))
    break
  }
  else{ #if all is good, start processing the files
    # apply functions from above to get erps for correct and incorrect trials
    go.erp <- get_go(mat = dat, csv = trial_info, electrode = electrode)
    nogo.erp <- get_nogo(mat = dat, csv = trial_info, electrode = electrode)
    
    # append each new matrix row to the previous one 
    go.matrix <- rbind(go.matrix, go.erp)
    nogo.matrix <- rbind(nogo.matrix, nogo.erp)
    
    # print out the progress and make sure the files match up.
    print(paste("participant:", substr(csv.files[i], 0, 4), "is complete."))
  }
}

# Create a grand average by taking the mean voltage at each measurement point for all participants
grand_go <- colMeans(go.matrix, na.rm = T) 
grand_nogo <- colMeans(nogo.matrix, na.rm = T)

# Define the linear space for the x axis of the graphs 
x = linspace(-200,800,1025)

# Create a plot with both go and nogo waves 
individual_plot <- ggplot() + 
  geom_line(mapping = aes(x = x, y = grand_go), color = "green") + 
  geom_line(mapping = aes(x = x, y = grand_nogo), color = "red") + 
  scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  theme_classic() + 
  annotate("rect", xmin = 200, xmax = 295, ymin = -2, ymax = 3,
           alpha = .5) + 
  annotate("rect", xmin = 300, xmax = 500, ymin = -2, ymax = 3,
           alpha = .5) + 
  xlab("") + 
  ylab(expression("Mean amplitude"~(mu*"V")))

# Create a plot with the difference wave
difference_plot <- ggplot() + 
  geom_line(mapping = aes(x = x, y = grand_nogo - grand_go), color = "red") + 
  scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  theme_classic() + 
  annotate("rect", xmin = 200, xmax = 295, ymin = -2, ymax = 3,
           alpha = .5) + 
  annotate("rect", xmin = 300, xmax = 500, ymin = -2, ymax = 3,
           alpha = .5) + 
  xlab("") + 
  ylab(expression("Mean amplitude"~(mu*"V")))

# Create a grid of both plots 
plot_grid(individual_plot, difference_plot,
          ncol = 1,
          nrow = 2,
          labels = c("Individual waveforms", "Difference waveform"),
          label_x = 0.25)

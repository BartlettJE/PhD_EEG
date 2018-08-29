require(R.matlab) #function to be able to read matlab files - EEG data saved from MNE Pythin 
require(tidyverse)
require(pracma) #function to allow the calculation of linear space for plotting 
require(cowplot)

# Functions to get the average amplitude for each trial type 
get_correct <- function(mat, csv, electrode){
  # Function to calculate the average amplitude for each Go trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for? 
  correct_trials <- csv$correct == 1 & csv$Block != "Practice" #this is only to select correct trials 
  #correct_trials <- csv$Block != "Practice"
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  average_correct <- colMeans(dat_elec[1, correct_trials, ], na.rm = T) #create an average for each go trial 
  return(average_correct*1e6) #returns the average (in micro volts) of each go trial along the number of samples per epoch 
}

get_incorrect <- function(mat, csv, electrode){
  # Function to calculate the average amplitude for each NoGo trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for? 
  incorrect_trials <- csv$correct== 0 & csv$Block != "Practice" #this is only to select NoGo trials
  #incorrect_trials <- csv$Block != "Practice"
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  average_incorrect <- colMeans(dat_elec[1, incorrect_trials, ], na.rm = T) #Create an average for each NoGo trial 
  return(average_incorrect*1e6) #returns the average (in micro volts) of each nogo trial along the number of samples per epoch 
}

# prepare batch processing 

# read a list of .csv (experiment data) to append later 
csv.files <- list.files(path = "Raw_data/Behavioural/Eriksen/",
                        pattern = "*.csv",
                        full.names = F)

# read a list of .mat (EEG data) to append later 
mat.files <- list.files(path = "Rdata/Eriksen/",
                        pattern = "*.mat",
                        full.names = F)

# Define which electrode I want to focus on out of the array of 33
electrode = "Fz"

# Create two empty matrices to append the voltage at each millisecond 
correct.matrix <- matrix(ncol = 1025)
incorrect.matrix <- matrix(ncol = 1025)

# Run a for loop to add the data to each matrix above 
for (i in 1:length(csv.files)){
    # for each file, read in the .csv trial information and .mat EEG file
    trial_info <- read.csv(paste("Raw_data/Behavioural/Eriksen/", csv.files[i], sep = "")) 
    dat <- readMat(paste("Rdata/Eriksen/", mat.files[i], sep = ""))
    
    # apply functions from above to get erps for correct and incorrect trials
    correct.erp <- get_correct(mat = dat, csv = trial_info, electrode = electrode)
    incorrect.erp <- get_incorrect(mat = dat, csv = trial_info, electrode = electrode)

    # append each new matrix row to the previous one 
    correct.matrix <- rbind(correct.matrix, correct.erp)
    incorrect.matrix <- rbind(incorrect.matrix, incorrect.erp)
    
    # print out the progress and make sure the files match up. 
    # I could put in some defensive coding here. 
    print(paste("participant:", csv.files[i], mat.files[i], "is complete."))
}

# average across all of the columns to get the grand mean for correct and incorrect trials
grand_correct <- colMeans(correct.matrix, na.rm = T) 
grand_incorrect <- colMeans(incorrect.matrix, na.rm = T)

# Create linear space for x axis 
x = linspace(-200,800,1025)

# Create plots
# plot 1 = separate lines for each waveform
individual_plot <- ggplot() + 
  geom_line(mapping = aes(x = x, y = grand_correct), color = "green") + 
  geom_line(mapping = aes(x = x, y = grand_incorrect), color = "red") + 
  scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  theme_classic() + 
  annotate("rect", xmin = 25, xmax = 75, ymin = min(grand_incorrect), ymax = max(grand_incorrect),
           alpha = .5) + 
  annotate("rect", xmin = 200, xmax = 400, ymin = min(grand_incorrect), ymax = max(grand_incorrect),
           alpha = .5) +
  xlab("") + 
  ylab(expression("Mean Amplitude"~(mu*"V"))) 

# plot 2 = difference wave to make it easier to interpret 
difference_plot <- ggplot() + 
  geom_line(mapping = aes(x = x, y = grand_incorrect - grand_correct), color = "black") + 
  scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  theme_classic() + 
  annotate("rect", xmin = 25, xmax = 75, ymin = min(grand_incorrect - grand_correct), ymax = max(grand_incorrect - grand_correct),
           alpha = .5) + 
  annotate("rect", xmin = 200, xmax = 400,ymin = min(grand_incorrect - grand_correct), ymax = max(grand_incorrect - grand_correct),
           alpha = .5) +
  theme_classic() + 
  xlab("Time (ms)") + 
  ylab(expression("Mean Amplitude"~(mu*"V"))) 

# arrange the plots in a grid to present together 
eriksen_grid <- plot_grid(individual_plot, difference_plot,
          ncol = 1,
          nrow = 2,
          label_x = 0.35)

#  Save plot
# save_plot("Plots/Eriksen_grand.pdf", eriksen_grid, #specific function for saving plots 
#           ncol = 1,
#           nrow = 1,
#           base_aspect_ratio = 2)

  
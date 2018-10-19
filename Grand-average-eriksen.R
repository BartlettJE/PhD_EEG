require(R.matlab) #function to be able to read matlab files - EEG data saved from MNE Pythin 
require(tidyverse)
require(pracma) #function to allow the calculation of linear space for plotting 
require(cowplot)

# Load my packages
source("EEG_functions.R")

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
    print(paste("participant", substr(csv.files[i], 0, 4), "is complete."))
}

# Calculate how many trials were included for correct and incorrect responses 

# Create empty matrix to append to 
trial.n <- matrix(nrow = length(csv.files),
                  ncol = 3)

# Run a for loop to add the data to each matrix above 
for (i in 1:length(csv.files)){
  # for each file, read in the .csv trial information and .mat EEG file
  trial_info <- read.csv(paste("Raw_data/Behavioural/Eriksen/", csv.files[i], sep = "")) 
  mat <- readMat(paste("Rdata/Eriksen/", mat.files[i], sep = ""))
  
  # apply functions from above to get erps for correct and incorrect trials
  trials_ns <- trial_N_Eriksen(mat = mat, csv = trial_info, electrode = electrode)
  
  # append each new matrix row to the previous one 
  trial.n[i, ] <- trials_ns
  
  # print out the progress and make sure the files match up. 
  # I could put in some defensive coding here. 
  print(paste("participant", substr(csv.files[i], 0, 4), "is complete."))
}

# Convert to data frame to be more informative 
trial.n <- data.frame(trial.n)
colnames(trial.n) <- c("Participant", "N Correct", "N Incorrect")


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

# plot for if participant wants an individual plot of their data
# participant_plot <- ggplot() + 
#   geom_line(mapping = aes(x = x, y = incorrect.matrix[20, ] - correct.matrix[20, ]), color = "green") + 
#   scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
#   geom_hline(yintercept = 0, linetype = 2) + 
#   geom_vline(xintercept = 0, linetype = 2) + 
#   theme_classic() + 
#   annotate("rect", xmin = 25, xmax = 75, ymin = min(incorrect.matrix[20, ]), ymax = max(incorrect.matrix[20, ]),
#            alpha = .5) +
#   annotate("rect", xmin = 200, xmax = 400, ymin = min(incorrect.matrix[20, ]), ymax = max(incorrect.matrix[20, ]),
#            alpha = .5) +
#   xlab("") + 
#   ylab(expression("Mean Amplitude"~(mu*"V"))) 
# 
# (participant_grid <- plot_grid(participant_plot,
#                               difference_plot,
#                               ncol = 1,
#                               nrow = 2,
#                               labels = c("Participant 1024", "All participants"),
#                               label_x = 0.35))

# # Save plot
# save_plot("Plots/Eriksen_participant_1024.pdf", participant_grid, #specific function for saving plots
#           ncol = 1,
#           nrow = 1,
#           base_aspect_ratio = 2)

  
require(R.matlab) #function to be able to read matlab files - EEG data saved from MNE Pythin 
require(tidyverse)
require(pracma) #function to allow the calculation of linear space for plotting 
require(cowplot)
require(readbulk)

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
electrode = "Pz"

# Define the linear space for the x axis of the graphs 
x = linspace(-200,800,1025)

# Run a for loop to add the data to each matrix above
for (i in 1:length(csv.files)) {
  # for each file, read in the .csv trial information and .mat EEG file
  trial_info <- read.csv(paste("Raw_data/Behavioural/Eriksen/", csv.files[i], sep = ""))
  dat <- readMat(paste("Rdata/Eriksen/", mat.files[i], sep = ""))
  
  # Some defensive coding
  # Make sure the csv and mat files match up - breaks loop if they do not
  if (substr(csv.files[i], 0, 4) != substr(mat.files[i], 0, 4)) {
    print(paste("The files of participant ", substr(csv.files[i], 0, 4), " do not match.", sep = ""))
    break
  }
  else{
    #if all is good, start processing the files
    # apply functions from above to get erps for correct and incorrect trials
    correct.erp <-
      eriksen_erp(mat = dat,
                  csv = trial_info,
                  electrode = electrode,
                  correct = 1)
    incorrect.erp <-
      eriksen_erp(mat = dat,
                  csv = trial_info,
                  electrode = electrode,
                  correct = 0)
    
    # append each new matrix row to the previous one
    amplitude.dat <- data.frame(
      "subject" = substr(csv.files[i], 0, 4),
      "electrode" = electrode,
      "response" = c(rep("correct", 1025), rep("incorrect", 1025)),
      "amplitude" = c(correct.erp, incorrect.erp),
      "time" = rep(x, 2)
    )
    write.csv(amplitude.dat, paste("processed_data/eriksen/", 
                                   substr(csv.files[i], 0, 4),
                                   "-",
                                   electrode,
                                   "-eriksen.csv", 
                                   sep = ""))
    
    # print out the progress and make sure the files match up.
    # I could put in some defensive coding here.
    print(paste("participant", substr(csv.files[i], 0, 4), "is complete."))
  }
}

# Calculate how many trials were included for correct and incorrect responses 
# Participants will only be included with > 8 trials in both conditions 

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
colnames(trial.n) <- c("participant", "n_correct", "n_incorrect")

# Add eligible column for n trials > 8 in each condition
trial.n <- trial.n %>% 
  mutate(included = case_when(n_correct & n_incorrect > 7 ~ 1,
                              n_correct & n_incorrect < 8 ~ 0))

# Read in processed data from file 
amplitude.dat <- read_bulk(directory = "processed_data/eriksen/",
                           extension = ".csv")

# append eligible or not 
amplitude.dat <- left_join(amplitude.dat, trial.n,
            by = c("subject" = "participant")) 

  # Add smoking group 
amplitude.dat <- amplitude.dat %>% 
  mutate(smoking_group = case_when(substr(subject, 0, 1) == 1 ~ "Non-Smoker",
                                   substr(subject, 0, 1) == 2 ~ "Smoker")) %>% 
  filter(included == 1) # remove any Ss with < 8 trials in each condition

# Create difference wave: incorrect - correct trials 
difference_wave <- amplitude.dat %>% 
  select(-X) %>% 
  spread(key = response, value = amplitude) %>% 
  mutate(difference = incorrect - correct)

# Create constant colour scheme for all plots 
difference_wave$electrode <- factor(difference_wave$electrode, 
                                    levels = c("Cz", "Fz", "Pz"),
                                    labels = c("Cz", "Fz", "Pz"))

group.cols <- c("#a6cee3", "#1f78b4", "#b2df8a")

names(group.cols) <- (levels(difference_wave$electrode))

colScale <- scale_color_manual(name = "Electrode", values = group.cols)

# Create difference wave plot 

# Subset data for when I want to show individual data
# subject <- 2016
# difference_wave2 <- subset(difference_wave, subject == 2016)

(grand_difference <- difference_wave %>% 
  ggplot(aes(x = time, y = difference)) + 
  facet_grid(electrode~smoking_group) + 
  # stat_summary(aes(group = interaction(smoking_group, subject), color = smoking_group),
  #              fun.y = mean,
  #              geom = "line",
  #              size = 1,
  #              alpha = 0.2) + 
  stat_summary(aes(group = interaction(smoking_group, electrode), color = electrode),
               fun.y = mean,
               geom = "line",
               size = 1,
               alpha = 1) + 
  # stat_summary(data = difference_wave2, # optional label participant
  #              fun.y = mean,
  #              geom = "line",
  #              color = "black",
  #              size = 1,
  #              alpha = 1) +
  scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
  scale_y_continuous(limits = c(-15, 25),
                     breaks = seq(-15, 25, 5)) + 
  annotate("rect", xmin = 25, xmax = 75, ymin = -15, ymax = 25, alpha = 0.3) + #ERN
  annotate("rect", xmin = 200, xmax = 400, ymin = -15, ymax = 25, alpha = 0.3) + #Pe
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  theme(legend.position="none") + 
  xlab("Time (ms)") + 
  ylab(expression("Mean amplitude"~(mu*"V"))) + 
  colScale)

# Save plot
# save_plot(filename = "ERP-plots/Grand_average_eriksen.pdf",
#           plot = grand_difference,
#           base_height = 10,
#           base_width = 16)

# # Save participant plot
# save_plot(
#   filename = paste("ERP-plots/participant_plots/",
#                    subject,
#                    "-Eriksen.pdf",
#                    sep = ""),
#   plot = grand_difference,
#   base_height = 10,
#   base_width = 16
# )

require(R.matlab) #function to be able to read matlab files - EEG data saved from MNE Pythin 
require(tidyverse)
require(pracma) #function to allow the calculation of linear space for plotting 
require(cowplot)
require(readbulk)

source("EEG_functions.R")

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
electrode = "Pz"

# Define the linear space for the x axis of the graphs 
x = linspace(-200,800,1025)

# Run a for loop to add the dataframe to the object above 
for (i in 1:length(csv.files)){
  # for each file, read in the .csv trial information and .mat EEG file
  trial_info <- read.csv(paste("Raw_data/Behavioural/Go-NoGo/", csv.files[i], sep = "")) 
  dat <- readMat(paste("Rdata/Go-NoGo/", mat.files[i], sep = ""))
  
  # recode correct response to make nogo informative 
  trial_info <- trial_info %>%
    mutate(correct = ifelse(Stim_type == "Go" & response == "x", 1, 
                            ifelse(Stim_type == "NoGo" & response == "None", 1,
                                   0))) 
  # Some defensive coding
  # Make sure the csv and mat files match up - breaks loop if they do not
  if (substr(csv.files[i], 0, 4) != substr(mat.files[i], 0, 4)){
    print(paste("The files of participant ", substr(csv.files[i], 0, 4), " do not match.", sep = ""))
    break
  }
  else{ #if all is good, start processing the files
    # apply functions from above to get erps for correct and incorrect trials
    go.erp <- get_go(mat = dat, csv = trial_info, electrode = electrode)
    nogo.erp <- get_nogo(mat = dat, csv = trial_info, electrode = electrode)
    
    # append each new matrix row to the previous one 
    amplitude.dat <- data.frame(
        "subject" = substr(csv.files[i], 0, 4),
        "electrode" = electrode,
        "condition" = c(rep("Go", 1025), rep("NoGo", 1025)),
        "amplitude" = c(go.erp, nogo.erp),
        "time" = rep(x, 2)
      )
    # save data to access later
    write.csv(amplitude.dat, paste("processed_data/gonogo/", 
                                   substr(csv.files[i], 0, 4),
                                   "-",
                                   electrode,
                                   "-gonogo.csv", 
                                   sep = ""))
    
    # print out the progress and make sure the files match up.
    print(paste("participant:", substr(csv.files[i], 0, 4), "is complete."))
  }
}

# Calculate how many trials were included for go and nogo responses 
# Participants will only be included with > 20 trials in both conditions 

# Calculate how many trials were included for go and nogo trials 
# Create empty matrix to append to 
trial.n <- matrix(nrow = length(csv.files),
                  ncol = 3)

# Run a for loop to add the data to each matrix above 
for (i in 1:length(csv.files)){
  # for each file, read in the .csv trial information and .mat EEG file
  trial_info <- read.csv(paste("Raw_data/Behavioural/Go-NoGo/", csv.files[i], sep = "")) 
  mat <- readMat(paste("Rdata/Go-NoGo/", mat.files[i], sep = ""))
  
  # recode correct response to make nogo informative 
  trial_info <- trial_info %>%
    mutate(correct = ifelse(Stim_type == "Go" & response == "x", 1, 
                              ifelse(Stim_type == "NoGo" & response == "None", 1,
                                     0))) 
  
  # apply functions from above to get erps for correct and incorrect trials
  trials_ns <- trial_N_gonogo(mat = mat, csv = trial_info, electrode = electrode)
  
  # append each new matrix row to the previous one 
  trial.n[i, ] <- trials_ns
  
  # print out the progress and make sure the files match up. 
  # I could put in some defensive coding here. 
  print(paste("participant", substr(csv.files[i], 0, 4), "is complete."))
}

# Convert to data frame to be more informative 
trial.n <- data.frame(trial.n)

# Append whether the participants are included or not 
trial.n <- trial.n %>% 
  mutate(included = case_when(n_go & n_nogo > 19 ~ 1,
                              n_go & n_nogo < 20 ~ 0))

colnames(trial.n) <- c("subject", "n_go", "n_nogo", "included")

# Read in all processed data to save time 
amplitude.dat <- read_bulk(directory = "processed_data/gonogo/",
                           extension = ".csv")

# Append whether they are included or not 
amplitude.dat <- left_join(amplitude.dat, trial.n)

# Add smoking group 
amplitude.dat <- amplitude.dat %>% 
  mutate(smoking_group = case_when(substr(subject, 0, 1) == 1 ~ "Non-Smokers",
                                  substr(subject, 0, 1) == 2 ~ "Smokers")) %>% 
  filter(included == 1)

# Create difference wave: nogo - go trials 
difference_wave <- amplitude.dat %>% 
  select(-X) %>% 
  spread(key = condition, value = amplitude) %>% 
  mutate(difference =  NoGo - Go)

# Create constant colour scheme for all plots 
difference_wave$electrode <- factor(difference_wave$electrode, 
                                        levels = c("Cz", "Fz", "Pz"),
                                        labels = c("Cz", "Fz", "Pz"))

group.cols <- c("#a6cee3", "#1f78b4", "#b2df8a")

names(group.cols) <- (levels(difference_wave$electrode))

colScale <- scale_color_manual(name = "Electrode", values = group.cols)

# Create a plot with both go and nogo waves 
difference_wave %>% 
  ggplot(aes(x = time, y = difference)) + 
  facet_grid(~smoking_group) + 
  # stat_summary(aes(group = interaction(smoking_group, subject), colour = smoking_group),
  #              fun.y = mean,
  #              geom = "line",
  #              size = 1,
  #              alpha = 0.2) + 
  stat_summary(aes(group = interaction(smoking_group, electrode), colour = electrode),
               fun.y = mean,
               geom = "line",
               size = 1,
               alpha = 1) + 
  scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  xlab("Time (ms)") + 
  ylab(expression("Mean amplitude"~(mu*"V"))) +
  scale_y_continuous(limits = c(-10, 15),
                     breaks = seq(-10, 15, 5)) + 
  colScale


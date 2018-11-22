require(R.matlab) #function to be able to read matlab files - EEG data saved from MNE Pythin 
require(tidyverse)
require(pracma) #function to allow the calculation of linear space for plotting 
require(cowplot)

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
electrode = "Fz"


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
colnames(trial.n) <- c("Participant", "N_Go", "N_Nogo")

trial.n <- trial.n %>% 
  mutate(included = ifelse (N_Go < 20, 0, 
                            ifelse(N_Nogo < 20, 0, 
                                   1)))

# Create an empty object to append the data frame to 
amplitude.dat <- NULL

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

# Add smoking group 
amplitude.dat <- amplitude.dat %>% 
  mutate(smoking_group = ifelse(substr(subject, 0, 1) == 1, 
                                "Non-Smoker", # if 1, non-smoker
                                "Smoker")) # if 2, smoker

# Create a plot with both go and nogo waves 
amplitude.dat %>% 
  ggplot(aes(x = time, y = amplitude)) + 
  facet_grid(~smoking_group) + 
  stat_summary(aes(group = interaction(smoking_group, subject, condition), colour = condition),
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

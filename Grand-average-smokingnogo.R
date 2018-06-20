require(R.matlab) #function to be able to read matlab files - EEG data saved from MNE Pythin
require(tidyverse)
require(pracma) #function to allow the calculation of linear space for plotting
require(cowplot)

# import functions from separate script
source(file = "EEG_functions.R")
# current = get go trials, get nogo trials

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

go.smoking.matrix <- matrix(ncol = 1025)
nogo.smoking.matrix <- matrix(ncol = 1025)
go.neutral.matrix <- matrix(ncol = 1025)
nogo.neutral.matrix <- matrix(ncol = 1025)

# Create data frame to save averages 
participant <- 1:length(csv.files)
N2_Go_smoking_Cz <- 1:length(csv.files)
N2_NoGo_smoking_Cz <- 1:length(csv.files)
N2_Go_neutral_Cz <- 1:length(csv.files)
N2_NoGo_neutral_Cz <- 1:length(csv.files)
P3_Go_smoking_Cz <- 1:length(csv.files)
P3_NoGo_smoking_Cz <- 1:length(csv.files)
P3_Go_neutral_Cz <- 1:length(csv.files)
P3_NoGo_neutral_Cz <- 1:length(csv.files)
erp.averages <- data.frame(participant, 
                           N2_Go_smoking_Cz, N2_Go_neutral_Cz, N2_NoGo_smoking_Cz, N2_NoGo_neutral_Cz,
                           P3_Go_smoking_Cz, P3_Go_neutral_Cz, P3_NoGo_smoking_Cz, P3_NoGo_neutral_Cz)

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
  } else{ #if all is good, start processing the files
    # apply functions to get erps for correct and incorrect trials by cue type
    go.erp.smoking <- get_go(mat = dat, csv = trial_info, electrode = electrode, cue_average = T, cue_type = "smoking")
    go.erp.neutral <- get_go(mat = dat, csv = trial_info, electrode = electrode, cue_average = T, cue_type = "neutral")
    nogo.erp.smoking <- get_nogo(mat = dat, csv = trial_info, electrode = electrode, cue_average = T, cue_type = "smoking")
    nogo.erp.neutral <- get_nogo(mat = dat, csv = trial_info, electrode = electrode, cue_average = T, cue_type = "neutral")

    # add to averages participant and mean ERPs 
    erp.averages$participant[i] <- substr(current.csv, 0, 4)
    erp.averages$smoking_group[i] <- substr(current.csv, 0, 1) #1 or 2
    erp.averages$N2_Go_smoking_Cz[i] <- mean(go.erp.smoking[400:500]) #400-500 for 200-300ms (200ms baseline period)
    erp.averages$N2_NoGo_smoking_Cz[i] <- mean(nogo.erp.smoking[400:500])
    erp.averages$N2_Go_neutral_Cz[i] <- mean(go.erp.neutral[400:500])
    erp.averages$N2_NoGo_neutral_Cz[i] <- mean(nogo.erp.neutral[400:500])
    erp.averages$P3_Go_smoking_Cz[i] <- mean(go.erp.smoking[500:700]) #500-700 for 300-500ms (200ms baseline period)
    erp.averages$P3_NoGo_smoking_Cz[i] <- mean(nogo.erp.smoking[500:700])
    erp.averages$P3_Go_neutral_Cz[i] <- mean(go.erp.neutral[500:700])
    erp.averages$P3_NoGo_neutral_Cz[i] <- mean(nogo.erp.neutral[500:700])
    
    # apply functions to get erps for go and nogo trials - not by cue type
    go.erp <- get_go(mat = dat, csv = trial_info, electrode = electrode, cue_average = F)
    nogo.erp <- get_nogo(mat = dat, csv = trial_info, electrode = electrode, cue_average = F)
    
    # append each new matrix row to the previous one
    go.matrix <- rbind(go.matrix, go.erp)
    nogo.matrix <- rbind(nogo.matrix, nogo.erp)
    
    # print out the progress and make sure the files match up.
    print(paste("participant:", substr(csv.files[i], 0, 4), "is complete."))
  }
}

# Save average data set to work with
write.csv(erp.averages,
          file = "Average_data/SmokingNoGo_ERP_averages.csv")

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

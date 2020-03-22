require(R.matlab) #function to be able to read matlab files - EEG data saved from MNE Pythin 
require(tidyverse)
require(pracma) #function to allow the calculation of linear space for plotting 
require(cowplot)
require(readbulk)
require(afex)
require(skimr)

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
    go.erp <- gonogo_erp(mat = dat, csv = trial_info, electrode = electrode, stim_type = "Go")
    nogo.erp <- gonogo_erp(mat = dat, csv = trial_info, electrode = electrode, stim_type = "NoGo")
    
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

colnames(trial.n) <- c("subject", "n_go", "n_nogo")

# Append whether the participants are included or not 
trial.n <- trial.n %>% 
  mutate(included = case_when(n_go & n_nogo > 19 ~ 1,
                              n_go & n_nogo < 20 ~ 0))

#write.csv(trial.n, "Average_data/gonogo_trial_numbers.csv")


# Read in all processed data to save time 
trial.n <- read_csv("Average_data/gonogo_trial_numbers.csv")

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

# # Subset data for when I want to show individual data
# subject <- 2016
# difference_wave2 <- subset(difference_wave, subject == 2016)

# Create a plot with both go and nogo waves 
(grand_difference <- difference_wave %>% 
  ggplot(aes(x = time, y = difference)) + 
  facet_grid(electrode~smoking_group) + 
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
  # stat_summary(data = difference_wave2, # optional label participant
  #              fun.y = mean,
  #              geom = "line",
  #              color = "black",
  #              size = 1,
  #              alpha = 1) +
  scale_x_discrete(limits = seq(from = -200, to = 800, by = 200)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  xlab("Time (ms)") + 
  annotate("rect", xmin = 175, xmax = 250, ymin = -10, ymax = 15, alpha = 0.3) + #N2
  annotate("rect", xmin = 300, xmax = 500, ymin = -10, ymax = 15, alpha = 0.3) + #P3
  theme(legend.position = "none") + 
  ylab(expression("Mean amplitude"~(mu*"V"))) +
  scale_y_continuous(limits = c(-10, 15),
                     breaks = seq(-10, 15, 5)) + 
  colScale)

# # Save plot
# save_plot(filename = "ERP-plots/Grand_average_gonogo.pdf",
#           plot = grand_difference,
#           base_height = 10,
#           base_width = 16)

# # Save participant plot
# save_plot(
#   filename = paste("ERP-plots/participant_plots/",
#                    subject,
#                    "-Go-NoGo.pdf",
#                    sep = ""),
#   plot = grand_difference,
#   base_height = 10,
#   base_width = 16
# )

### Number trials included 

trial.n %>% 
  filter(included == 1) %>%
  skim()

### Inferential stats

# N2 analysis
N2 <- difference_wave %>% 
  group_by(subject, smoking_group, electrode) %>% 
  filter(time >= 175 & time <= 250) %>% 
  summarise(mean_amp = mean(difference))

N2_ANOVA <- aov_ez(id = "subject",
                    data = N2, 
                    dv = "mean_amp",
                    between = "smoking_group",
                    within = "electrode")

afex_plot(N2_ANOVA, x = "smoking_group", trace = "electrode")

# P3 analysis 
P3 <- difference_wave %>% 
  group_by(subject, smoking_group, electrode) %>% 
  filter(time >= 300 & time <= 500) %>% 
  summarise(mean_amp = mean(difference))

P3_ANOVA <- aov_ez(id = "subject",
                   data = P3, 
                   dv = "mean_amp",
                   between = "smoking_group",
                   within = "electrode")

afex_plot(P3_ANOVA, x = "smoking_group", trace = "electrode")

# Descriptives 
# N2
difference_wave %>% 
  group_by(smoking_group, electrode) %>% 
  filter(time >= 175 & time <= 250) %>% 
  summarise(mean_Go = mean(Go),
            sd_Go = sd(Go),
            mean_NoGo = mean(NoGo),
            sd_NoGo = sd(NoGo))

# P3
difference_wave %>% 
  group_by(smoking_group, electrode) %>% 
  filter(time >= 300 & time <= 500) %>% 
  summarise(mean_Go = mean(Go),
            sd_Go = sd(Go),
            mean_NoGo = mean(NoGo),
            sd_NoGo = sd(NoGo))

# Behavioural analysis 
behav.dat <- read_bulk(directory = "Raw_data/Behavioural/Go-NoGo/",
                       extension = ".csv")

behav.dat <- behav.dat %>% 
  select(Block, subject_nr, correct, Stim_type, response_time, response) %>%
  mutate(correct = case_when(Stim_type == "Go" & response == "x" ~ 1,
                             Stim_type == "NoGo" & response == "None" ~ 1,
                             Stim_type == "Go" & response == "None" ~ 0,
                             Stim_type == "NoGo" & response == "x" ~ 0)) %>% 
  filter(Block != "Practice" & correct == 1 & subject_nr %in% difference_wave$subject)

perc_error <- behav.dat %>% 
  group_by(subject_nr, Stim_type) %>% 
  summarise(sum_error = sum(correct)) %>%   
  mutate(condition_n = case_when(Stim_type == "Go" ~ 320,
                                 Stim_type == "NoGo" ~ 80)) %>%
  mutate(perc_error = 100 - (sum_error / condition_n) * 100,
         smoking_group = case_when(substr(subject_nr, 0, 1) == 1 ~ "Non-Smoker",
                                   substr(subject_nr, 0, 1) == 2 ~ "Smoker")) 

error_ANOVA <- aov_ez(id = "subject_nr",
                      data = perc_error, 
                      dv = "perc_error",
                      between = "smoking_group",
                      within = "Stim_type")

afex_plot(error_ANOVA, x = "smoking_group", trace = "Stim_type", error_ci = T)

perc_error %>% 
  group_by(Stim_type) %>% 
  summarise(mean_error = mean(perc_error),
            sd_error = sd(perc_error))

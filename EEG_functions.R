# Functions to get the average amplitude for each trial type 
gonogo_erp <- function(mat, csv, electrode, stim_type){
  # Function to calculate the average amplitude for each Go trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for? 
  # - stim_type - "Go" or "NoGo" trials
  trials <- trial_info$correct == 1 & csv$Stim_type == stim_type & csv$Block != "Practice" #this is only to select correct trials 
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  average_correct <- colMeans(dat_elec[1, trials, ], na.rm = T) #create an average for each go trial 
  return(average_correct*1e6) #returns the average (in micro volts) of each go trial along the number of samples per epoch 
}

# Functions to get the average amplitude for each trial type 
eriksen_erp <- function(mat, csv, electrode, correct){
  # Function to calculate the average amplitude for each Go trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for? 
  # - correct - mean amplitude for correct (1) or incorrect (0) trials?
  trials <- csv$correct == correct & csv$Block != "Practice" #this is only to select correct trials 
  #correct_trials <- csv$Block != "Practice"
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  average <- colMeans(dat_elec[1, trials, ], na.rm = T) #create an average for each go trial 
  return(average*1e6) #returns the average (in micro volts) of each go trial along the number of samples per epoch 
}

# Function to get trial number for Eriksen task
trial_N_Eriksen <- function(csv, mat, electrode){
  # Returns trial N for correct and incorrect trials 
  # Arguments:
  # csv = .csv containing trial information 
  # mat = .mat file containing the processed EEG data
  # electrode = select the EEG electrode to use 
  correct_trials <- trial_info$correct == 1 & trial_info$Block != "Practice" #this is only to select correct trials 
  
  #correct_trials <- csv$Block != "Practice"
  incorrect_trials <- trial_info$correct == 0 & trial_info$Block != "Practice" #this is only to select NoGo trials
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  
  # need to select the right trials
  dat_correct <- dat_elec[1, correct_trials, ]
  
  # now need to remove the NaNs for rejected epochs
  dat_correct <- na.omit(dat_correct)
  
  # Now for incorrect trials
  dat_incorrect <- dat_elec[1, incorrect_trials, ]
  
  # remove NaNs for rejected epochs
  dat_incorrect <- na.omit(dat_incorrect)
  
  # Save information for printing
  participant <- trial_info$subject_nr[1]
  n.correct <- nrow(dat_correct)
  n.incorrect <- nrow(dat_incorrect)
  
  # Save all the trial Ns as a vector
  participant_info <- c(participant, n.correct, n.incorrect)
  
  return(participant_info)
}

# Function to get trial number for Eriksen task
trial_N_gonogo <- function(csv, mat, electrode){
  # Returns trial N for correct and incorrect trials 
  # Arguments:
  # csv = .csv containing trial information 
  # mat = .mat file containing the processed EEG data
  # electrode = select the EEG electrode to use 
  go_trials <- trial_info$correct == 1 & trial_info$Block != "Practice" & trial_info$Stim_type == "Go" #this is only to select Go trials 
  
  #correct_trials <- csv$Block != "Practice"
  nogo_trials <- trial_info$correct == 1 & trial_info$Block != "Practice" & trial_info$Stim_type == "NoGo" #this is only to select NoGo trials
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames 
  # two brackets are used as one returns another list
  # two brackets returns a large array 
  
  # need to select the right trials
  dat_go <- dat_elec[1, go_trials, ]
  
  # now need to remove the NaNs for rejected epochs
  dat_go <- na.omit(dat_go)
  
  # Now for incorrect trials
  dat_nogo <- dat_elec[1, nogo_trials, ]
  
  # remove NaNs for rejected epochs
  dat_nogo <- na.omit(dat_nogo)
  
  # Save information for printing
  participant <- trial_info$subject_nr[1]
  n.go <- nrow(dat_go)
  n.nogo <- nrow(dat_nogo)
  
  # Save all the trial Ns as a vector
  participant_info <- c(participant, n.go, n.nogo)
  
  return(participant_info)
}

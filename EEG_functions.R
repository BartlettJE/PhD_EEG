# Functions to get the average amplitude for each trial type
get_go <- function(mat, csv, electrode, cue_average, cue_type){
  # Function to calculate the average amplitude for each Go trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for?
  # - cue average - do we want select a cue type? 
  # - trial type - if so, what cue type do we want? 
  
  # check to see if we're breaking it down by cue type
  if (cue_average == TRUE){
    go_trials <- csv$Stimulus == "Go" & csv$Block != "Practice" & csv$Cue_type == cue_type
  } else if (cue_average == FALSE){
    go_trials <- csv$Stimulus == "Go" & csv$Block != "Practice"
    } else{
    print("You must specify if you want to break down by cue.")
    break
    }
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames
  # two brackets are used as one returns another list
  # two brackets returns a large array
  average_correct <- colMeans(dat_elec[1, go_trials, ]) #create an average for each go trial
  return(average_correct*1e6) #returns the average (in micro volts) of each go trial along the number of samples per epoch
}

get_nogo <- function(mat, csv, electrode, cue_average, cue_type){
  # Function to calculate the average amplitude for each NoGo trial
  # Arguments:
  # - mat = a matlab file from the saved file in MNE Python
  # - csv = a .csv file from the OpenSesame trial data
  # - electrode - which electrode would you like the data for?
  # - cue average - do we want select a cue type? 
  # - trial type - if so, what cue type do we want? 
  
  # check to see if we're breaking it down by cue type
  if (cue_average == TRUE){
    nogo_trials <- csv$Stimulus== "NoGo" & csv$Block != "Practice" & csv$Cue_type == cue_type
  } else if (cue_average == FALSE){
    nogo_trials <- csv$Stimulus== "NoGo" & csv$Block != "Practice"
  } else{
    print("You must specify if you want to break down by cue.")
    break
    }
  
  dat_elec <- mat[[electrode]] #subset one data frame from the list of data frames
  # two brackets are used as one returns another list
  # two brackets returns a large array
  average_incorrect <- colMeans(dat_elec[1, nogo_trials, ]) #Create an average for each NoGo trial
  return(average_incorrect*1e6) #returns the average (in micro volts) of each nogo trial along the number of samples per epoch
}

# Calculate ERP from individual mat files 
# Returns a matrix of each participant along 1025 time points
calculate_erp <- function(mat.files, directory, electrode, matrix, min.trials = 10){
  # Arguments:
  # mat.files = list of files to read in 
  # directory = specify a directory to read the files from 
  # electrode = which electrode to get the ERPs for
  # matrix = specify a matrix to store the values in
  # min.trials = what is the minimum number of trials to accept?
  for (i in 1:length(mat.files)){
    dat <- readMat(paste(directory, mat.files[i], sep = ""))
    if (length(dat[[electrode]]) == 0){
      print(paste("Warning: participant ", substr(mat.files[i], 0, 4), " had ", length(dat[[electrode]]), " trials and was excluded.", sep = ""))
    } else if (length(dat[[electrode]][1, , 1]) >= min.trials){
      mat_elec <- dat[[electrode]]
      average_volt <- colMeans(mat_elec[1, , ])*1e6
      erp.matrix <- rbind(erp.matrix, average_volt)
      print(paste("Participant ", substr(mat.files[i], 0, 4), " had ", length(dat[[electrode]][1, ,1]), " trials included.", sep = ""))
    } else{
      print(paste("Warning: participant", substr(mat.files[i], 0, 4), " only had ", length(dat[[electrode]][1, ,1]), " trials and was excluded.", sep = ""))
    }
  }
  return(erp.matrix)
}

# Calculate ERP from individual mat files 
# Returns a matrix of each participant along 1025 time points
calculate_mean_amplitude <- function(mat.files, directory, electrode, min.trials = 5){
  # Arguments:
  # mat.files = list of files to read in 
  # directory = specify a directory to read the files from 
  # electrode = which electrode to get the ERPs for
  # matrix = specify a matrix to store the values in
  # min.trials = what is the minimum number of trials to accept?
  
  # create data frame to save averages
  participant <- 1:length(mat.files)
  smoking_group <- 1:length(mat.files)
  erp.averages <- data.frame(participant, smoking_group)
  
  for (i in 1:length(mat.files)){
    dat <- readMat(paste(directory, mat.files[i], sep = ""))
    if (length(dat[[electrode]]) == 0){
      print(paste("Warning: participant ", substr(mat.files[i], 0, 4), " had ", length(dat[[electrode]]), " trials and was excluded.", sep = ""))
    } else if (length(dat[[electrode]][1, , 1]) >= min.trials){
      mat_elec <- dat[[electrode]]
      average_volt <- colMeans(mat_elec[1, , ])*1e6
      
      erp.averages$participant[i] <- substr(mat.files[i], 0, 4)
      erp.averages$smoking_group[i] <- substr(mat.files[i], 0, 1)
      erp.averages$N2[i] <- mean(average_volt[400:500])
      erp.averages$P3[i] <- mean(average_volt[500:700])
      
      print(paste("Participant ", substr(mat.files[i], 0, 4), " had ", length(dat[[electrode]][1, ,1]), " trials included.", sep = ""))
    } else{
      print(paste("Warning: participant", substr(mat.files[i], 0, 4), " only had ", length(dat[[electrode]][1, ,1]), " trials and was excluded.", sep = ""))
    }
  }
  return(erp.averages)
}

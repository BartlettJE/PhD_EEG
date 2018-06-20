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
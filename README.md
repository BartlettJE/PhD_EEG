# Overview 

This repository contains the scripts and functions used for the processing pipeline of my two EEG tasks. There are four key files: 
- `batch_process_eriksen.py`
- `batch_process_gonogo.py`
- `Grand-average-Eriksen.R`
- `Grand-average-GoNoGo.R`

## Step one: Python Pre-Processing

There are two files per task, the first processes the data of each participant in Python using the MNE Python package. This takes a .bdf file from the EEG recorder and performs the pre-processing steps.
The end point of this file is creating a Matlab file (.mat) which can be imported into R for the processing stages. The .mat file contains a matrix of 1000 voltage readings for each of the 32 electrodes and 420 trials per task. 

## Step two: R Processing 

Once the .mat files have been created, they can be imported into R for the final processing stages. There is an additional script `EEG_functions.R` which contains a series of helper functions for taking the matrix of voltages and converting it to a mean amplitude per trial. 
After calculating the mean amplitude per trial, these are combined from all participants to form the final processing stages. The processed data is then saved to make the reporting script below more concise.

## Step three: Reporting 

The final key script in the repository is the R Markdown file `Chapter Four Reproducible Results.Rmd` to create a reproducible results section. This takes the processed data and calculates summary statistics, inferential statistics, plots, and reliability estimates. 

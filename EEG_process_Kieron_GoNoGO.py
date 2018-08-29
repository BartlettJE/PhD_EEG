# -*- coding: utf-8 -*-
"""
@author: James Bartlett
"""

import mne
import os
from matplotlib import pyplot as plt
import numpy as np
import scipy.io as sio
import pandas as pd
import glob
from mne.preprocessing import ICA
from mne.preprocessing import create_eog_epochs, create_ecg_epochs
from collections import defaultdict

#  Step 1 - read in data and define settings
#  Read in OpenSesame file
rtdata = pd.read_csv(
    'Raw_data/Behavioural/Smoking-nogo/1004-smokingnogo.csv', sep=',')

#  Read in EEG file from Biosemi II
raw = mne.io.read_raw_edf('Raw_data/EEG/Smoking-nogo/1004-smokingnogo.bdf',
                          preload=True)

#  Important information for plotting and saving
participant_n = "1004"  # participant code of participant

#  Identify which trials were Go and which were NoGo from Opensesame file
go_smoking = np.where((rtdata['Stimulus'] == 'Go') & (rtdata['Cue_type'] == 'smoking'))[0]  
go_neutral = np.where((rtdata['Stimulus'] == 'Go') & (rtdata['Cue_type'] == 'neutral'))[0]
nogo_smoking = np.where((rtdata['Stimulus'] == 'NoGo') & (rtdata['Cue_type'] == 'smoking'))[0]
nogo_neutral = np.where((rtdata['Stimulus'] == 'NoGo') & (rtdata['Cue_type'] == 'neutral'))[0]

#  Convert the RT variable into a matrix
rts = rtdata['response_time'].as_matrix()

# EEG sampling rate
biosemi_hz = 1024.0
#  Convert to integer and make sure it is in sync with the EEG sampling rate
rts = np.int64(rts / 1000.0 * biosemi_hz)

#  Set preference key - Status is Biosemi marker name
mne.set_config('MNE_STIM_CHANNEL',  # original marker name from MNE
               'Status',  # Change to to Biosemi marker name
               set_env=True)  # Update environment + config file

#  Step 2 - find events from the .bdf file
events0 = mne.find_events(raw)
# take out beginning of recording - first marker is recorded but we don't need it
events = events0[1:, :]

# Quick fix for the markers being strange - MNE expects positive markers but we have negative
# It codes the offset of the marker as the onset.
# This creates the markers
st = raw.get_data(40)[0]
st = st - np.min(st)
st2 = st[0:-1] - st[1:]
eventscalc = np.where(st2 == -2)[0] + 1
events[:, 0] = eventscalc  # This writes over the original find events from MNE

#  Read in a montage for electrode positions
#  We used a Biosemi II cap with 32 electrodes
# predefined electrode positions on the head
montage = mne.channels.read_montage('biosemi32')
raw.set_montage(montage)

#  Step 3 - rereference voltages
#  Define which reference to use
raw.set_eeg_reference('average',  # average of all electrodes as reference
                      projection=True)

# Option to define a cut-off for the maximum voltage
#reject = dict(eeg=100e-6)

#  Step 4 - apply bandpass filtering
raw.filter(l_freq=0.15,  # 0.15 based on Rietdijk et al.
           h_freq=30.0)

#  Label events as correct or incorrect
events[go_smoking, 2] = 1
events[go_neutral, 2] = 2
events[nogo_smoking, 2] = 3
events[nogo_neutral, 2] = 4

#  Record what the values are for the events
event_id = {'Go/smoking': 1, 'Go/neutral': 2, 'NoGo/smoking': 3, 'NoGo/neutral': 4}

#  Step 5 - perform occular correction procedure

n_components = 33  # has to be the same number of components as number of electrodes
method = 'fastica'  # for comparison with EEGLAB try "extended-infomax" here
decim = 3  # we need sufficient statistics, not all time points -> saves time

# we will also set state of the random number generator - ICA is a
# non-deterministic algorithm, but we want to have the same decomposition
# and the same order of components each time this tutorial is run
random_state = 23  # equivalent to set.seed

#  define ICA - performs signal decomposition
ica = ICA(n_components=n_components,
          method=method,
          random_state=random_state)

#  print the ICA object
print(ica)

#  Pick channels by type and name
pickseeg = mne.pick_types(raw.info,
                          meg=False,  # do not want MEG
                          eeg=True,
                          eog=True,
                          # do not include marker or facial electrodes
                          exclude=['Status', 'EXG2', 'EXG3', 'EXG4', 'EXG5', 'EXG6', 'EXG7', 'EXG8'])
#  Run ICA on raw data
ica.fit(raw,
        picks=pickseeg,
        decim=decim)

#  Print the results of the ICA to a topographic map
ica.plot_components(picks=pickseeg)

#
# Break here to select which components to exclude
#

#  Select likely eyeblink component from topographic maps
eog_inds = [17]
#  append this component to exclude
ica.exclude.extend(eog_inds)

#  Remove selected components from the signal
ica.apply(raw)  # directly changes raw data set

#  Step 6 - finally extract the epochs

reject = dict(eeg = 100e-6)

epochs = mne.Epochs(raw,  # raw data
                    events[32:208],  # event markers. Ignore practice, start at trial 32
                    event_id,  # are the markers for go or nogo trials?
                    tmin=-0.2, tmax=0.8,  # define epoch
                    proj=True,  # something about projection vectors
                    picks=pickseeg,  # which channels?
                    # Which period should be selected for baseline correction?
                    #reject = reject,
                    baseline=(-0.2, 0),
                    preload=True)  # true = load all epochs from disk

print epochs

go_smoke_av = epochs['Go/smoking'].average()
nogo_smoke_av = epochs['NoGo/smoking'].average()
go_neutral_av = epochs['Go/neutral'].average()
nogo_neutral_av = epochs['NoGo/neutral'].average()

# delete the original raw data
del raw

# until this point, the data is hidden, make it an object
eps_go_smoke = epochs['Go/smoking']._data
eps_go_neutral = epochs['Go/neutral']._data
eps_nogo_smoke = epochs['NoGo/smoking']._data
eps_nogo_neutral = epochs['NoGo/neutral']._data

# define the channels
epschn = epochs.ch_names

#  Data pre-processing is complete!

#  Option to save data as a .mat file to be used in R

# Go trials smoking cues 
eps2 = np.moveaxis(eps_go_smoke, [0, 1, 2], [1, 0, 2])

d = defaultdict(list)
#
for i in np.arange(33):
    d[epschn[i]].append(eps2[i, :, :])

sio.savemat('Kieron_data/' + participant_n + '_Go_smoking.mat', mdict=d)

# Go trials neutral cues
eps2 = np.moveaxis(eps_go_neutral, [0, 1, 2], [1, 0, 2])

d = defaultdict(list)
#
for i in np.arange(33):
    d[epschn[i]].append(eps2[i, :, :])

sio.savemat('Kieron_data/' + participant_n + '_Go_neutral.mat', mdict=d)

# NoGo trials smoking cues 
eps2 = np.moveaxis(eps_nogo_smoke, [0, 1, 2], [1, 0, 2])

d = defaultdict(list)
#
for i in np.arange(33):
    d[epschn[i]].append(eps2[i, :, :])

sio.savemat('Kieron_data/' + participant_n + '_NoGo_smoking.mat', mdict=d)

# NoGo trials neutral cues
eps2 = np.moveaxis(eps_nogo_neutral, [0, 1, 2], [1, 0, 2])

d = defaultdict(list)
#
for i in np.arange(33):
    d[epschn[i]].append(eps2[i, :, :])

sio.savemat('Kieron_data/' + participant_n + '_NoGo_neutral.mat', mdict=d)


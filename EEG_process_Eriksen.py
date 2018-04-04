# -*- coding: utf-8 -*-
"""
@author: James Bartlett 
"""

import mne
from mne.preprocessing import ICA
from mne.preprocessing import create_eog_epochs, create_ecg_epochs
import os
from matplotlib import pyplot as plt
import numpy as np
import scipy.io as sio
import pandas as pd
import glob
from collections import defaultdict

#  Step 1 - read in data and define settings 
#  Read in OpenSesame file 
rtdata = pd.read_csv('Raw_data/Behavioural/Eriksen/1001-eriksen.csv',
                     sep=',')

#  Read in EEG file from Biosemi II 
raw = mne.io.read_raw_edf('Raw_data/EEG/Eriksen/1001-eriksen.bdf', 
                          preload=True)

#  Important information for plotting and saving 
participant_n = "1001" #participant code of participant 

#  Identify which trials were correct and which were incorrect from Opensesame file 
correct = np.where(rtdata['correct']==1)[0]
incorrect = np.where(rtdata['correct']==0)[0]

#  Convert the RT variable into a matrix 
rts = rtdata['response_time'].as_matrix()

# EEG sampling rate 
biosemi_hz = 1024.0
#  Convert to integer and make sure it is in sync with the EEG sampling rate 
rts = np.int64(rts/1000.0*biosemi_hz)

#  Set preference key - Status is Biosemi marker name 
mne.set_config('MNE_STIM_CHANNEL', #original marker name from MNE 
               'Status', #Change to to Biosemi marker name
               set_env=True) #Update environment + config file 

#  Step 2 - find events from the .bdf file 
events0 = mne.find_events(raw)
events = events0[1:,:] #take out beginning of recording - first marker is recorded but we don't need it 

# Quick fix for the markers being strange - MNE expects positive markers but we have negative 
# It codes the offset of the marker as the onset.
# This creates the markers  
st = raw.get_data(40)[0]
st = st - np.min(st)
st2 = st[0:-1]-st[1:]
eventscalc = np.where(st2==-2)[0]+1
events[:,0] = eventscalc #This writes over the original find events from MNE 

#  Read in a montage for electrode positions 
#  We used a Biosemi II cap with 32 electrodes 
montage = mne.channels.read_montage('biosemi32') #predefined electrode positions on the head 
raw.set_montage(montage)

#  Step 3 - rereference voltages 
#  Define which reference to use 
raw.set_eeg_reference('average', #average of all electrodes as reference 
                      projection=True)



#  Step 4 - apply bandpass filtering 
raw.filter(l_freq=0.15, # 0.15 based on Rietdijk et al. 
           h_freq=30.0)

#  Label events as correct or incorrect 
events[correct,2] = 1
events[incorrect,2] = 2

#  Record what the values are for the events 
event_id = {'correct': 1, 'incorrect': 2}

#  Step 5 - perform occular correction procedure 

n_components = 33  # has to be the same number of components as number of electrodes
method = 'fastica'  # for comparison with EEGLAB try "extended-infomax" here
decim = 3  # we need sufficient statistics, not all time points -> saves time

# we will also set state of the random number generator - ICA is a
# non-deterministic algorithm, but we want to have the same decomposition
# and the same order of components each time this tutorial is run
random_state = 23 #equivalent to set.seed 

#  define ICA - performs signal decomposition 
ica = ICA(n_components=n_components, 
          method=method, 
          random_state=random_state)

#  print the ICA object 
print(ica)

#  Pick channels by type and name
pickseeg = mne.pick_types(raw.info, 
                          meg=False, #do not want MEG  
                          eeg=True, 
                          eog=True, 
                          #do not include marker or facial electrodes
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
eog_inds = [18]
#  append this component to exclude 
ica.exclude.extend(eog_inds)

#  Remove selected components from the signal 
ica.apply(raw) # directly changes raw data set 

#  Step 6 - finally extract the epochs

# As this is for the Flanker task and we are interested in error processing 
# We need time 0 to be when the response was made, not stimulus onset. 
events2 = events 
events2[:,0] = events2[:,0] + rts #add reaction time to event marker

# Option to define a cut-off for the maximum voltage 
reject = dict(eeg=100e-6)

epochs = mne.Epochs(raw, #raw data 
                    events2, #event markers 
                    event_id, #are the markers for correct or incorrect trials?
                    tmin=-0.2, tmax=0.8, #define epoch 
                    proj=True, #something about projection vectors 
                    picks=pickseeg, #which channels? 
                    baseline=(-0.2, 0), #Which period should be selected for baseline correction? 
                    reject = reject, #exclude anything above 100 micro volts
                    preload=True) # true = load all epochs from disk

# delete the original raw data 
del raw

# until this point, the data is hidden, make it an object                     
eps = epochs._data
# define the channels
epschn = epochs.ch_names

#  Data pre-processing is complete! 

#  Option to save data as a .mat file to be used in R 
eps2 = np.moveaxis(eps,[0,1,2],[1,0,2])

d = defaultdict(list)

for i in np.arange(33):
    d[epschn[i]].append(eps2[i,:,:])

#sio.savemat('Clean_data/mat_files/Eriksen/' + participant_n + '_Eriksen.mat', mdict=d)


#  Step 7 - plotting time 
#  calculate mean for condition 1 - correct trials 
cond1 = np.mean(eps[correct,:,:],
                axis=0)
#  calculate mean for condition 2 - incorrect trials 
cond2 = np.mean(eps[incorrect,:,:],
                axis=0)

#  Calculate an interval of numbers to be used as the x axis in the plot 
x = np.linspace(-200, #min time of epoch
                800, #max time of epoch 
                1025) #how many samples in this period? 

#  Select channel to plot 
chsel = 'Pz'

#  Select this channel from the Epoch data 
ch = epschn.index(chsel)

#  Begin pyplot 
fig, ax = plt.subplots(2, figsize=(7,10))

#  plot 1 = plot both correct and incorrect epochs 
#  define line 1
line1, = ax[0].plot(x,
           cond1[ch,:]*1e6,
           '-', 
           linewidth=2,
           label='Correct ('+str(len(correct))+')')
#  define line 2 
line2, = ax[0].plot(x, 
           cond2[ch,:]*1e6, 
           linewidth=2,
           label='Incorrect ('+str(len(incorrect))+')')

#  plot settings               
#  calculate position for electrode text 
xpos = ax[0].get_xlim()[0]+(ax[0].get_xlim()[1]-ax[0].get_xlim()[0])*0.05
ypos = ax[0].get_ylim()[1]-(ax[0].get_ylim()[1]-ax[0].get_ylim()[0])*0.1
ax[0].text(30, ypos, 
  "ERN", #which channel to plot? 
  fontsize=15)
ax[0].text(300, ypos, 
  "Pe", #which channel to plot? 
  fontsize=15)
ax[0].legend(loc='lower right')
ax[0].set_title("Eriksen Flanker task Participant " + participant_n + " " + chsel)
ax[0].axvspan(25, 75, color='red', alpha=0.5) #ROI for ERN
ax[0].axvspan(200, 400, color='red', alpha=0.5) #ROI for Pe
ax[0].axhline(y = 0, color = 'black', linestyle = 'dashed')
ax[0].axvline(x = 0, color = 'black', linestyle = 'dashed')
ax[0].grid(False)

#  Plot 2 - plot the difference wave 
#  define line 3 
line3, = ax[1].plot(x, 
           (cond2[ch,:] - cond1[ch, :])*1e6, 
           linewidth=2,
           label='Difference')

#  plot settings               
#  calculate position for electrode text 
ax[1].legend(loc='lower right')
ax[1].axvspan(25, 75, color='red', alpha=0.5) #ROI for ERN
ax[1].axvspan(200, 400, color='red', alpha=0.5) #ROI for Pe
ax[1].grid(True)
ax[1].axhline(y = 0, color = 'black', linestyle = 'dashed')
ax[1].axvline(x = 0, color = 'black', linestyle = 'dashed')
ax[1].grid(False)

plt.show()

#fig.savefig("ERP-plots/Eriksen/" + "Eriksen-" + participant_n + "-" + chsel + ".png", dpi=300)        
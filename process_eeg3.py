# -*- coding: utf-8 -*-
"""
Created on Fri Jan 12 10:50:42 2018

@author: ac0195
"""

import mne
import os
from matplotlib import pyplot as plt
import numpy as np
import scipy.io as sio
import pandas as pd
import glob

rtdata = pd.read_csv('800-gonogo.csv',sep=',')
correct = np.where(rtdata['correct']==1)[0]
incorrect = np.where(rtdata['correct']==0)[0]

rts = rtdata['response_time'].as_matrix()
rts = np.int64(rts/1000.0*1024.0)

data_path = 'gonogo800.bdf'

raw = mne.io.read_raw_edf(data_path, preload=True)

# Give the sample rate
print('sample rate:', raw.info['sfreq'], 'Hz')
# Give the size of the data matrix
print('channels x samples:', raw._data.shape)

print('Shape of data array:', raw._data.shape)

#raw0.set_channel_types(mapping={'EXG1': 'eog'})
#raw0.set_channel_types(mapping={'EXG2': 'eog'})

mne.set_config('MNE_STIM_CHANNEL', 'Status', set_env=True)
events0 = mne.find_events(raw)
events = events0[1:,:] #take out beginning of recording - first marker 

montage = mne.channels.read_montage('biosemi32') #predefined electrode positions on the head 

raw.set_eeg_reference('average', projection=True)
raw.set_montage(montage)

#raw.info['bads'] = ['MEG 2443', 'EEG 053']  # mark bad channels  

#reject = dict(eeg=180e-6)

raw.filter(l_freq=0.15, h_freq=30.0)  # band-pass filter data - 0.15 based on Rietdijk et al. 
# Extract epochs and save them:

#eventsdata = pd.read_csv('BDFdata/'+fname+'-ANT.txt',sep='\t')

events[correct,2] = 1
events[incorrect,2] = 2

event_id = {'correct': 1, 'incorrect': 2}
#mne.viz.plot_events(events, raw.info['sfreq'], raw.first_samp)

#reject = dict(eeg=80e-6)  

from mne.preprocessing import ICA
from mne.preprocessing import create_eog_epochs, create_ecg_epochs

n_components = 33  # has to be the same number of components as number of electrodes
method = 'fastica'  # for comparison with EEGLAB try "extended-infomax" here
decim = 3  # we need sufficient statistics, not all time points -> saves time

# we will also set state of the random number generator - ICA is a
# non-deterministic algorithm, but we want to have the same decomposition
# and the same order of components each time this tutorial is run
random_state = 23 #equivalent to set.seed 

ica = ICA(n_components=n_components, method=method, random_state=random_state)
print(ica)

pickseeg = mne.pick_types(raw.info, meg=False, eeg=True, eog=True, exclude=['Status', 'EXG2', 'EXG3', 'EXG4', 'EXG5', 'EXG6', 'EXG7', 'EXG8']) 
#picksall = mne.pick_types(raw.info, meg=False, eeg=True, eog=True, exclude=['Status'])
#reject = dict(eeg=60e-6)
ica.fit(raw, picks=pickseeg, decim=decim)
print(ica)

ica.plot_components(picks=pickseeg)

# Break here to select which components to exclude

eog_inds = [8]
ica.exclude.extend(eog_inds)

ica.apply(raw) #Changes raw data set 


events2 = events 
events2[:,0] = events2[:,0] + rts


epochs = mne.Epochs(raw, events2, event_id, tmin=-0.2, tmax=1.0, 
                    proj=True, picks=pickseeg, 
                    baseline=(-0.2, 0), preload=True)
del raw
                    
eps = epochs._data
epschn = epochs.ch_names

#eps2 = np.zeros(eps.shape)
#for i in np.arange(eps.shape[0]):
#    for j in np.arange(eps.shape[1]):
#        eps2[i,j,:-rts[i]] = eps[i,j,rts[i]:]
    

cond1 = np.mean(eps[correct,:,:],axis=0)
cond2 = np.mean(eps[incorrect,:,:],axis=0)
x = np.linspace(-200,1000,1230)

chsel = 'Pz'

ch = epschn.index(chsel)
fig, ax = plt.subplots(2, figsize=(7,10))


line1, = ax[0].plot(x, (cond2[ch,:] - cond1[ch,:])*1e6, '-', linewidth=2,
                 label='Difference ('+str(len(correct))+')')
#line2, = ax[0].plot(x, cond2[ch,:], linewidth=2,
#                 label='incorrect ('+str(len(incorrect))+')')

                 
xpos = ax[0].get_xlim()[0]+(ax[0].get_xlim()[1]-ax[0].get_xlim()[0])*0.05
ypos = ax[0].get_ylim()[1]-(ax[0].get_ylim()[1]-ax[0].get_ylim()[0])*0.1
ax[0].text(xpos, ypos, chsel, fontsize=15)
ax[0].legend(loc='lower left')
ax[0].set_title(data_path)
ax[0].grid(True)

plt.show()

#eps3 = np.moveaxis(eps2,[0,1,2],[1,0,2])
#
#from collections import defaultdict
#
#d = defaultdict(list)
#
#for i in np.arange(40):
#    d[epschn[i]].append(eps3[i,:,:])
#
#sio.savemat('epochsResponse.mat', mdict=d)

#fig.savefig("Results2/"+fname+"-"+chsel+".png", dpi=300)        
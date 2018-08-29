import mne
import os
#from matplotlib import pyplot as plt
import numpy as np
import scipy.io as sio
import pandas as pd
import glob
#import copy
from mne.preprocessing import ICA
#from mne.preprocessing import create_eog_epochs, create_ecg_epochs
from collections import defaultdict

# read csv containing ICA components to exclude
excludelist = pd.read_csv("Smokingnogo.csv",sep=',')

#  file list of raw experiment file
filelist = glob.glob("Raw_data/Behavioural/Smoking-nogo/*.csv")

for filepath in filelist:
    filename = os.path.basename(filepath)
#  Look in folder - only write if file doesn't already exist as a .mat
    if os.path.isfile("Rdata/Smoking-nogo/"+os.path.splitext(filename)[0]+'.mat')==False:
#  read .csv file
        rtdata = pd.read_csv(filepath,sep=',')

#  label go and nogo trials for each combination of cue type
        go_smoking = np.where((rtdata['Stimulus'] == 'Go') & (rtdata['Cue_type'] == 'smoking'))[0]  
        go_neutral = np.where((rtdata['Stimulus'] == 'Go') & (rtdata['Cue_type'] == 'neutral'))[0]
        nogo_smoking = np.where((rtdata['Stimulus'] == 'NoGo') & (rtdata['Cue_type'] == 'smoking'))[0]
        nogo_neutral = np.where((rtdata['Stimulus'] == 'NoGo') & (rtdata['Cue_type'] == 'neutral'))[0]

#  convert response time variable to a matrix and save as rts
        rts = rtdata['response_time'].as_matrix()

#  convert to sampling rate
        rts = np.int64(rts/1000.0*1024.0)

#  read the raw EEG data
        EEGpath = "Raw_data/EEG/Smoking-nogo/"+os.path.splitext(filename)[0]+'.bdf'

#  start initial MNE analysis - read raw file
        raw = mne.io.read_raw_edf(EEGpath, 
                                  preload=True)

#  Finding markers the DIY way - MNE has a problem with negative markers
        st = raw.get_data(40)[0]
        st = st - np.min(st)
        st2 = st[0:-1]-st[1:]
        eventscalc = np.where(st2==-2)[0]+1

#  Set preference key - Status is Biosemi marker name
        mne.set_config('MNE_STIM_CHANNEL', 'Status', set_env=True)

#  find markers and remove the initial starting marker
        events0 = mne.find_events(raw)
        events = events0[1:,:]
        events[:,0] = eventscalc[:208]

#print to console how many events were found
        print("Found "+str(len(events))+" events and "+str(len(eventscalc))+" eventscalc")

# define which reference to use - average of all electrodes
        raw.set_eeg_reference('average', 
                              projection=True)

# We used a Biosemi II cap with 32 electrodes - predefined electrode positions on the head
        montage = mne.channels.read_montage('biosemi32')
        raw.set_montage(montage)

# band-pass filter data
        raw.filter(l_freq=0.15, 
                   h_freq=30.0)
        
# mark unique events by number         
        events[go_smoking, 2] = 1
        events[go_neutral, 2] = 2
        events[nogo_smoking, 2] = 3
        events[nogo_neutral, 2] = 4

# record a value for each event type        
        event_id = {'Go/smoking': 1, 
                    'Go/neutral': 2, 
                    'NoGo/smoking': 3, 
                    'NoGo/neutral': 4}


#prepare ICA ocular correction procedure
        n_components = 33  # if float, select n_components by explained variance of PCA
        method = 'fastica'  # for comparison with EEGLAB try "extended-infomax" here
        decim = 3  # we need sufficient statistics, not all time points -> saves time

        # we will also set state of the random number generator - ICA is a
        # non-deterministic algorithm, but we want to have the same decomposition
        # and the same order of components each time this tutorial is run
        random_state = 23

#prepare ICA object
        ica = ICA(n_components = n_components,
                  method = method,
                  random_state = random_state)
#print details
        print(ica)

#  Pick channels by type and name
        pickseeg = mne.pick_types(raw.info, meg=False, eeg=True, eog=True,
        exclude=['Status', 'EXG2', 'EXG3', 'EXG4', 'EXG5', 'EXG6', 'EXG7', 'EXG8'])

# fit ICA with channel picks and object above
        ica.fit(raw, 
                picks = pickseeg, 
                decim = decim)
        print(ica)

#plot ICA components
        ica.plot_components(picks=pickseeg)

# Start Stoyan's automated ICA component picker
        eog_inds = []
        load_inds0 = np.where(excludelist['filename']==os.path.splitext(filename)[0])
        load_inds = load_inds0[0]
        if len(load_inds)>0:
            i = 0
            exclude_component = excludelist.iloc[load_inds,i]
            while np.isnan(exclude_component.iloc[0]) == False:
                eog_inds = [eog_inds, int(exclude_component.iloc[0])]
                i = i + 1
                exclude_component = excludelist.iloc[load_inds,i]
        else:
            d = {"filename": [os.path.splitext(filename)[0]],"Exclude0": [""],"Exclude1": [""],"Exclude2": [""],"Exclude3": [""],"Exclude4": [""],"Exclude5": [""],"ExcludeX": [""]}
            df = pd.DataFrame.from_dict(d, dtype = str)
            i = 0
            exclude_component = input("Type single component to exclude: ")
            while exclude_component < 40:
                eog_inds = [eog_inds, int(exclude_component)]
                df.iloc[0,i] = exclude_component
                i = i + 1
                exclude_component = input("Type single component to exclude: ")
            excludelist = excludelist.append(df)
            excludelist.to_csv("Smokingnogo.csv", index=False)

        ica.exclude.extend(eog_inds[1:])

# exclude chosen ICA components
        ica.apply(raw)

# specify extreme voltage threshold
        reject = dict(eeg=100e-6)

# Create epochs of the data
        epochs = mne.Epochs(raw, 
                            events[32:208], #ignore practice trials 
                            event_id,
                            tmin=-0.2, tmax=0.8,
                            proj=True, 
                            picks=pickseeg,
                            baseline=(-0.2, 0),
                            reject = reject,
                            preload=True)

        del raw

# Save a different version of the epoch data for each trial type to a visible object
        eps_go_smoke = epochs['Go/smoking']._data
        eps_go_neutral = epochs['Go/neutral']._data
        eps_nogo_smoke = epochs['NoGo/smoking']._data
        eps_nogo_neutral = epochs['NoGo/neutral']._data

# Save a list of channel names
        epschn = epochs.ch_names
        
# Save a data file for each trial and cue type

# Go trials smoking cues      
        epschn = epochs.ch_names
        eps3 = np.moveaxis(eps_go_smoke,[0,1,2],[1,0,2])

        d = defaultdict(list)
        for i in np.arange(33):
            d[epschn[i]].append(eps3[i,:,:])
# save as .mat file for analysis in R
        sio.savemat("Kieron_data/Go_smoking/"+os.path.splitext(filename)[0]+'_Go_smoking.mat', mdict=d)

# Go trials neutral cues      
        eps3 = np.moveaxis(eps_go_neutral,[0,1,2],[1,0,2])

        d = defaultdict(list)
        for i in np.arange(33):
            d[epschn[i]].append(eps3[i,:,:])
# save as .mat file for analysis in R
        sio.savemat("Kieron_data/Go_neutral/"+os.path.splitext(filename)[0]+'_Go_neutral.mat', mdict=d)

# NoGo trials smoking cues      
        eps3 = np.moveaxis(eps_nogo_smoke,[0,1,2],[1,0,2])

        d = defaultdict(list)
        for i in np.arange(33):
            d[epschn[i]].append(eps3[i,:,:])
# save as .mat file for analysis in R
        sio.savemat("Kieron_data/Nogo_smoking/"+os.path.splitext(filename)[0]+'_Nogo_smoking.mat', mdict=d)

# NoGo trials neutral cues      
        eps3 = np.moveaxis(eps_nogo_neutral,[0,1,2],[1,0,2])

        d = defaultdict(list)
        for i in np.arange(33):
            d[epschn[i]].append(eps3[i,:,:])
# save as .mat file for analysis in R
        sio.savemat("Kieron_data/Nogo_neutral/"+os.path.splitext(filename)[0]+'_Nogo_neutral.mat', mdict=d)



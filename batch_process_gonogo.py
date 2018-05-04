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
#import feather

# read csv containing ICA components to exclude
excludelist = pd.read_csv("Gonogo.csv",sep=',')

#  file list of raw experiment file
filelist = glob.glob("Raw_data/Behavioural/Go-NoGo/*.csv")
for filepath in filelist:
    filename = os.path.basename(filepath)
#  Look in folder - only write if file doesn't already exist as a .mat
    if os.path.isfile("Rdata/Go-NoGo/"+os.path.splitext(filename)[0]+'.mat')==False:
#  read .csv file
        rtdata = pd.read_csv(filepath,sep=',')

#  label go and nogo trials
        Go = np.where(rtdata['Stim_type'] == "Go")[0]
        Nogo = np.where(rtdata['Stim_type'] == "NoGo")[0]

#  convert response time variable to a matrix and save as rts
        rts = rtdata['response_time'].as_matrix()

#  convert to sampling rate
        rts = np.int64(rts/1000.0*1024.0)

#  read the raw EEG data
        EEGpath = "Raw_data/EEG/Go-NoGo/"+os.path.splitext(filename)[0]+'.bdf'

#  start initial MNE analysis - read raw file
        raw = mne.io.read_raw_edf(EEGpath, preload=True)

#  unsure what's going on here - potentially finding markers
        st = raw.get_data(40)[0]
        st = st - np.min(st)
        st2 = st[0:-1]-st[1:]
        eventscalc = np.where(st2==-2)[0]+1

#Set preference key - Status is Biosemi marker name
        mne.set_config('MNE_STIM_CHANNEL', 'Status', set_env=True)

#find markers and remove the initial starting marker
        events0 = mne.find_events(raw)
        events = events0[1:,:]
        events[:,0] = eventscalc[:420]

#print to console how many events found
        print("Found "+str(len(events))+" events and "+str(len(eventscalc))+" eventscalc")

# define which reference to use - average of all electrodes
        raw.set_eeg_reference('average', projection=True)

# We used a Biosemi II cap with 32 electrodes - predefined electrode positions on the head
        montage = mne.channels.read_montage('biosemi32')
        raw.set_montage(montage)

# band-pass filter data
        raw.filter(l_freq=0.15, h_freq=30.0)

#define event markers
        events[Go,2] = 1
        events[Nogo,2] = 2
        event_id = {'Go': 1, 'NoGo': 2}

#prepare ICA ocular correction procedure
        n_components = 33  # if float, select n_components by explained variance of PCA
        method = 'fastica'  # for comparison with EEGLAB try "extended-infomax" here
        decim = 3  # we need sufficient statistics, not all time points -> saves time

        # we will also set state of the random number generator - ICA is a
        # non-deterministic algorithm, but we want to have the same decomposition
        # and the same order of components each time this tutorial is run
        random_state = 23

#prepare ICA object
        ica = ICA(n_components=n_components,
        method=method,
        random_state=random_state)
#print details
        print(ica)

#  Pick channels by type and name
        pickseeg = mne.pick_types(raw.info, meg=False, eeg=True, eog=True,
        exclude=['Status', 'EXG2', 'EXG3', 'EXG4', 'EXG5', 'EXG6', 'EXG7', 'EXG8'])

# fit ICA with channel picks and object above
        ica.fit(raw, picks=pickseeg, decim=decim)
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
            while exclude_component < 20:
                eog_inds = [eog_inds, int(exclude_component)]
                df.iloc[0,i] = exclude_component
                i = i + 1
                exclude_component = input("Type single component to exclude: ")
            excludelist = excludelist.append(df)
            excludelist.to_csv("Eriksen.csv", index=False)

        ica.exclude.extend(eog_inds[1:])

# exclude chosen ICA components
        ica.apply(raw)

# Create epochs of the data
        epochs = mne.Epochs(raw, events, event_id,
        tmin=-0.2, tmax=0.8,
        proj=True, picks=pickseeg,
        baseline=(-0.2, 0),
        preload=True)

        del raw

        eps = epochs._data
        epschn = epochs.ch_names
        eps3 = np.moveaxis(eps,[0,1,2],[1,0,2])

        d = defaultdict(list)
        for i in np.arange(33):
            d[epschn[i]].append(eps3[i,:,:])

# save as .mat file for analysis in R
        sio.savemat("Rdata/Go-NoGo/"+os.path.splitext(filename)[0]+'.mat', mdict=d)

        #pan = pd.Panel(eps3)
        #df = pan.to_frame()
        #df.columns = epschn

        #df.reset_index(inplace=True)
        #df.to_feather("Rdata/Eriksen/"+os.path.splitext(filename)[0]+".feather")

        #nr,nc = eps3.shape
        #datar = ro.r.matrix(eps3, nrow=nr, ncol=nc)

        #ro.r.assign("data", datar)
        #ro.r("save(data, file='Rdata/Eriksen/"+os.path.splitext(filename)[0]+".Rdata')")


        #fig.savefig("Results2/"+fname+"-"+chsel+".png", dpi=300)

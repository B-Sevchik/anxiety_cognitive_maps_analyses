# -*- coding: utf-8 -*-
"""
Created on Thu Mar  2 15:37:07 2023

@author: rmg55
"""
import glob
import pandas as pd

#grab all the files in the given folder that have data
path = '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/Pilot6/'
#r"/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/TestingData2"
#"C:\\Users\\rmg55\\Box\\Data\\SF_Sequence2\\"
allFiles = glob.glob(path + "*.txt")
print('allFiles', allFiles)

#initialize the final list of lists that has our data
out_data = []

# generate the column names
colnames = ['subject'] + ['s' + str(i) for i in range(1, 21)]

# add column names to out data
out_data.append(colnames)
print(colnames)

#loop through all the files
for file in allFiles:
    #opens the file
    data = open(file).readlines()
    print('data', data)
    
    #grab the worker ID
    subID = data[2].strip().split(':')[1]
    
    #grab the STAI data
    STAI_data = data[5].strip().split(':')[1].split(';')
    print(STAI_data)
    
    out_data.append([subID] + STAI_data)

# finally, turn it into a pandas data frame
df = pd.DataFrame(out_data)

df.to_csv(path + 'STAIscores.csv')
    
    
    
    
    
    
    
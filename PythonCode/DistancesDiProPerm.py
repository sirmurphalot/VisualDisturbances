# Visual Disturbances DiProPerm Analysis
# Author: Alexander Murph

##########################################################################################################
##########################################################################################################
## Update 9/28: Uploading this to GitHub.  If one wishes to recreate these results, they'll need to add ##
## in their own file paths -- these are specific to my directories.  Data will NOT be released yet.     ##
## When it is I'll update the file paths so these files will work directly from GitHub                  ##
##########################################################################################################
##########################################################################################################

import numpy as np
import matplotlib.pyplot as plt
#import pandas as pd
import csv
# %matplotlib inline

from diproperm.DiProPerm import DiProPerm


X = []
y = []

with open("../Data/all_distances.csv") as csvDataFile:
    csvReader = csv.reader(csvDataFile)
    for row in csvReader:
        X.append(row[1:])

with open("../Data/all_distances_response.csv") as csvDataFile:
    csvReader = csv.reader(csvDataFile)
    for row in csvReader:
        y.append(row[1])

y = np.array(y)
X = np.array(X)
y = y[1:] 
X = X[1:,1:]
y = y.astype(int)
X = X.astype(float)


#X = pd.read_csv("full_playtime_data.csv", index_col=None, header=0, dtype = 'a')
#y = pd.read_csv("full_playtime_response.csv", index_col=None, header=0, dtype = 'a')

# DiProPerm with mean difference classifier, mean difference summary
# statistic, and 1000 permutation samples.
dpp = DiProPerm(B=1000, separation_stats=['md', 't', 'auc'], clf='md')
dpp.fit(X, y)

print(dpp.test_stats_['md'])

plt.figure(figsize=[12, 5])
# show histogram of separation statistics
plt.subplot(1, 2, 1)
dpp.plot_perm_sep_stats(stat='md')

# the observed scores
plt.subplot(1, 2, 2)
dpp.plot_observed_scores()
plt.title("all_distances")
plt.show()
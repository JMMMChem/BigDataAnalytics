import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import time
#import os
#import csv
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
from kneed import KneeLocator
from sklearn import metrics
from scipy.spatial.distance import cdist
import seaborn as sb
from sklearn.cluster import KMeans
from sklearn.metrics import pairwise_distances_argmin_min
from mpl_toolkits.mplot3d import Axes3D
import seaborn as sns

original = pd.read_csv('computers.csv',sep=';',encoding='latin1')

##Data scaling and variable transformation

original['cd'] = original['cd'].apply(lambda x: 0 if x.strip()=='no' else 1)
original['multi'] = original['multi'].apply(lambda x: 0 if x.strip()=='no' else 1)
original['premium'] = original['premium'].apply(lambda x: 0 if x.strip()=='no' else 1)
scaler = StandardScaler()
CPscaled = scaler.fit_transform(original)
CPscaled_trs=np.transpose(CPscaled)

## CONSTRUCT THE ELBOW GRAPH AND FIND THE OPTIMAL K and plot its results

Nc = range(1, 20)

def elbow(CPscaled, Nc):
    output = []
    
    for i in Nc:
        kmeans = KMeans(n_clusters=i,random_state=1)
        kmeans.fit(CPscaled)
        output.append(kmeans.inertia_)
    #score = [kmeans[i].fit(CPscaled).inertia_ for i in range(len(kmeans))]
    #score
    return output
#score to see how well is the fit
start_time = time.time()

score=elbow(CPscaled, Nc)

elbow_new = KneeLocator(Nc, score, curve='convex', direction='decreasing')
print("The best number of k clusters is: ", elbow_new.knee)
print("With %d clusters, the sum of squared distances is: %f" % (elbow_new.knee, elbow_new.elbow_y))

plt.plot(Nc,score)
plt.xlabel('Number of Clusters')
plt.ylabel('Score')
plt.title('Elbow Curve')
plt.show()

#k_max=20
print("The time that it took to build the k matrix is: {}".format(time.time() - start_time))

##CLUSTER THE DATA USING THE OPTIMUM K
k_means=KMeans(
    init='random',
    n_clusters=elbow_new.knee,
    n_init=25,
    max_iter=300,
    random_state=42
    )

km_results=k_means.fit(CPscaled)
km_labels=km_results.labels_
km_centroids=km_results.cluster_centers_
km_labels_unique=np.unique(km_labels)
ris=np.transpose(np.vstack([CPscaled_trs,km_labels]))

##Plot the first 2 dimensions of the clusters
mean=np.mean(original["price"])
sd=np.std(original["price"])
mean2=np.mean(original["speed"])
sd2=np.std(original["speed"])

plt.scatter(km_centroids[:,0],km_centroids[:,1])

km_centroids_price=km_centroids[:,0]*sd + mean
km_centroids_speed=km_centroids[:,0]*sd2 + mean2

plt.scatter(km_centroids_price, km_centroids_speed)



##Find the cluster with the highest average price

ind=original.columns.get_loc("price")-1
price_max=list(km_centroids[:,ind])
maximum=max(price_max)
mean_price=maximum*sd+mean

prices = pd.DataFrame()
prices['cluster'] = range(1,len(price_max)+1)
prices['prices'] = price_max
cluster_max = prices['cluster'][prices['prices'] == maximum].iloc[0]



print('The centroid with the highest price has an average value of %.2f' %mean_price)
print('The cluster corresponding to this maximum is number %.0f' %cluster_max)

##Print the heat map for centroids

heat_datos=pd.DataFrame(km_centroids)
sns.heatmap(heat_datos, cmap="RdYlBu_r")





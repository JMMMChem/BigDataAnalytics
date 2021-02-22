import pandas as pd
import matplotlib.pyplot as plt
import time
import multiprocessing as mp
import numpy as np
from functools import partial
import threading

def check_occurrence_par(input_string,id,data,output):
    for row in data:
        output.append(row.count(input_string))
    
if __name__ == "__main__":
    
    #READ FILE AND ENSURE SEQUENCE IS UPPERCASE
    proteins = pd.read_csv('proteins.csv',sep=';')
    proteins['sequence'] = proteins['sequence'].str.upper().replace(np.nan,'')
    
    #INSERT SEQUENCES INTO A LIST
    data = proteins['sequence'].values
    
    threads = 4
    
    splitdata = np.array_split(data,threads)
    
    #REQUEST USER INPUT AND ENSURE IT IS UPPERCASE
    user_input = input("ENTER YOUR PROTEIN: ") 
    user_input = user_input.upper() 
    
    #START TIME MEASUREMENT
    start_time = time.time()
    
    # Create a list of jobs and then iterate through
	# the number of threads appending each thread to
	# the job list
    results = []
    jobs = []
    
    for i in range(0, threads):
        thread = threading.Thread(target=check_occurrence_par(user_input, i, splitdata[i],results))
        jobs.append(thread)

	# Start the threads (i.e. calculate the random number lists)
    for j in jobs:
        j.start()

	# Ensure all of the threads have finished
    for j in jobs:
        j.join()

    
    #KEEP ONLY MATCHES
    matches = pd.DataFrame()
    matches['ID'] = range(len(results))
    matches['count'] = results
    matches = matches[matches['count']>0]
    
    #PLOT HISTOGRAM
    plt.bar(matches['ID'], matches['count'], width=100)
    plt.ticklabel_format(style='plain', axis='x')
    plt.title('Protein Frecuency')
    plt.xlabel('Protein ID')
    plt.ylabel('Frequency')
    plt.show()
    
    #PRINT MAX OCURRENCE 
    #this method is slower but returns all values in case of multiple maximums
    max = matches['count'].max()
    max_sequences = matches['ID'][matches['count']==max]
    
    print("\n\nMAX VALUE OF OCCURRENCES IS:\n\n", max,"\n\nPRESENT IN THE FOLLOWING",len(max_sequences), "INDICES:\n\n", max_sequences.values)
    
    #PRINT TOTAL TIME
    total_time = time.time() - start_time
    print("\n\nEXECUTION TIME: %.2f SECONDS ---" % total_time)

        
    

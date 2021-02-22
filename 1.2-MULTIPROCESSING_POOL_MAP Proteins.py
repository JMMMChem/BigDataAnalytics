import pandas as pd
import matplotlib.pyplot as plt
import time
import multiprocessing as mp
import numpy as np
from functools import partial

def check_occurrence(input_1,data):
    results_1 = pd.DataFrame()
    results_1['ID'] = data['structureId']
    results_1['count'] = data['sequence'].str.count(input_1)
    return results_1

if __name__ == '__main__':
    #READ FILE AND ENSURE SEQUENCE IS UPPERCASE
    proteins = pd.read_csv('proteins.csv',sep=';')
    proteins['sequence'] = proteins['sequence'].str.upper().replace(np.nan,'')
    
    #REQUEST USER INPUT AND ENSURE IT IS UPPERCASE
    user_input = input("ENTER YOUR PROTEIN: ") 
    user_input = user_input.upper() 
    
    #INSERT SEQUENCES INTO A LIST
    data = pd.DataFrame(proteins)
    data1 = data[:len(data)//4]
    data2 = data[len(data)//4:2*len(data)//4]
    data3 = data[2*len(data)//4:3*len(data)//4]
    data4 = data[3*len(data)//4:4*len(data)//4]
    
    #START TIME MEASUREMENT
    start_time = time.time()
    
    #INITIALISE CLUSTER
    pool = mp.Pool(1) 
    results=[]
    arguments = [data1,data2,data3,data4]
     
    #EXECUTE FUNCTION IN PARALLEL
    fun=partial(check_occurrence,user_input)
    results = pool.map(fun,arguments)
    
    #CLOSE CLUSTER
    pool.close()   
    
    total_time = time.time() - start_time
    #UNIFY ALL PARTITIONS
    join_results = pd.concat(results)
    
    #KEEP ONLY MATCHES
    matches = pd.DataFrame()
    matches['ID'] = range(len(join_results))
    matches['count'] = join_results['count']
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
    
    print("\n\nEXECUTION TIME: %.2f SECONDS ---" % total_time)

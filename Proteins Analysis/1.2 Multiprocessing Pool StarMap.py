import pandas as pd
import matplotlib.pyplot as plt
import time
import multiprocessing as mp
import numpy as np

def occurrence(input_string,data,results):   
    for i in range(len(data)):
        results.append(data[i].count(input_string))
    return results

if __name__ == '__main__':
    
    #READ FILE AND ENSURE SEQUENCE IS UPPERCASE
    proteins = pd.read_csv('proteins.csv',sep=';')
    proteins['sequence'] = proteins['sequence'].str.upper().replace(np.nan,'')
    
    #INSERT SEQUENCES INTO A LIST
    data = proteins['sequence'].to_list()
    
    data1 = data[:len(data)//8]
    data2 = data[len(data)//8:2*len(data)//8]
    data3 = data[2*len(data)//8:3*len(data)//8]
    data4 = data[3*len(data)//8:4*len(data)//8]
    data5 = data[4*len(data)//8:5*len(data)//8]
    data6 = data[5*len(data)//8:6*len(data)//8]
    data7 = data[6*len(data)//8:7*len(data)//8]
    data8 = data[7*len(data)//8:]
    
    #REQUEST USER INPUT AND ENSURE IT IS UPPERCASE
    user_input = input("ENTER YOUR PROTEIN: ") 
    user_input = user_input.upper() 
    
    #START TIME MEASUREMENT
    start_time = time.time()
    #INITIALISE CLUSTER
    pool = mp.Pool(2)
    
    results= []
    arguments = [(user_input,data1,results),(user_input,data2,results),
                 (user_input,data3,results),(user_input,data4,results),
                 (user_input,data5,results),(user_input,data6,results),
                 (user_input,data7,results),(user_input,data8,results)]
     
    #EXECUTE FUNCTION IN PARALLEL
    results = pool.starmap(occurrence,arguments)
    
    #CLOSE CLUSTER
    pool.close()   
    
    total_time = time.time() - start_time
    #UNIFY ALL PARTITIONS
    join_results = [item for sublist in results for item in sublist]
    
    #KEEP ONLY MATCHES
    matches = pd.DataFrame()
    matches['ID'] = range(len(join_results))
    matches['count'] = join_results
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

        
    

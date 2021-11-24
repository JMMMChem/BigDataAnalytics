import pandas as pd
import matplotlib.pyplot as plt
import time

#READ FILE AND ENSURE SEQUENCE IS UPPERCASE
proteins = pd.read_csv('proteins.csv',sep=';')
proteins['sequence'] = proteins['sequence'].str.upper()

#REQUEST USER INPUT AND ENSURE IT IS UPPERCASE
user_input = input("ENTER YOUR PROTEIN: ") 
user_input = user_input.upper()

#DEFINE FUNCTION THAT RETURNS DATAFRAME WITH PROTEIN ID AND COUNT OF OCCURENCES
def check_occurrence(input,data):
    results = pd.DataFrame()
    results['ID'] = data['structureId']
    results['count'] = data['sequence'].str.count(input)
    return results

#START TIME MEASUREMENT AND EXECUTE THE FUNCTION
start_time = time.time()
results = check_occurrence(user_input, proteins)
total_time = time.time() - start_time
#KEEP ONLY THE MATCHES
matches = results[results['count']>0]

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

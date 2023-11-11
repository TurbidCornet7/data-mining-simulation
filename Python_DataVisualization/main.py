import csv
from collections import Counter
from matplotlib import pyplot as plt
import pandas as pd

def createoutput(i):
    with open (f'outputs/output{i}.csv', 'r') as file:
        with open (f'order_outputs/routput{i}.csv', 'w') as new_file:
            reader = csv.reader(file)
            index = 0
            csv_writer = csv.writer(new_file, lineterminator='\n')
            time = next(reader) #we keep the time saved somewhere
            csv_writer.writerow(['IndexAgent']+['Opinion']+['Time'])
            while 1:
                c = reader(1)
                if not c:
                    break
                elif c == '\n' or c == ' ' or c == '\t':
                    continue
                else:
                    if (index == 0): 
                        csv_writer.writerow([f'{index}']+[f'{c}']+[f'{time}'])
                    else: 
                        csv_writer.writerow([f'{index}']+[f'{c}'])
                    index += 1

def counting(index):
    with open (f'order_outputs/routput{index}.csv', 'r') as csv_file:
        csv_reader = csv.DictReader(csv_file)
        opinions_counter = Counter(['A','B'])
        firstRow = next(csv_reader)
        time = firstRow['Time']
        for row in csv_reader:
            opinions_counter.update(row['Opinion'])
        print(opinions_counter)

        opinions = []
        popularity = []
        for item in opinions_counter.items():
            opinions.append(item[0])
            popularity.append(item[1])

        print(opinions)
        print(popularity)

        return opinions,popularity, time

def createsetoutputs(end): #end is the number of the output that we have
    with open (f'routputTot.csv', 'w') as csv_file:
        csv_writer = csv.writer(csv_file, lineterminator='\n')
        csv_writer.writerow(['IndexOutput']+['OpinionA']+['OpinionB']+['Time'])
        for i in range(2,end,2):
            createoutput(i)
            opn, pop, time = counting (i)
            csv_writer.writerow([f'{i}']+[f'{pop[0]}']+[f'{pop[1]}']+[f'{time}'])

def readroutputTot (): #this function is going to be used just at the end to plot graphs reading from the cleaned outputs file
    columns = ["IndexOutput","OpinionA","OpinionB"]
    data = pd.read_csv('routputTot.csv', usecols=columns)
    df = pd.DataFrame(data)
    fig, ax = plt.subplots()
    ax.plot(df.IndexOutput, df.OpinionA)
    ax.plot(df.IndexOutput, df.OpinionB)
    plt.show()   


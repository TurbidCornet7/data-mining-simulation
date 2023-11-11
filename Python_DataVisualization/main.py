import csv
from collections import Counter
from matplotlib import pyplot as plt
import pandas as pd
import re

def createoutput(i):
    with open (f'outputs/output{i}.csv', 'r') as file:
        with open (f'order_outputs/routput{i}.csv', 'w') as new_file:
            reader = csv.reader(file)
            index = 0
            csv_writer = csv.writer(new_file, lineterminator='\n')
            time = next(reader) #we keep the time saved somewhere
            n = int(re.search(r'\d+', time[0]).group()) #if there is "Time: etc.."
            csv_writer.writerow(['IndexAgent']+['Opinion']+['Time'])
            while 1:
                c = file.read(1)
                if not c:
                    break
                elif c == '\n' or c == ' ' or c == '\t':
                    continue
                else:
                    if (index == 0): 
                        #csv_writer.writerow([f'{index}']+[f'{c}']+[f'{time[0]}'])
                        csv_writer.writerow([f'{index}']+[f'{c}']+[f'{n}']) #if there is "Time: etc.."
                    else: 
                        csv_writer.writerow([f'{index}']+[f'{c}'])
                    index += 1

def counting(index, undecided):
    with open (f'order_outputs/routput{index}.csv', 'r') as csv_file:
        csv_reader = csv.DictReader(csv_file)
        if(undecided):
            opinions_counter = Counter(['A','B','U'])
        else:
            opinions_counter = Counter(['A','B'])
        firstRow = next(csv_reader)
        time = firstRow['Time']
        print(time)
        for row in csv_reader:
            opinions_counter.update(row['Opinion'])

        opinions = []
        popularity = []
        for item in opinions_counter.items():
            opinions.append(item[0])
            popularity.append(item[1])

        print(opinions)
        print(popularity)

        return opinions,popularity, time

def createsetoutputs(end,undecided): #end is the number of the output that we have
    with open (f'routputTot.csv', 'w') as csv_file:
        csv_writer = csv.writer(csv_file, lineterminator='\n')
        if (undecided):
            csv_writer.writerow(['IndexOutput']+['OpinionA']+['OpinionB']+['OpinionU']+['Time'])
        else:
            csv_writer.writerow(['IndexOutput']+['OpinionA']+['OpinionB']+['Time'])
        for i in range(0,end,1):
            createoutput(i)
            opn, pop, time = counting (i,True)
            if (undecided):
                csv_writer.writerow([f'{i}']+[f'{pop[0]}']+[f'{pop[1]}']+[f'{pop[2]}']+[f'{time}'])
            else:
                csv_writer.writerow([f'{i}']+[f'{pop[0]}']+[f'{pop[1]}']+[f'{time}'])

def readroutputTot (): #this function is going to be used just at the end to plot graphs reading from the cleaned outputs file
    columns = ["IndexOutput","OpinionA","OpinionB", "Time"]
    data = pd.read_csv('routputTot.csv', usecols=columns)
    df = pd.DataFrame(data)
    
    fig1, ax1 = plt.subplots()
    ax1.plot(df.IndexOutput, df.OpinionA)
    ax1.plot(df.IndexOutput, df.OpinionB)

    fig2, ax2 = plt.subplots()
    ax2.plot(df.Time, df.OpinionA)
    ax2.plot(df.Time, df.OpinionB)
    plt.show()

createsetoutputs(72,True)
   


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
        for i in range(8,end,1):
            createoutput(i)
            opn, pop, time = counting (i,True)
            if (undecided):
                csv_writer.writerow([f'{i}']+[f'{pop[0]}']+[f'{pop[1]}']+[f'{pop[2]}']+[f'{time}'])
            else:
                csv_writer.writerow([f'{i}']+[f'{pop[0]}']+[f'{pop[1]}']+[f'{time}'])

def readroutputTot (undecided): #this function is going to be used just at the end to plot graphs reading from the cleaned outputs file


    if (undecided):
        columns = ["IndexOutput","OpinionA","OpinionB","OpinionU", "Time"]
        data = pd.read_csv('routputTot.csv', usecols=columns)
        df = pd.DataFrame(data)
        fig1, ax1 = plt.subplots()
        ax1.plot(df.IndexOutput, df.OpinionA, label = 'OpinionA', marker = '.')
        ax1.plot(df.IndexOutput, df.OpinionB, label = 'OpinionB', marker = '.')
        ax1.plot(df.IndexOutput, df.OpinionU, label = 'OpinionU', marker = '.')

        fig2, ax2 = plt.subplots()
        ax2.plot(df.Time, df.OpinionA, label = 'OpinionA')
        ax2.plot(df.Time, df.OpinionB, label = 'OpinionB')
        ax2.plot(df.Time, df.OpinionU, label = 'OpinionU')

    else:
        columns = ["IndexOutput","OpinionA","OpinionB", "Time"]
        data = pd.read_csv('routputTot3.csv', usecols=columns)
        df = pd.DataFrame(data)
        
        fig1, ax1 = plt.subplots()
        ax1.plot(df.IndexOutput, df.OpinionA, label = "OpinionA")
        ax1.plot(df.IndexOutput, df.OpinionB, label = "OpinionB")
        

        fig2, ax2 = plt.subplots()
        ax2.plot(df.Time, df.OpinionA, label = "OpinionA")
        ax2.plot(df.Time, df.OpinionB, label = "OpinionB")

    ax1.set_xlabel('Index of the Output')
    ax1.set_ylabel('Number of people')
    ax1.set_title('Evolution of the Simulation 3-Majority')
    ax1.legend()
    ax1.grid()
    
    ax2.set_xlabel('Time')
    ax2.set_ylabel('Number of people')
    ax2.set_title('Evolution of the Simulation 3-Majority:')
    ax2.legend()
    ax2.grid()
    
    plt.tight_layout()
    plt.show()


def compareData (): #this function is going to be used just at the end to compare the different behaviour based on the change of the number n, i should fix it 
    
    Opinions = ["1mln","10mln","100mln"]
    lastLines = [27646, 2453653, 2981019]

    fig1, ax1 = plt.subplots()
    ax1.bar(Opinions, lastLines)
    
    ax1.set_xlabel('Number of Agents')
    ax1.set_ylabel('Time of the Simulation')
    ax1.set_title('Difference of time changing n, Undecided State Dynamics')
    ax1.legend()
    plt.tight_layout()
    plt.show()

def progressionBar (undecided):

    width = 0.35
    
    if (undecided):
        columns = ["IndexOutput","OpinionA","OpinionB","OpinionU", "Time"]
    else:
        columns = ["IndexOutput","OpinionA","OpinionB", "Time"]
    data = pd.read_csv('routputTot2.csv', usecols=columns)
    df = pd.DataFrame(data)

    plt.bar(df.IndexOutput-width, df.OpinionA, width = 0.35, label = "A")
    plt.bar(df.IndexOutput, df.OpinionB, width = 0.35, label = "B")
    if (undecided): 
        plt.bar(df.IndexOutput+width, df.OpinionU,width = 0.35, label = "U")

    plt.xlabel('Number of the Iteration')
    plt.ylabel('Number of people')
    plt.title('Evolution of the popularity of the 2 opinions, 10mln - 2-Choice')
    plt.legend()
    plt.grid()
    
    plt.tight_layout()
    plt.show()


def table ():

    fig, ax = plt.subplots()

    # hide axes
    fig.patch.set_visible(False)
    ax.axis('off')
    ax.axis('tight')

    columns_names = ["1mln, 10mln, 100mln"]

    data = {
        "1mln": [898144, 586594, 27646],
        "10mln": [4279623, 3977216, 2453653],
        "100mln": [-1, 13743251, 2981019]
    }

    df = pd.DataFrame(data, index = ["2-Choices", "3-Majority", "Undecided State Dynamics"])

    print(df)
    ax.table(cellText=df.values, rowLabels = ["2-Choices", "3-Majority", "Undecided State Dynamics"], colLabels=df.columns, loc='center')
    
    fig.tight_layout()

    plt.tight_layout()
    plt.show()

#readroutputTot(False)
compareData()

#table()
   


import csv
from collections import Counter
from matplotlib import pyplot as plt

def createoutput(i):
    with open (f'outputs/output{i}.csv', 'r') as file:
        with open (f'order_outputs/routput{i}.csv', 'w') as new_file:
            index = 0
            csv_writer = csv.writer(new_file, lineterminator='\n')
            csv_writer.writerow(['IndexAgent']+['Opinion'])
            while 1:
                c = file.read(1)
                if not c:
                    break
                elif c == '\n' or c == ' ' or c == '\t':
                    continue
                else:
                    csv_writer.writerow([f'{index}']+[f'{c}'])
                    index += 1

def counting(index):
    with open (f'order_outputs/routput{index}.csv', 'r') as csv_file:
        csv_reader = csv.DictReader(csv_file)
        opinions_counter = Counter(['A','B'])
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

        return opinions,popularity
        # fig, ax = plt.subplots()
        # ax.bar (opinions,popularity)
        # ax.set_title ("Distribution of Opinions")
        # ax.set_xlabel ("Opinions")
        # ax.set_ylabel ("Quantity")

        # plt.show()

def createsetoutputs(end): #end is the number of the output that we have
    with open (f'routputTot.csv', 'w') as csv_file:
        csv_writer = csv.writer(csv_file, lineterminator='\n')
        csv_writer.writerow(['IndexOutput']+['OpinionA']+['OpinionB'])
        for i in range(2,end,2):
            createoutput(i)
            opn, pop = counting (i)
            csv_writer.writerow([f'{i}']+[f'{pop[0]}']+[f'{pop[1]}'])


createsetoutputs(3)


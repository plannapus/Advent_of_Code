import csv
a=csv.reader(open("input01.txt"))
input=[]
calories=[]
for i in a:
    if len(i)>0:
        input.append(int(i[0]))
    else:
        calories.append(sum(input))
        input=[]

calories.append(sum(input))

max(calories)
#69528

calories.sort()
sum(calories[-3:])
#206152
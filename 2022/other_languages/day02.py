outcomes1 = {"A X":4,"A Y":8,"A Z":3,
"B X":1,"B Y":5,"B Z":9,
"C X":7,"C Y":2,"C Z":6}

outcomes2 = {"A X":3,"A Y":4,"A Z":8,
"B X":1,"B Y":5,"B Z":9,
"C X":2,"C Y":6,"C Z":7}

with open('input02.txt') as f:
    input = [line.strip() for line in f.readlines()]

sum([outcomes1[k] for k in input])
#9651

sum([outcomes2[k] for k in input])
#10560
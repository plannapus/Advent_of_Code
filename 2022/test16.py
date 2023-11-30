import time,sys
import re
from collections import deque
import networkx as nx
start=time.time()

inf = sys.argv[1] if len(sys.argv) > 1 else '16.in'
G = nx.Graph()
valves = dict()
with open(inf) as fi:
    lines = fi.read()[:-1].strip(',').split('\n')
    for line in lines:
        l = line.strip(',')
        a = l.split(' ')[9:]
        for i  in range(len(a)):
            a[i] = a[i][:2]
        rate = [int(x) for x in re.findall(r'[-]?\d+',line)]
        #valves['AA'] = 0
        if rate[0] > 0:
            valves[l.split(' ')[1]]= rate[0]
        for x in a:
            G.add_edge(l.split(' ')[1],x)
            G.add_edge(x,l.split(' ')[1])
print(valves)
distances = dict()
for v1 in G.nodes():
    for v2 in G.nodes():
        if v1 != v2:
            distances[(v1,v2)] = len(nx.shortest_path(G,v1,v2))-1

def findnextValve(s,openvalves):
    nodes = deque([(s,set())])
    posibleValves = set()
    while nodes:
        start, visited = nodes.popleft()
        visited.add(start)
        for n in valves.keys():
            if n not in visited and n != start:
                nodes.append((n,visited))
                if n not in openvalves.keys() and n != s:
                    posibleValves.add((n,distances[(s,n)]))
    return posibleValves

def release(openvalves,maxmin):
    pressure = 0
    for valve in openvalves.keys():
        pressure += valves[valve]*(maxmin-openvalves[valve])
    return pressure

def releasepressure(start,openvalves,maxmin, E = False):
    valve = start
    paths = deque([(valve, 0, openvalves,False,0,0)])
    finalpaths = deque([])
    while paths:
        valve, dist, openvalves, moving, mins , released = paths.popleft()
        if moving:
            if mins +dist< maxmin+1:
                paths.append((valve, 0, openvalves, not moving , mins+dist,0))
            else:
                #if maxmin == 26 and not E:
                    #pathsE = releasepressure('AA',openvalves,26,True)
                    #for _,_,o,_,_,_ in pathsE:
                        #openvalves = openvalves | o
                        #finalpaths.append((valve, 0, openvalves, not moving, mins,release(openvalves,maxmin)))
                #else:
                    #finalpaths.append((valve, 0, openvalves, not moving, mins,release(openvalves,maxmin)))
                finalpaths.append((valve, 0, openvalves, not moving, mins,release(openvalves,maxmin)))
        else:
            mins +=1
            if valve != start:
                openvalves[valve]=mins-1
            nextvalves = findnextValve(valve,openvalves)
            if len(nextvalves)>0:
                for nvalve, dist in nextvalves:
                    paths.append((nvalve,dist, openvalves.copy(),not moving, mins,0))
            else:
                #if maxmin == 26 and not E:
                    #pathsE = releasepressure('AA',openvalves,26,True)
                    #for _,_,o,_,_,_ in pathsE:
                        #openvalves = openvalves | o
                        #finalpaths.append((valve, 0, openvalves, not moving, mins,release(openvalves,maxmin)))
                #else:
                    #finalpaths.append((valve, 0, openvalves, not moving, mins,release(openvalves,maxmin)))
                finalpaths.append((valve, 0, openvalves, not moving, maxmin,release(openvalves,maxmin)))
    return finalpaths

paths = releasepressure('AA',dict(),30)
rpressures = set()
for valve,_,openvalves,moving,mins,releasedpressure in paths:
    if releasedpressure > 1000:
        rpressures.add(releasedpressure)
print('part1:', max(rpressures))

print('part2 -finding paths...')
paths = releasepressure('AA',dict(),26)
rpressures = deque([])
for valve,_,openvalves,moving,mins,releasedpressure in paths:
    if releasedpressure > 1000:
        rpressures.append((openvalves,releasedpressure))
print('part2 - find maximum of elephant and player')

maxpressures= set()

i=0
for o1,r1 in rpressures:
    i+=1
    for o2, r2 in rpressures:
        if o1.keys().isdisjoint(o2.keys()):
            #print(r1+r2,o1,o2)
            maxpressures.add(r1+r2)
    if i %1000==0:
        print(i)
print('part2:', max(maxpressures))

end=time.time()

print(round(end-start,5))
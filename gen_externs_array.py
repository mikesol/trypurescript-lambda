import json
from collections import defaultdict
import os

import os
import subprocess
o = []
for root, dirs, files in os.walk("staging/output", topdown=False):
    for name in files:
        if name.split('.')[-1] == 'cbor':
            o.append(os.path.join(root, name).replace('staging/output/', ''))

srcs = subprocess.run(["npx", "spago", "sources"],
                      cwd='staging', capture_output=True)

flz = [x for x in srcs.stdout.decode(
    'utf-8').split('\n') if x != 'src/**/*.purs' and x != '']
dpcmd = ["npx", "purs", "graph"]+flz
deps = subprocess.run(dpcmd, cwd='staging', capture_output=True)
graph = json.loads(deps.stdout.decode('utf-8'))


class Graph:

    def __init__(self, n):
        self.graph = defaultdict(list)
        self.N = n
    def addEdge(self, m, n):
        self.graph[m].append(n)
    def sortUtil(self, n, visited, stack):
        visited[n] = True
        for element in self.graph[n]:
            if visited[element] == False:
                self.sortUtil(element, visited, stack)
        stack.insert(0, n)

    def topologicalSort(self):
        visited = [False]*self.N
        stack = []
        for element in range(self.N):
            if visited[element] == False:
                self.sortUtil(element, visited, stack)
        return stack


ks = [k for k in graph.keys()]

g = Graph(len(graph))
for k, v in graph.items():
  for e in v['depends']:
    g.addEdge(ks.index(e), ks.index(k))

ts = g.topologicalSort()
#print(ks[0])

#print(len(ts), len(o))
o = [ks[x]+'/externs.cbor' for x in ts]

print(o)

with open('src/Externs.hs', 'w') as externs:
    externs.write("""module Externs(externFileList) where

externFileList :: [String]
externFileList = [""")
    externs.write(','.join(['"%s"' % x for x in o]))
    externs.write(']')

import json
from random import randint

class Vertex():

  def __init__(self, id):
    self.id = id
    self.neighbours = []
  
  def add_neighbour(self, vertex):
    if vertex.id not in map(lambda node: node.id, self.neighbours):
      self.neighbours.append(vertex)

class Graph():
  
  def __init__(self):
    self.vertices = []
  
  def add_vertex(self, vertex):
    if vertex.id not in map(lambda vertex: vertex.id, self.vertices):
      self.vertices.append(vertex)
  
  def connect_vertices(self, vertex1, vertex2):
    vertex1.add_neighbour(vertex2)
    vertex2.add_neighbour(vertex1)
  
  def to_json(self, filename):
    def get_conflicts():
      conflicts = []
      for v in self.vertices:
        for u in v.neighbours:
          conflicts.append({
            'zrzeda': v.id,
            'nielubiany': u.id
          })
      return conflicts
    graph_as_dict = {
      'studenci': len(self.vertices),
      'konflikty': get_conflicts()
    }
    with open(filename, 'w') as file:
      json.dump(graph_as_dict, file, indent=2)


def random_4colorable_graph(max_vertices):
  from random import choice
  graph = Graph()
  for i in range(max_vertices):
    vertex = Vertex(i)
    graph.add_vertex(vertex)
  for v in graph.vertices:
    for _ in range(3):
      attempts = 50
      while attempts > 0:
        u = choice(graph.vertices)
        if v != u and len(u.neighbours) < 4:
          graph.connect_vertices(v, u)
          break
        attempts -= 1
  return graph

def random_graph(max_vertices):
  from random import choice
  graph = Graph()
  for i in range(max_vertices):
    vertex = Vertex(i)
    graph.add_vertex(vertex)
  for v in graph.vertices:
      attempts = 1000
      max_nbs  = randint(max_vertices-30, 3 * max_vertices // 4)
      to_connect = randint(0, 10)
      connected = 0
      while attempts > 0:
        u = choice(graph.vertices)
        if v != u and len(u.neighbours) < max_nbs:
          graph.connect_vertices(v, u)
          connected += 1
        if connected > to_connect:
          break
        attempts -= 1
  return graph


# random_4colorable_graph(10).to_json('graph.json')
for i in range(20):
  random_graph(randint(10, 40)).to_json(f'./graphs/graph{20+i}.json')
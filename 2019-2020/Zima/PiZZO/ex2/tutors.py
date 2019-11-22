import json


class Vertex():

  def __init__(self, id):
    self.id = id
    self.neighbours = []
  
  def add_neighbour(self, vertex):
    if vertex.id not in map(lambda node: node.id, self.neighbours):
      self.neighbours.append(vertex)
  
  def __str__(self):
    return f"""vertex id: {self.id}
    neighbours: {', '.join(map(lambda node: str(node.id), self.neighbours))}"""
  

class Graph():
  
  def __init__(self):
    self.vertices = []
  
  def add_vertex(self, vertex):
    if vertex.id not in map(lambda vertex: vertex.id, self.vertices):
      self.vertices.append(vertex)
  
  def connect_vertices(self, vertex1, vertex2):
    vertex1.add_neighbour(vertex2)
    vertex2.add_neighbour(vertex1)
  
  def reindex_vertices(self):
    for i, vertex in enumerate(self.vertices):
      vertex.id = i
  
  def __str__(self):
    vertices_info = []
    for node in self.vertices:
      vertices_info.append(str(node))
    return '\n'.join(vertices_info)
  

class StudentsGraph(Graph):

  def __init__(self, filename):
    self.normal_students = []
    self.whiners = []
    super().__init__()
    self.init_from_file(filename)
  
  def init_from_file(self, filename):
    with open(filename, 'r') as file:
      data = json.load(file)
      for id in range(1, data['studenci']+1):
        vertex = Vertex(id)
        self.add_vertex(vertex)
      for conflict in data['konflikty']:
        whiner = self.vertices[conflict['zrzeda']-1]
        self.add_whiner(whiner)
        self.add_vertex(whiner)
        normal_student = self.vertices[conflict['nielubiany']-1]
        self.add_vertex(normal_student)
        self.connect_vertices(whiner, normal_student)
  
  def add_whiner(self, whiner):
    if whiner.id not in map(lambda whiner: whiner.id, self.whiners):
      self.whiners.append(whiner)
  
  def remove_irrevelant_vertices(self):
    """Leaves only those vertices that can make impact on coloring (coloring of victims of degree < 4 is irrevelant)"""
    self.vertices = list(filter(lambda vertex: vertex in self.whiners or len(vertex.neighbours) > 3, self.vertices))
    for vertex in self.vertices:
      vertex.neighbours = list(filter(lambda v: v in self.vertices, vertex.neighbours))
    self.reindex_vertices()
  
  def to_SAT(self):
    def tutor1(student):
      return str(4*student+1)
    def tutor2(student):
      return str(4*student+2)
    def tutor3(student):
      return str(4*student+3)
    def tutor4(student):
      return str(4*student+4)
    def not_tutor(tutor, student):
      return '-' + tutor(student)
    def at_least_one_tutor(student):
      return [tutor(student) for tutor in [tutor1, tutor2, tutor3, tutor4]]
    def not_both_tutors(student, t1, t2):
      return [not_tutor(tutor, student) for tutor in [t1, t2]]
    def exactly_one_tutor(student):
      return [
        at_least_one_tutor(student),
        not_both_tutors(student, tutor1, tutor2),
        not_both_tutors(student, tutor1, tutor3),
        not_both_tutors(student, tutor1, tutor4),
        not_both_tutors(student, tutor2, tutor3),
        not_both_tutors(student, tutor2, tutor4),
        not_both_tutors(student, tutor3, tutor4)
      ]
    def not_the_same_tutor(student1, student2, tutor):
      return [
        not_tutor(tutor, student1),
        not_tutor(tutor, student2)
      ]
    def not_the_same_tutors(student1, student2):
      return [not_the_same_tutor(student1, student2, tutor) for tutor in [tutor1, tutor2, tutor3, tutor4]]
    def format_clauses(clauses):
      return '\n'.join(list(map(lambda clause: ' '.join(clause+['0']), clauses)))

    self.remove_irrevelant_vertices()
    clauses = []
    visited = []
    for vertex in self.vertices:
      visited.append(vertex)
      clauses += exactly_one_tutor(vertex.id)
      for neighbour in vertex.neighbours:
        if neighbour not in visited:
          clauses += not_the_same_tutors(vertex.id, neighbour.id)
    return f'p cnf {4*len(self.vertices)} {len(clauses)}\n' + format_clauses(clauses)

  def __str__(self):
    return '\n'.join([f'whiners: {list(map(lambda whiner: whiner.id, self.whiners))}',
                      f'normal students: {list(map(lambda student: student.id, self.normal_students))}',
                      f'graph: \n{super().__str__()}'])


        


sg = StudentsGraph('problem.json')
print(sg.to_SAT())

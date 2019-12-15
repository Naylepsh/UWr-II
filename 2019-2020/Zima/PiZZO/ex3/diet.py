import json
from z3 import *


with open('data.json', 'r', encoding='utf-8') as infile:
  data = json.load(infile)
print(data)

solver = Solver()
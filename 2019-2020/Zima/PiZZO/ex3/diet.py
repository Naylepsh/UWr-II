import json
from z3 import *


MEALS = ['śniadanie', 'lunch', 'obiad', 'podwieczorek', 'kolacja']


def var_name(name, meal):
  return name.replace(' ', '_') + '_' + meal


def provide_variables(outfile, ingredients):
  """creates int variables for all pairs of vars and meal types"""
  for ingredient in ingredients:
    for meal in MEALS:
      v = var_name(ingredient['nazwa'], meal)
      outfile.write(f'{v} = Int("{v}")\n')


def provide_equations(outfile, ingredients, param, target):
  for meal in MEALS:
    right_side = '+'.join([
      f'{var_name(ingredient["nazwa"], meal)}*{ingredient[param]}' for ingredient in ingredients
      ])
    outfile.write(f'{meal+"_"+param}={right_side}\n')
  outfile.write(f'{"+".join([meal+"_"+param for meal in MEALS])} >= {target["min"]}\n')
  outfile.write(f'{"+".join([meal+"_"+param for meal in MEALS])} <= {target["max"]}\n')
    

if __name__ == '__main__':
  with open('tests/data.json', 'r', encoding='utf-8') as infile:
    data = json.load(infile)
  
  with open('tests/solve.py', 'w', encoding='utf-8') as outfile:
    provide_variables(outfile, data['składniki'])
    for param in data['parametry']:
      provide_equations(outfile, data['składniki'], param, data['cel'][param])
  solver = Solver()
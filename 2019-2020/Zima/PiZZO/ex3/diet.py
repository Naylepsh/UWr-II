import json
from z3 import *


def provide_variables(outfile, variables):
  for variable in variables:
    outfile.write(f'{variable} = Int("{variable}")\n')

if __name__ == '__main__':
  with open('tests/data.json', 'r', encoding='utf-8') as infile:
    data = json.load(infile)
  ingredients = [0]*len(data['składniki'])
  with open('tests/solve.py', 'w', encoding='utf-8') as outfile:
    provide_variables(outfile, list(map(lambda ingredient: ingredient['nazwa'].replace(' ', '_'), data['składniki'])))
    # for param in data['parametry']:
    #   for ingredient in data['skladniki']:

  # for ingredient in data['skladniki']:
  #   ingredients[i] = Int(ingredient.name)
  solver = Solver()
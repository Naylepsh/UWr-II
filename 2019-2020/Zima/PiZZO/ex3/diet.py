import json
from z3 import *
from functools import reduce
import sys


MEALS = ['sniadanie', 'lunch', 'obiad', 'podwieczorek', 'kolacja']

class Ingredient():

  def __init__(self, ingredient_dict, params):
    self.name = ingredient_dict['nazwa']
    self.params = { param : ingredient_dict[param] for param in params }
    self.vars = { self.var_name(meal) : Int(self.var_name(meal)) for meal in MEALS }
  
  def var_name(self, meal):
    return self.name + ' ' + meal
  
  def get_var(self, meal):
    return self.vars[self.var_name(meal)]
  
  def value(self, meal, param):
    return ToReal(self.vars[self.var_name(meal)]) * self.params[param]
  
  def __repr__(self):
    return self.name


def create_ingredient_vars(ingredients, params):
  return { ingredient['nazwa'] : Ingredient(ingredient, params) for ingredient in ingredients } 


def create_meal_vars(params):
  return { param : { meal : Int(param+' '+meal) for meal in MEALS} for param in params }


def create_meal_assertions(solver, meal_vars, ingredient_vars, target):
  for meal in MEALS:
    things_eaten = sum([ingredient.get_var(meal) for ingredient in ingredient_vars.values()])
    solver.add(things_eaten > 0)
  for param in meal_vars:
    total_value = 0
    for meal in meal_vars[param]:
      meal_value = sum([ingredient.value(meal, param) for ingredient in ingredient_vars.values()])
      total_value += meal_value
    solver.add(And(
      total_value >= target[param]['min'],
      total_value <= target[param]['max']))


def create_ingredient_assertions(solver, ingredients, target):
  for ingredient in ingredients.values():
    for var in ingredient.vars:
      solver.add(And(
        ingredient.vars[var] >= 0, 
        ingredient.vars[var] <= min([ (target[param]['max'] // ingredient.params[param]) + 1 for param in ingredient.params])))


def create_conflict_assertions(solver, ingredients, conflicts):
  for conflict in conflicts:
    left = ingredients[conflict['nazwa1']]
    right = ingredients[conflict['nazwa2']]
    for meal in MEALS:
      var_left = left.get_var(meal)
      var_right = right.get_var(meal)
      solver.add(
        And(
          Implies(var_left  > 0, var_right == 0),
          Implies(var_right > 0, var_left  == 0)
      ))

if __name__ == '__main__':
  # path_to_file = input('Enter a path to a file: ')
  path_to_file = sys.argv[1]
  with open(path_to_file, 'r') as infile:
    data = json.load(infile)
  solver = Solver()
  ingredient_vars = create_ingredient_vars(data['skladniki'], data['parametry'])
  meal_vars = create_meal_vars(data['parametry'])
  create_ingredient_assertions(solver, ingredient_vars, data['cel'])
  create_meal_assertions(solver, meal_vars, ingredient_vars, data['cel'])
  create_conflict_assertions(solver, ingredient_vars, data['konflikty'])
  if solver.check() == sat:
    model = solver.model()
    for meal in MEALS:
      print(meal+': ', end='')
      used_ingredients = list(filter(lambda x: model.evaluate(x.get_var(meal)).as_long() > 0, ingredient_vars.values()))
      food = []
      for ingredient in used_ingredients:
        food += [str(ingredient)] * model.evaluate(ingredient.get_var(meal)).as_long()
      print(', '.join(food))
  else:
    print('Nie mozna wygenerowac diety.')
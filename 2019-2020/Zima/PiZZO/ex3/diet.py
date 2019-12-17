import json
from z3 import *
from functools import reduce
from time import time


MEALS = ['sniadanie', 'lunch', 'obiad', 'podwieczorek', 'kolacja']

class Ingredient():
  params = {}

  def __init__(self, ingredient_dict, params):
    self.name = ingredient_dict['nazwa']
    if not Ingredient.params:
      Ingredient.params = { param : ingredient_dict[param] for param in params }
    self.vars = { self.var_name(meal) : Int(self.var_name(meal)) for meal in MEALS }
  
  def var_name(self, meal):
    return self.name + ' ' + meal
  
  def get_var(self, meal):
    return self.vars[self.var_name(meal)]
  
  def value(self, meal, param):
    return ToReal(self.vars[self.var_name(meal)]) * Ingredient.params[param]


def create_ingredient_vars(ingredients, params):
  return { ingredient['nazwa'] : Ingredient(ingredient, params) for ingredient in ingredients } 


def create_meal_vars(params):
  return { param : { meal : Int(param+' '+meal) for meal in MEALS} for param in params }


def create_meal_assertions(meal_vars, ingredient_vars, target):
  assertions = []
  for param in meal_vars:
    total_value = 0
    for meal in meal_vars[param]:
      meal_value = sum([ingredient.value(meal, param) for ingredient in ingredient_vars.values()])
      total_value += meal_value
    assertions.append(And(
      total_value >= target[param]['min'],
      total_value <= target[param]['max']))
  return assertions


def create_ingredient_assertions(ingredients, target):
  assertions = []
  for ingredient in ingredients.values():
    for var in ingredient.vars:
      assertions.append(And(
        ingredient.vars[var] >= 1, 
        ingredient.vars[var] <= min([ target[param]['max'] // ingredient.params[param] for param in ingredient.params])))
  return assertions


def create_conflict_assertions(ingredients, conflicts):
  assertions = []
  for conflict in conflicts:
    left = ingredients[conflict['nazwa1']]
    right = ingredients[conflict['nazwa2']]
    for meal in MEALS:
      var_left = left.get_var(meal)
      var_right = right.get_var(meal)
      assertions.append(
        And(
          Implies(var_left  > 0, var_right == 0),
          Implies(var_right > 0, var_left  == 0)
      ))
  return assertions

if __name__ == '__main__':
  with open('tests/data.json', 'r') as infile:
    data = json.load(infile)
  s = time()
  ingredient_vars = create_ingredient_vars(data['skladniki'], data['parametry'])
  meal_vars = create_meal_vars(data['parametry'])
  ingredient_assertions = create_ingredient_assertions(ingredient_vars, data['cel'])
  meal_assertions = create_meal_assertions(meal_vars, ingredient_vars, data['cel'])
  conflict_assertions = create_conflict_assertions(ingredient_vars, data['konflikty'])
  print(time() - s)
  solve(*(ingredient_assertions+meal_assertions+conflict_assertions))
import json
from z3 import *
from functools import reduce


MEALS = ['sniadanie', 'lunch', 'obiad', 'podwieczorek', 'kolacja']


class Ingredient():

  def __init__(self, ingredient_dict, nutrients):
    self.name = ingredient_dict['nazwa']
    self.nutrients = { nutrient : ingredient_dict[nutrient] for nutrient in nutrients}
    self.vars = { self.var_name(meal) : Int(self.var_name(meal)) for meal in MEALS }
  
  def var_name(self, meal):
    return self.name + ' ' + meal
  
  def get_var(self, meal):
    return self.vars[self.var_name(meal)]
  
  def value(self, meal, nutrient):
    return ToReal(self.vars[self.var_name(meal)]) * self.nutrients[nutrient]
  
  def nutritional_value(self, nutrient):
    return self.nutrients[nutrient]
  
  def __repr__(self):
    return self.name


def create_ingredient_vars(ingredients, params):
  return { ingredient['nazwa'] : Ingredient(ingredient, params) for ingredient in ingredients } 


def create_meal_vars(nutrients):
  return { param : { meal : Int(param+' '+meal) for meal in MEALS} for param in nutrients }


def create_meal_assertions(solver, meal_vars, ingredient_vars, target):
  for meal in MEALS:
    things_eaten = sum([ingredient.get_var(meal) for ingredient in ingredient_vars.values()])
    solver.add(things_eaten > 0)
  for nutrient in meal_vars:
    total_value = 0
    for meal in meal_vars[nutrient]:
      meal_value = sum([ingredient.value(meal, nutrient) for ingredient in ingredient_vars.values()])
      total_value += meal_value
    solver.add(And(
      total_value >= target[nutrient]['min'],
      total_value <= target[nutrient]['max']))


def create_ingredient_assertions(solver, ingredients, target):
  for ingredient in ingredients.values():
    for var in ingredient.vars:
      solver.add(And(
        ingredient.vars[var] >= 0, 
        ingredient.vars[var] <= min([ (target[param]['max'] // ingredient.nutritional_value(param)) for param in ingredient.nutrients])))


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


def create_vars(solver, data):
  return (create_ingredient_vars(data['skladniki'], data['parametry']),
          create_meal_vars(data['parametry']))


def provide_assertions(solver, vars, data):
  ingredient_vars, meal_vars = vars
  create_ingredient_assertions(solver, ingredient_vars, data['cel'])
  create_meal_assertions(solver, meal_vars, ingredient_vars, data['cel'])
  create_conflict_assertions(solver, ingredient_vars, data['konflikty'])


def print_model(solver, vars):
  ingredient_vars, meal_vars = vars
  model = solver.model()
  for meal in MEALS:
    print(meal+': ', end='')
    used_ingredients = list(filter(lambda x: model.evaluate(x.get_var(meal)).as_long() > 0, ingredient_vars.values()))
    food = []
    for ingredient in used_ingredients:
      food += [str(ingredient)] * model.evaluate(ingredient.get_var(meal)).as_long()
    print(', '.join(food))


if __name__ == '__main__':
  path_to_file = input('Enter a path to a file: ')
  with open(path_to_file, 'r') as infile:
    data = json.load(infile)

  solver = Solver()
  vars = create_vars(solver, data)
  provide_assertions(solver, vars, data)

  if solver.check() == sat:
    print_model(solver, vars)
  else:
    print('Nie mozna wygenerowac diety.')
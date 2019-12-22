import json
from z3 import *
import sys


MEALS = ['sniadanie', 'lunch', 'obiad', 'podwieczorek', 'kolacja']


def id_creator():
  """
  Helper function creating new string id when called

  How to use:
  new_id = id_creator()
  first_id = new_id()
  second_id = new_id()
  and so on..
  """
  id = { 'value' : 0 }
  def new_id():
    old = id['value']
    id['value'] += 1
    return str(old)
  return new_id


def create_food_table(ingredients, nutrients):
  return { ingredient['nazwa'] : { nutrient : ingredient[nutrient] for nutrient in nutrients } for ingredient in ingredients }


def create_ingredient_vars(solver, generate_id, ingredients):
  xs = { ingredient['nazwa'] : Int(generate_id()) for ingredient in ingredients }
  for var in xs.values():
    solver.add(var >= 0, var <= sys.maxsize)
  return xs


def provide_meal_assertions(solver, meal_vars):
  for meal in meal_vars.values():
    solver.add(Sum(list(meal.values())) > 0)


def provide_nutrition_assertions(solver, food_table, target, meal_vars, s):
  for nutrient in target:
    value_per_ingredient = []
    for ingredient in food_table:
      value_per_ingredient.append(Sum([ ToReal(meal_vars[meal][ingredient]) * food_table[ingredient][nutrient] for meal in meal_vars ]))
    total_value = Sum(value_per_ingredient)
    solver.add(total_value >= target[nutrient]['min'], total_value <= target[nutrient]['max'])


def provide_conflict_assertions(solver, conflicts, meal_vars):
  for conflict in conflicts:
    x = conflict['nazwa1']
    y = conflict['nazwa2']
    for vars in meal_vars.values():
      solver.add(Or(Not(vars[x]  > 0),Not(vars[y] > 0)))


def print_model(solver, meal_vars):
  model = solver.model()
  for meal in meal_vars:
    print(f'{meal}: ', end='')
    food = []
    for ingredient in meal_vars[meal]:
      food += [ingredient] * model.evaluate(meal_vars[meal][ingredient]).as_long()
    print(', '.join(food))


def json_model(solver, meal_vars, filename):
  model = solver.model()
  solution = {}
  for meal in meal_vars:
    food = []
    for ingredient in meal_vars[meal]:
      food += [ingredient] * model.evaluate(meal_vars[meal][ingredient]).as_long()
    solution[meal] = food
  with open(filename, 'w', encoding='utf-8') as file:
    json.dump(solution, file, indent=2)


if __name__ == '__main__':
  path_to_file = input()
  with open(path_to_file, 'r', encoding='utf-8') as infile:
    data = json.load(infile)

  food_table = create_food_table(data['składniki'], data['parametry'])
  
  solver = Solver()
  generate_id = id_creator()
  meal_vars = { meal : create_ingredient_vars(solver, generate_id, data['składniki']) for meal in MEALS }
  provide_meal_assertions(solver,meal_vars)
  provide_nutrition_assertions(solver, food_table, data['cel'], meal_vars, data['składniki'])
  provide_conflict_assertions(solver, data['konflikty'], meal_vars)

  if solver.check() == sat:
    if len(sys.argv) == 3 and sys.argv[1] == 'json':
      json_model(solver, meal_vars, sys.argv[2])
    else:
      print_model(solver, meal_vars)
  else:
    print('Nie można wygenerować diety.')
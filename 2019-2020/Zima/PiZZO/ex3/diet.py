import json
from z3 import *


MEALS = ['sniadanie', 'lunch', 'obiad', 'podwieczorek', 'kolacja']


class Ingredient():
  """
  The Ingredient object containg name and nutritional values of an ingredient. 
  Comes also with SMT variables for each of the meal.
  """

  def __init__(self, ingredient_info, nutrients):
    """
    Parameters:
    ingredient_info (dict): Dictionary containing nutrition information
    nutrients (list): List of nutrients to keep track of
    """
    self.name = ingredient_info['nazwa']
    self.nutrients = { nutrient : ingredient_info[nutrient] for nutrient in nutrients}
    self.vars = self._init_vars()
  
  def _init_vars(self):
    """
    Returns:
    vars (dict): Dictionary of SMT variables for each of the meal
    """
    return { self.var_name(meal) : Int(self.var_name(meal)) for meal in MEALS }
  
  def var_name(self, meal):
    return self.name + ' ' + meal
  
  def get_var(self, meal):
    return self.vars[self.var_name(meal)]
  
  def value(self, meal, nutrient):
    """
    Value = Occurences_of_the_ingredient_in_a_meal * it's_nutrition_value

    Parameters:
    meal (string)
    nutrient (string)

    Returns:
    value (SMT.Real)
    """
    return ToReal(self.get_var(meal)) * self.nutritional_value(nutrient)
  
  def nutritional_value(self, nutrient):
    return self.nutrients[nutrient]
  
  def __repr__(self):
    return self.name


def create_ingredient_vars(ingredients, nutrients):
  """
  Creates ingredient SMT variables.

  Parameters:
  ingredients (list): List of dicts containing the name and nutrition values of an ingredient
  nutrients (list): List of nutrients to keep track of

  Returns:
  variables (dict): A (name : Ingredient) dictionary
  """
  return { ingredient['nazwa'] : Ingredient(ingredient, nutrients) for ingredient in ingredients } 


def create_meal_vars(nutrients):
  """
  Creates meal SMT variables grouped by nutrients

  Parameters:
  nutrients (list): List of nutrients to keep track of

  Returns:
  variables (dict): Dictionary of nutrients, each containing a dictionary of nutrition-value-of-a-meal SMT variables
  """
  return { param : { meal : Int(param+' '+meal) for meal in MEALS} for param in nutrients }


def create_meal_assertions(solver, meal_vars, ingredient_vars, target):
  """
  Provides SMT solver with meal assertions such as:
  - Each meal is not empty
  - Total sum of each nutrient of all meals is kept within given boundaries

  Parameters:
  solver (z3solver): SMT solver
  meal_vars (dict): Dictionary of nutrients, each containing a dictionary of nutrition-value-of-a-meal SMT variables
  ingredient_vars (dict):  A (name : Ingredient) dictionary
  target (dict): Dictionary of nutrients, each containing a dict of min and max boundaries
  """
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


def create_ingredient_assertions(solver, ingredients_vars, target):
  """
  Provides SMT solver with ingredient occurence boundaries

  Parameters:
  solver (z3solver): SMT solver
  ingredient_vars (dict):  A (name : Ingredient) dictionary
  target (dict): Dictionary of nutrients, each containing a dict of min and max boundaries
  """
  for ingredient in ingredients_vars.values():
    for var in ingredient.vars:
      solver.add(And(
        ingredient.vars[var] >= 0, 
        ingredient.vars[var] <= min([ (target[param]['max'] // ingredient.nutritional_value(param)) for param in ingredient.nutrients])))


def create_conflict_assertions(solver, ingredients_vars, conflicts):
  """
  Provides SMT solver with conditions preventing ingredients-in-conflict from being grouped up together

  Parameters:
  solver (z3solver): SMT solver
  ingredients_vars (dict):  A (name : Ingredient) dictionary
  conflicts (list): List of dictionaries of pair of ingredients-in-conflict

  """
  for conflict in conflicts:
    left = ingredients_vars[conflict['nazwa1']]
    right = ingredients_vars[conflict['nazwa2']]
    for meal in MEALS:
      var_left = left.get_var(meal)
      var_right = right.get_var(meal)
      solver.add(
        And(
          Implies(var_left  > 0, var_right == 0),
          Implies(var_right > 0, var_left  == 0)
      ))


def create_vars(solver, data):
  """
  Creates SMT ingredient and meal variables

  Parameters:
  solver (z3solver): SMT solver
  data (dict): Dictionary containing ingredient and nutrition informations

  Returns:
  ingredient_and_meal_vars (tuple): Tuple of two data structures containing ingredient and meal SMT variables
  """
  return (create_ingredient_vars(data['skladniki'], data['parametry']),
          create_meal_vars(data['parametry']))


def provide_assertions(solver, vars, data):
  """
  Provides SMT solver with ingredient occurence, meal nutrition and ingredient conflict assertions

  Parameters:
  solver (z3solver): SMT solver
  vars (tuple): Tuple of two data structers, first containing ingredient SMT vars, second containg meal ones
  data (dict): Dictionary containing ingredient and nutrition informations
  """
  ingredient_vars, meal_vars = vars
  create_ingredient_assertions(solver, ingredient_vars, data['cel'])
  create_meal_assertions(solver, meal_vars, ingredient_vars, data['cel'])
  create_conflict_assertions(solver, ingredient_vars, data['konflikty'])


def print_model(solver, vars):
  """
  Prints ingredients-to-consume-by-meal that satisfy previously given conditions to SMT solver

  Parameters:
  solver (z3solver): SMT solver
  vars (tuple): Tuple of two data structers, first containing ingredient SMT vars, second containg meal ones
  """
  ingredient_vars, meal_vars = vars
  model = solver.model()
  for meal in MEALS:
    print(meal+': ', end='')
    food = []
    for ingredient in used_ingredients.values():
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
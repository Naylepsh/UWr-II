from itertools import permutations
from abc import ABC, abstractmethod


# Abstract base class
class Formula(ABC):

	def __init__(self, symbol):
		self.symbol = symbol

	def __str__(self):
		return self.symbol

	@abstractmethod
	def eval(self, vars):
		pass

	def free_vars(self):
		return set()


class Const(Formula):
	pass


class TrueConst(Const):

	def __init__(self):
		super().__init__('true')

	def eval(self, vars):
		return True


class FalseConst(Const):

	def __init__(self):
		super().__init__('false')

	def eval(self, vars):
		return False


class Var(Formula):

	def eval(self, vars):
		for symbol, value in list(vars):
			if self.symbol == symbol:
				return value
		raise Exception('Symbol not found')

	def free_vars(self):
		return set(self.symbol)


class UnOp(Formula):

	def __init__(self, symbol, child):
		super().__init__(symbol)
		self.child = child

	def __str__(self):
		return f'{self.symbol}({self.child})'

	def free_vars(self):
		return self.child.free_vars()


class Neg(UnOp):

	def __init__(self, child):
		super().__init__('¬', child)

	def eval(self, vars):
		return not self.child.eval(vars)


class BinOp(Formula):

	def __init__(self, symbol, left, right):
		super().__init__(symbol)
		self.left = left
		self.right = right

	def __str__(self):
		return f'({self.left}{self.symbol}{self.right})'

	def free_vars(self):
		return self.left.free_vars() | self.right.free_vars()

class And(BinOp):

	def __init__(self, left, right):
		super().__init__('∧', left, right)

	def eval(self, vars):
		return self.left.eval(vars) and self.right.eval(vars)
		

class Or(BinOp):

	def __init__(self, left, right):
		super().__init__('∨', left, right)

	def eval(self, vars):
		return self.left.eval(vars) or self.right.eval(vars)


class Impl(BinOp):

	def __init__(self, left, right):
		super().__init__('=>', left, right)

	def eval(self, vars):
		if self.left.eval(vars):
			return self.right.eval(vars)
		return True

class Equiv(BinOp):

	def __init__(self, left, right):
		super().__init__('<=>', left, right)

	def eval(self, vars):
		return self.left.eval(vars) == self.right.eval(vars)


def is_tautology(formula):
	vars = formula.free_vars()
	values = [True, False] * len(vars)
	for var_zip in [zip(vars, value) for value in permutations(values, len(vars))]:
		if not formula.eval(list(var_zip)):
			return False
	return True

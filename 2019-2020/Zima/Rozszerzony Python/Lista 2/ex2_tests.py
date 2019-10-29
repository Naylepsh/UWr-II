from ex2 import *

def const_test():
	assert TrueConst().eval([])
	assert not FalseConst().eval([])
	print('"Constant" tests passed.')


def var_test():
	vars = [
	('x', True),
	('y', False)
	]
	assert Var('x').eval(vars)
	assert not Var('y').eval(vars)
	print('"Var" tests passed.')


def and_test():
	vars = [
	('x', True),
	('y', False)
	]
	assert And(TrueConst(), TrueConst()).eval(vars)
	assert not And(TrueConst(), FalseConst()).eval(vars)
	assert not And(FalseConst(), TrueConst()).eval(vars)
	assert not And(FalseConst(), FalseConst()).eval(vars)
	assert And(Var('x'), Var('x')).eval(vars)
	assert not And(Var('x'), Var('y')).eval(vars)
	print('"And" tests passed.')


def or_test():
	vars = [
	('x', True),
	('y', False)
	]
	assert Or(TrueConst(), TrueConst()).eval(vars)
	assert Or(FalseConst(), TrueConst()).eval(vars)
	assert Or(TrueConst(), FalseConst()).eval(vars)
	assert not Or(FalseConst(), FalseConst()).eval(vars)
	assert Or(Var('x'), Var('x')).eval(vars)
	assert Or(Var('x'), Var('y')).eval(vars)
	assert not Or(Var('y'), Var('y')).eval(vars)
	print('"Or" tests passed.')


def impl_test():
	vars = [
	('x', True),
	('y', False)
	]
	assert Impl(TrueConst(), TrueConst()).eval(vars)
	assert not Impl(TrueConst(), FalseConst()).eval(vars)
	assert Impl(FalseConst(), TrueConst()).eval(vars)
	assert Impl(FalseConst(), FalseConst()).eval(vars)
	assert Impl(Var('x'), Var('x')).eval(vars)
	assert not Impl(Var('x'), Var('y')).eval(vars)
	print('"Implication" tests passed.')


def tautology_test():
	assert is_tautology(TrueConst())
	assert not is_tautology(FalseConst())
	assert not is_tautology(Var('x'))
	assert not is_tautology(Or(Var('x'), (Var('y'))))
	assert is_tautology(Or(Var('x'), TrueConst()))
	assert not is_tautology(And(Var('x'), FalseConst()))
	assert is_tautology(Impl(FalseConst(), Var('x')))
	print('"is_tautology(formula)" tests passed.')


def formula_printing():
	print(Impl(Var('x'), And(Var('y'), TrueConst())))
	print(
		Or(
			Impl(
				And(
					Var('x'),
					Var('y')
					),
				Or(
					Var('z'),
					FalseConst()
					)
				),
			Impl(
				Neg(
					TrueConst()
					),
				Var('φ')
				)

			)
		)

const_test()
var_test()
and_test()
or_test()
impl_test()
tautology_test()
formula_printing()
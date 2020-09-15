from forms import *


def fv(f):
    if isVar(f):
        return set(getVar(f))
    elif isNeg(f):
        return fv(getNeg(f))
    elif isConj(f):
        return fv(getConjL(f)) | fv(getConjR(f))
    elif isDisj(f):
        return fv(getDisjL(f)) | fv(getDisjR(f))


def val(f, S):
    if isVar(f):
        return S[getVar(f)]
    elif isNeg(f):
        return not val(getNeg(f), S)
    elif isConj(f):
        return val(getConjL(f), S) and val(getConjR(f), S)
    elif isDisj(f):
        return val(getDisjL(f), S) or val(getDisjR(f), S)


formula = conj(var('x'), neg(var('y')))
print(fv(formula))

S = {'x' : True, 'y' : False}
print(val(formula, S))
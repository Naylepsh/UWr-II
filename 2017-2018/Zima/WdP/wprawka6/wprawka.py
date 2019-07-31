def foo(x):
    return x + 1


def compose(f,g):
    return lambda x: f(g(x))


def repeated(f, n):
    if n < 0: raise Exception('Nigga what are you doing?')
    else:
        def f_0(x):
            return x
        g = f_0
        for i in range(n):
            g = compose(f,g)
        return g

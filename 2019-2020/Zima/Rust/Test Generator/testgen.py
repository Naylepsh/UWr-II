import json
import random
from testfunc import run
from testform import TEST_FILENAME


ALPHABET = 'qwertyuiopasdfghjklzxcvbnm'
INT_MIN = -2147483648
INT_MAX = 2147483647
RANDOM_STR_LEN = 16
SUPPORTED_TYPES = ['int', 'string']


def random_str(length):
    return ''.join([random.choice(ALPHABET) for _ in range(length)])


def random_int(min, max):
    return random.randint(min, max)


def random_float(min, max):
    return random.random() * (max-min) + min


def read_data(filename):
    with open(filename, 'r') as file:
        data = json.load(file)
    return data


def generate_var(type, **params):
    if type == 'string':
        return random_str(params['lower_bound']);
    elif type == 'int':
        if params['lower_bound'] is None:
            params['lower_bound'] = INT_MIN
        if params['upper_bound'] is None:
            params['upper_bound'] = INT_MAX
        return random_int(params['lower_bound'], params['upper_bound'])


def generate_tests(test_details):
    tests = []
    for i in range(test_details['n']):
        args = []
        for var in test_details['types']:
            type, *params = var.split(' ')
            if type == 'array':
                sub_type, n, *params = params
                lower_bound = int(params[0]) if params else None
                upper_bound = int(params[1]) if len(params) > 1 else None
                array_args = []
                for _ in range(int(n)):
                    array_args.append(generate_var(sub_type, lower_bound=lower_bound, upper_bound=upper_bound))
                args.append(array_args)
            elif type in SUPPORTED_TYPES:
                lower_bound = int(params[0]) if params else None
                upper_bound = int(params[1]) if len(params) > 1 else None
                args.append(generate_var(type, lower_bound=lower_bound, upper_bound=upper_bound))
            else:
                raise Exception('REEEE')
        
        tests.append(generate_test(test_details['function name'], i, args, run(args)))
    return '\n'.join(tests)


def generate_test(function_name, i, args, res):
    params = ', '.join(map(lambda x: str(x), args))
    return f'#[test]\nfn test{i}() {{\n\tassert_eq!({function_name}({params}), {res})\n}}\n'


print(generate_tests(read_data(TEST_FILENAME)))
# generate_tests(read_data(TEST_FILENAME))
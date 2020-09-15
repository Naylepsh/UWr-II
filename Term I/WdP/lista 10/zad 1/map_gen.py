from random import randint


def gen_map(size, fname):
    map = open(fname, 'w')
    for i in range(size):
        for j in range(size):
            if randint(0,3) != 0:
                map.write('.')
            else:
                map.write('#')
        map.write('\n')

gen_map(15, 'niebieski.txt')
gen_map(15, 'czerwony.txt')
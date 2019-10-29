import json

TEST_FILENAME = 'test-details.json'


if __name__ == '__main__':
    fname = input('Enter function name: ')
    types = []
    while True:
        x = input('Enter type name (empty to quit): ').lower()
        if not x:
            break
        types.append(x)

    n = int(input('Enter number of tests to be generated: '))

    with open(TEST_FILENAME, 'w') as file:
        file.write(json.dumps({'function name': fname, 'types': types, 'n': n}))

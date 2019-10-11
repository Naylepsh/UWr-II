$tests_file = 'tests.txt'
$accept = 100
$reject = 100

# generate automata and tests
& 'python' '.\automaton_test_generator.py' $tests_file $accept $reject

& 'echo' 'Tests generated'

# load automata from file and run it on tests
& 'cat' $tests_file | & 'python' '..\automaton.py' > 'automaton_output.txt'

# check automata output
& 'python' '.\automaton_test_checker.py' 'automaton_output.txt' $accept $reject

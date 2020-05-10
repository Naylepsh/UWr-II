#!/usr/bin/env python3

import subprocess

AVAILABLE_TESTS = 7

for i in range(AVAILABLE_TESTS):
  cmd = ['dotnet', 'run', str(i)]
  process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
  process.wait()
  for line in process.stdout:
    print(line)
  print()
print('Finished testing')
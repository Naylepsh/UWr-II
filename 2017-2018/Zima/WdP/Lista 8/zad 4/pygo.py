P = []
I = {}

for x in open('program.pygo'):
  i = 1
  L = x.split()
  if len(L) == 0:
    continue
  ### CHANGES HERE
  # get identifier
  if L[0][len(L[0]) - 1] == ':':
    I[L[0][:-1]] = i + 1  # using string without last char ':' as a key
    L = L[1:]
  i += 1
  ### We possibly just got rid of the fist element of L, thus we need to check
  ### whether there's still some command in it
  if len(L) == 0:
    continue
  if L[1] == '=':
    P.append( ('=', L[0], ' '.join(L[2:])) )
  elif L[0] == 'print':
    P.append( ('print', ' '.join(L[1:])) )
  elif L[0] == 'goto':
    ### CHANGES HERE
    # if our identifier is a word e.g. 'start'
    if not L[1].isdigit():
      P.append( ('goto', I[L[1]]) )
    else:
      P.append( ('goto', L[1]) )
  elif L[0] == 'if':
    P.append( ('if', ' '.join(L[1:-2]) , L[-1]))
    
#for instr in P:
#  print (instr)

PC = 0
M = {} # memory

while PC < len(P):
  instr = P[PC]
  typ = instr[0]
  if typ == '=':
    M[instr[1]] = eval(instr[2], M)
    PC += 1
  elif typ == 'print':
    print (eval(instr[1], M))
    PC += 1
  elif typ == 'goto':
      PC = int(instr[1]) -1
  elif typ == 'if':
    warunek = eval(instr[1], M)
    if warunek:
      ### CHANGES HERE
      # if our identifier is a word e.g. 'start'
      if not instr[2].isdigit():
        PC = I[instr[2]] - 1
      else:
        PC = int(instr[2]) - 1
    else:
      PC += 1 

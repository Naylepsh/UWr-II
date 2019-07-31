# array[0] means a.begin
# array[len(array)-1] means a.end


def insert(a, x, pos):
    if len(a) == 0:
        a.append(pos)
        a.append(x)
        a.append(pos)
    elif a[0] is None and a[len(a)-1] is None:
        a[0], a[1] = pos, pos
        a.insert(1,x)
    else:
        end = a[len(a)-1]
        if pos < a[0]:
            # same as: a = [pos] + (a[0] - pos) * [0] + a[1:]
            for i in range(a[0] - pos):
                a.insert(1, 0)
            a[0] = pos
        elif pos > end:
            # same as: a = a[:-1] + (pos - end) * [0] + [pos]
            for i in range(pos - end):
                a.insert(len(a)- 1, 0)
            a[-1] = pos

        a[pos - a[0] + 1] = x   # +1, because array[0] is our beginning index


def get_val(a, pos):
    if a[0] <= pos <= a[-1]:
        return a[pos + 1 - a[0]]
    else:
        return None

liczby = {
  1 : 'jeden',
  2 : 'dwa',
  3 : 'trzy',
  4 : 'cztery',
  5 : 'pięć',
  6 : 'sześć',
  7 : 'siedem',
  8 : 'osiem',
  9 : 'dziewięć',
  10 : 'dziesięć',
  11 : 'jedenaście',
  12 : 'dwanaście',
  13 : 'trzynaście',
  14 : 'czternaście',
  15 : 'piętnaście',
  16 : 'szesnaście',
  17 : 'siedemnaście',
  18 : 'osiemnaście',
  19 : 'dziewiętnaście',
  20 : 'dwadzieścia',
  30 : 'trzydzieści',
  40 : 'czterdzieści',
  50 : 'pięćdziesiąt',
  60 : 'sześćdziesiąt',
  70 : 'siedemdziesiąt',
  80 : 'osiemdziesiąt',
  90 : 'dziewięćdziesiąt',
  100 : 'sto',
  200 : 'dwieście',
  300 : 'trzysta',
  400 : 'czterysta',
  500 : 'pięćset',
  600 : 'sześćset',
  700 : 'siedemset',
  800 : 'osiemset',
  900 : 'dziewięćset'
}

def into_text(n):
    n = str(n)
    text = ""
    skip = False
    for i in range(0, len(n)):
        digit = int(n[i])
        if skip:
            skip = False
        # if 10*n[i] + n[i+1] is between 10 and 19
        # then merge it into one number
        # and skip n[i+1]
        elif digit == 1 and i == len(n) - 2:
            text += liczby[digit * 10 + int(n[i+1])] + " "
            skip = True
        # if such number can be found in liczby
        elif digit * 10 ** (len(n) - i) in liczby:
            text += liczby[digit * 10 ** (len(n) - 1 - i)] + " "
        else:
            exit("Unexpected error.")
    return text


def text_with_nums(word_list):
    string = ""
    for element in word_list:
        element = str(element)
        # the only change we have to make
        # is when an element is a number
        if element.isnumeric():
            string += into_text(element)
        else:
            string += element + " "
    return string


print(text_with_nums(['Ala', 'ma', 13, 'kotów', 'i', '57', 'kanarków']))

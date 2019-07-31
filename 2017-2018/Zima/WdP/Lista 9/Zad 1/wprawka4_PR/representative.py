def representative(num_list):
    """Returns smallest k-digit number in a list
    where k is a number of the highest number in a non-empty list"""

    # if an element from a list is higher than current representative
    # and it has more digits than make it a new representative
    # or if an element is lower than representative but
    # it has the same number of digits - then to the same
    repr = num_list[0]
    for element in num_list:
        if element > repr and len(str(element)) > len(str(repr)):
            repr = element
        elif element < repr and len(str(element)) == len(str(repr)):
            repr = element
    return repr


print(representative([1,2,3,4]))
print(representative([1,2,3,44,55]))
print(representative([55,44,3,2,1]))
print(representative([1,2,3,4,5,6,7,8,9,10]))

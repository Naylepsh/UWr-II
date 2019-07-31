from math import sqrt


class Location:
    """Location class contains x, y, z coordinates
       and methods to manipulate them"""
    def __init__(self, x, y=0, z=0):
        self.x = x
        self.y = y
        self.z = z

    def __str__(self):
        return str(self.x) + " " + str(self.y) + " " + str(self.z)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y and self.z == other.z

    def is_within_bounds(self, min_x, min_y, max_x, max_y):
        """Checks whether location is contained within given bounds"""
        return (min_x <= self.x < max_x and
                min_y <= self.y < max_y)

    def distance_from(self, location):
        """Counts the distance to given location. Assumes they're on the same z-level"""
        x_diff = abs(self.x - location.x)
        y_diff = abs(self.y - location.y)
        return sqrt(x_diff * x_diff + y_diff * y_diff)

    def get_xyz(self):
        """Returns tuple of xyz-coordinates"""
        return self.x, self.y, self.z

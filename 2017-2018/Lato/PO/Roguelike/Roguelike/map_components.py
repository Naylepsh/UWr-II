from random import randint
from abc import ABC, abstractmethod
from settings import *
from location import *


class Tile(ABC):
    def __init__(self, location, interactive=False, accessible=False, allows_spawn=False, visible=True):
        self.location = location
        self.interactive = interactive
        self.accessible = accessible
        self.allows_spawn = allows_spawn
        self.visible = True if ALL_TILES_VISIBLE else visible
        self.drawn = False

    @abstractmethod
    def draw(self, graphics):
        pass

    @abstractmethod
    def erase(self, graphics):
        pass


class Downstairs(Tile):
    def __init__(self, location):
        Tile.__init__(self, location, interactive=True, accessible=True, allows_spawn=False, visible=False)
        self.bg = None
        self.fg = None

    def draw(self, graphics):
        if not self.drawn and self.visible:
            self.fg = graphics.create_text((int((self.location.x + 0.5) * SQUARE_SIZE),
                                            int((self.location.y + 0.5) * SQUARE_SIZE)),
                                           text="▾", fill="green", font=DRAWING_FONT)
            self.drawn = True

    def erase(self, graphics):
        if self.drawn:
            if self.bg is not None:
                graphics.delete(self.bg)
                self.bg = None
            if self.fg is not None:
                graphics.delete(self.fg)
                self.fg = None
            self.drawn = False

    @staticmethod
    def interact_with(entity):
        entity.move_down()


class Upstairs(Tile):
    def __init__(self, location):
        Tile.__init__(self, location, interactive=True, accessible=True, allows_spawn=False, visible=False)
        self.bg = None
        self.fg = None

    def draw(self, graphics):
        if not self.drawn and self.visible:
            self.fg = graphics.create_text((int((self.location.x + 0.5) * SQUARE_SIZE),
                                            int((self.location.y + 0.5) * SQUARE_SIZE)),
                                           text="▴", fill="green", font=DRAWING_FONT)
            self.drawn = True

    def erase(self, graphics):
        if self.drawn:
            if self.bg is not None:
                graphics.delete(self.bg)
                self.bg = None
            if self.fg is not None:
                graphics.delete(self.fg)
                self.fg = None
            self.drawn = False

    @staticmethod
    def interact_with(entity):
        entity.move_up()


class Wall(Tile):
    def __init__(self, location):
        Tile.__init__(self, location, interactive=False, accessible=False, allows_spawn=False, visible=False)
        self.bg = None
        self.fg = None

    def draw(self, graphics):
        if not self.drawn and self.visible:
            self.fg = graphics.create_text((int((self.location.x + 0.5) * SQUARE_SIZE),
                                            int((self.location.y + 0.5) * SQUARE_SIZE)),
                                           text="■", fill="grey", font=DRAWING_FONT)
            self.drawn = True

    def erase(self, graphics):
        if self.drawn:
            if self.bg is not None:
                graphics.delete(self.bg)
                self.bg = None
            if self.fg is not None:
                graphics.delete(self.fg)
                self.fg = None
            self.drawn = False


class Floor(Tile):
    def __init__(self, location):
        Tile.__init__(self, location, interactive=False, accessible=True, allows_spawn=True, visible=False)
        self.bg = None
        self.fg = None

    def draw(self, graphics):
        if not self.drawn and self.visible:
            self.fg = graphics.create_text((int((self.location.x + 0.5) * SQUARE_SIZE),
                                            int((self.location.y + 0.5) * SQUARE_SIZE)),
                                           text=".", fill="grey", font=DRAWING_FONT)
            self.drawn = True

    def erase(self, graphics):
        if self.drawn:
            if self.bg is not None:
                graphics.delete(self.bg)
                self.bg = None
            if self.fg is not None:
                graphics.delete(self.fg)
                self.fg = None
            self.drawn = False


"""class LootBox(Tile):
    def __init__(self, location, size):
        Tile.__init__(self, location, size)
        self.set_interactivity(True)
        self.set_accessibility(True)
        self.set_spawn_allowance(False)
        self.content = []  # List of items in loot-box
        self.generate_content(randint(1, 3))
        self.opened = False
        self.status_changed = False

    def draw(self, graphics):
        if self.status_changed:
            self.undraw(graphics)
            self.status_changed = False
            self.drawn = False
        if not self.drawn:
            Tile.draw(self, graphics)
            if self.opened:
                self.fg = graphics.create_text((int((self.location.x + 0.5) * self.size),
                                                int((self.location.y + 0.5) * self.size)),
                                               text="◻", fill="brown")
            else:
                self.fg = graphics.create_text((int((self.location.x + 0.5) * self.size),
                                                int((self.location.y + 0.5) * self.size)),
                                               text="■", fill="brown")
            self.drawn = True

    def interact_with(self, entity):
        # empty the content of a loot-box and make it non-open-able
        for item in self.content:
            entity.loot_item(item)
        self.content = None
        self.interactive = False
        self.opened = True
        self.status_changed = True

    def generate_content(self, how_many=0):
        # TODO: make it so all items (including their stats) are taken from a file
        for i in range(how_many):
            x = randint(0, 3)
            if x == 0:
                item = Weapon(5, "weapon")
            elif x == 1:
                item = Armor(5, "armor")
            else:
                item = Potion("potion")
            self.content.append(item)"""


class Fountain(Tile):
    def __init__(self, location):
        Tile.__init__(self, location, interactive=True, accessible=True, allows_spawn=False, visible=False)
        self.health_restored = 50
        self.active = True
        self.bg = None
        self.fg = None

    def interact_with(self, entity):
        if self.active:
            entity.increase_health(self.health_restored)
            self.active = False
            self.drawn = False

    def draw(self, graphics):
        if not self.drawn and self.visible:
            color = "aqua" if self.active else "grey"
            self.fg = graphics.create_text((int((self.location.x + 0.5) * SQUARE_SIZE),
                                            int((self.location.y + 0.5) * SQUARE_SIZE)),
                                           text="◉", fill=color)
            self.drawn = True

    def erase(self, graphics):
        if self.drawn:
            if self.bg is not None:
                graphics.delete(self.bg)
                self.bg = None
            if self.fg is not None:
                graphics.delete(self.fg)
                self.fg = None
            self.drawn = False


class Level:
    """class Level is a 2D array of tiles and some methods
    initialize their values in 'unique' ways and draw them"""
    def __init__(self, tiles_x, tiles_y, starting_location):
        self.tiles = []
        self.tiles_x = tiles_x
        self.tiles_y = tiles_y
        self.init_level_layout(starting_location)

    def init_level_layout(self, starting_location):
        """Based on Drunkard Walk algorithm:
        find random direction(N,S,W,E)
        move by a random distance in that direction (and add each traveled tile into a pile)
        repeat n times
        last tile is Downstairs-tile
        for all possible tiles, if tile hasn't been traveled to then turn it into a wall"""

        # initialize traveled_locations
        traveled_locations = [[False for _ in range(self.tiles_y)] for _ in range(self.tiles_x)]
        x, y, _ = starting_location.get_xyz()  # removing z-coordinate
        traveled_locations[x][y] = True

        # creating basic layout
        for i in range(LEVEL_CREATION_ITER):
            dist = self.rand_dist()
            for k in range(dist[2]):
                if 1 <= x + dist[0] < self.tiles_x - 1 and 1 <= y + dist[1] < self.tiles_y - 1:
                    traveled_locations[x][y] = True
                    if dist[0] != 0:
                        x += dist[0]
                    if dist[1] != 0:
                        y += dist[1]

        # placing floor and walls
        for i in range(self.tiles_x):
            col = []
            for j in range(self.tiles_y):
                if traveled_locations[i][j]:
                    col.append(Floor(Location(i, j)))
                else:
                    col.append(Wall(Location(i, j)))
            self.tiles.append(col)

    def init_upstairs(self, location):
        self.tiles[location.x][location.y] = Upstairs(location)

    def init_downstairs(self, location):
        self.tiles[location.x][location.y] = Downstairs(location)

    def init_fountain(self, location):
        self.tiles[location.x][location.y] = Fountain(location)

    def nearest_suitable_tile(self, location):
        while not self.tiles[location.x][location.y].allows_spawn:
            new_x = location.x + randint(-1, 1)
            new_y = location.y + randint(-1, 1)
            new_location = Location(new_x, new_y, location.z)
            if new_location.is_within_bounds(1, 1, self.tiles_x - 2, self.tiles_y - 2):
                location = new_location
        return location

    def make_tiles_visible(self, locations):
        for location in locations:
            if location.is_within_bounds(0, 0, TILES_X, TILES_Y):
                self.tiles[location.x][location.y].visible = True

    def number_of_tiles_allowing_spawn(self):
        n = 0
        for row in self.tiles:
            for tile in row:
                if tile.allows_spawn:
                    n += 1
        return n

    def is_tile_available(self, location):
        return (location.is_within_bounds(0, 0, self.tiles_x, self.tiles_y) and
                self.get_tile(Location(location.x, location.y)).accessible)

    @staticmethod
    def rand_dist():
        """Returns random tuple of xy coordinates such that x != 0 and y != 0 and a random distance"""
        dir_x = randint(-1, 1)
        dir_y = randint(-1, 1) if dir_x == 0 else 0
        while dir_x == dir_y == 0:
            dir_x = randint(-1, 1)
            dir_y = randint(-1, 1) if dir_x == 0 else 0
        return dir_x, dir_y, randint(1, 5)

    def draw(self, graphics):
        for i in range(len(self.tiles)):
            for j in range(len(self.tiles[0])):
                self.tiles[i][j].draw(graphics)

    def erase(self, graphics):
        for i in range(len(self.tiles)):
            for j in range(len(self.tiles[0])):
                self.tiles[i][j].erase(graphics)

    def get_tile(self, location):
        return self.tiles[location.x][location.y]

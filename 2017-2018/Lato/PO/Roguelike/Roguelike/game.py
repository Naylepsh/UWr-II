from map_components import *
from entities import *
from tkinter import *
from settings import *
from random import randint


class Game:
    def __init__(self):
        # init gui
        self.frame = Frame()
        self.frame.pack()
        self.canvas = Canvas(self.frame, width=WINDOW_WIDTH, height=WINDOW_HEIGHT, bg="black")
        self.canvas.pack()
        self.is_game_over = False
        self.should_quit = False
        self.hud = HUD(0, 0, 0)

        # init entities
        self.creatures = []
        self.player = Player(Location(randint(1, TILES_X - 2), randint(1, TILES_Y - 2)))
        self.players_turn = True
        self.hud.update(self.player)

        # initialize the first level
        self.levels = []
        self.levels_discovered = 0
        self.init_level(TILES_X, TILES_Y, SQUARE_SIZE, upstairs=False, fountains=FOUNTAINS_PER_LEVEL)
        self.make_adjacent_tiles_visible(self.player.location)

        # bind keys
        self.canvas.bind('w', self.move_player_north)
        self.canvas.bind('s', self.move_player_south)
        self.canvas.bind('a', self.move_player_west)
        self.canvas.bind('d', self.move_player_east)
        self.canvas.bind('e', self.interact)
        self.canvas.bind('x', self.attack)
        self.canvas.bind('z', self.sleep)
        self.canvas.bind('k', self.kill_player)
        self.canvas.bind('<Escape>', self.quit)
        self.canvas.focus_set()

    def init_level(self, tiles_x, tiles_y, downstairs=True, upstairs=True, fountains=0):
        """Initializes level layout with given parameters"""
        level = Level(tiles_x, tiles_y, self.player.location)
        if upstairs:
            level.init_upstairs(self.player.location)
        if downstairs:
            location = level.nearest_suitable_tile(Location(randint(1, tiles_x - 2), randint(1, tiles_y - 2)))
            level.init_downstairs(location)
        if fountains > 0:
            for i in range(fountains):
                location = level.nearest_suitable_tile(Location(randint(1, tiles_x - 2), randint(1, tiles_y - 2)))
                level.init_fountain(location)
        self.levels.append(level)

        self.creatures.append([])
        number_of_creatures = min(self.levels_discovered + 3, level.number_of_tiles_allowing_spawn() // 2, 1)
        self.init_creatures(number_of_creatures + 3, self.levels_discovered)

    def init_creatures(self, how_many, z_level):
        """Initializes a specified number of creatures (currently only skeletons) on a given z-level"""
        creatures = []
        for i in range(how_many):
            location = Location(randint(1, self.levels[z_level].tiles_x - 2),
                                randint(1, self.levels[z_level].tiles_y - 2),
                                z_level)
            location = self.levels[z_level].nearest_suitable_tile(location)
            creatures.append(Skeleton(location))
        self.creatures[z_level] += creatures

    def run(self):
        while not self.is_game_over:
            self.draw()
            self.update_models()
        while not self.should_quit:
            self.draw_game_over_screen()

    def draw(self):
        """Draws level layout, player and all entities at player's level and also updates HUD"""
        self.levels[self.player.location.z].draw(self.canvas)
        self.player.draw(self.canvas)
        for entity in self.creatures[self.player.location.z]:
            if self.levels[entity.location.z].get_tile(entity.location).visible:
                entity.draw(self.canvas)
            else:
                entity.erase(self.canvas)
        self.hud.update(self.player)
        self.hud.draw(self.canvas)
        self.canvas.update()

    def erase(self):
        self.levels[self.player.location.z].erase(self.canvas)
        self.canvas.delete(ALL)

    def update_models(self):
        if not self.players_turn:
            for entity in self.creatures[self.player.location.z]:
                if entity.is_alive:
                    self.move_entity(entity)
            self.players_turn = True
        if self.player.health <= 0:
            self.is_game_over = True

    def entity_move_effects(self, entity, prev_location, current_location):
        self.levels[entity.location.z].get_tile(prev_location).accessible = True
        self.levels[entity.location.z].get_tile(current_location).accessible = False

    def player_move_effects(self, prev_location, current_location):
        self.entity_move_effects(self.player, prev_location, current_location)
        self.player.decrease_health(HEALTH_DECREASE_PER_TURN)
        self.make_adjacent_tiles_visible(self.player.location, self.player.visibility_range)
        self.players_turn = False

    def move_player_north(self, _):
        location = Location(self.player.location.x, self.player.location.y - 1)
        if self.levels[self.player.location.z].is_tile_available(location):
            x1, y1, z1 = self.player.location.get_xyz()
            self.player.move_north()
            x2, y2, z2 = self.player.location.get_xyz()
            self.player_move_effects(Location(x1, y1, z1), Location(x2, y2, z2))

    def move_player_south(self, _):
        location = Location(self.player.location.x, self.player.location.y + 1)
        if self.levels[self.player.location.z].is_tile_available(location):
            x1, y1, z1 = self.player.location.get_xyz()
            self.player.move_south()
            x2, y2, z2 = self.player.location.get_xyz()
            self.player_move_effects(Location(x1, y1, z1), Location(x2, y2, z2))

    def move_player_west(self, _):
        location = Location(self.player.location.x - 1, self.player.location.y)
        if self.levels[self.player.location.z].is_tile_available(location):
            x1, y1, z1 = self.player.location.get_xyz()
            self.player.move_west()
            x2, y2, z2 = self.player.location.get_xyz()
            self.player_move_effects(Location(x1, y1, z1), Location(x2, y2, z2))

    def move_player_east(self, _):
        location = Location(self.player.location.x + 1, self.player.location.y)
        if self.levels[self.player.location.z].is_tile_available(location):
            x1, y1, z1 = self.player.location.get_xyz()
            self.player.move_east()
            x2, y2, z2 = self.player.location.get_xyz()
            self.player_move_effects(Location(x1, y1, z1), Location(x2, y2, z2))

    def make_adjacent_tiles_visible(self, location, visibility_range=VISIBILITY_RANGE):
        """Makes all tiles within visibility range from given location visible"""
        locations = []
        x, y, z = location.get_xyz()
        for x_off in range(-visibility_range, visibility_range + 1):
            for y_off in range(-visibility_range, visibility_range + 1):
                locations.append(Location(x + x_off, y + y_off))
        self.levels[z].make_tiles_visible(locations)

    def interact(self, _):
        """Makes player interact with an object in player's location.
        If any changes to player's z-coordinate were made, then
        the previous level is erased.
        If player's z-coordinate is greater than the number of levels discovered,
        then generate a new level."""
        x, y, z = self.player.location.get_xyz()
        if self.levels[z].tiles[x][y].interactive:
            previous_lvl = self.player.location.z
            self.levels[z].tiles[x][y].interact_with(self.player)
            current_lvl = self.player.location.z
            if previous_lvl != current_lvl:  # if player changed levels
                self.player.location.z = previous_lvl
                self.erase()
                self.player.location.z = current_lvl
            if self.player.location.z == self.levels_discovered + 1:  # if player has found a new level
                self.levels_discovered += 1
                self.init_level(TILES_X, TILES_Y, fountains=FOUNTAINS_PER_LEVEL)
                self.make_adjacent_tiles_visible(self.player.location)

    def attack(self, _):
        """Naive implementation of an attack.
        Searches in all directions (C, N, E, S, W) for an entity
        with the lowest amount of hp and attacks it.
        If no such entity is found, then nothing happens (player still loses his turn)"""
        x, y, z = self.player.location.get_xyz()
        directions = [[x, y, z], [x, y-1, z], [x-1, y, z], [x+1, y, z], [x, y+1, z]]
        creature_found = False
        dir_index = 0
        creature_to_attack = None
        while dir_index < len(directions):
            direction = directions[dir_index]
            for creature in self.creatures[direction[2]]:
                rules = [creature.location.x == direction[0],
                         creature.location.y == direction[1],
                         creature.is_alive,
                         not creature_found]
                if all(rules):
                    if creature_to_attack is None:
                        creature_to_attack = creature
                    elif creature_to_attack.health > creature.health:
                        creature_to_attack = creature
            dir_index += 1

        if creature_to_attack is not None:
            self.player.attack(creature_to_attack)
            if not creature_to_attack.is_alive:
                creature_to_attack.give_experience_to(self.player)
                self.levels[creature_to_attack.location.z].get_tile(creature_to_attack.location).accessible = True

        self.players_turn = False

    def sleep(self, _):
        self.players_turn = False

    def kill_player(self, _):
        self.player.health = 0
        self.players_turn = False

    def move_entity(self, entity):
        dist = entity.location.distance_from(self.player.location)
        if dist == 1:
            entity.attack(self.player)
        elif dist <= entity.visibility_range:
            self.move_entity_towards_target(entity, self.player, entity.visibility_range)
        else:
            self.move_entity_randomly(entity)

    def move_entity_towards_target(self, entity, target, max_steps):
        self.current_record = max_steps + 1
        self.all_steps = []

        def pathfinder(x, y, target, steps, steps_taken):
            if steps == max_steps or isinstance(self.levels[target.location.z].tiles[x][y], Wall):
                return
            elif target.location.x == x and target.location.y == y:
                if steps < self.current_record:
                    self.current_record = steps
                    self.all_steps = steps_taken
                    return
            else:
                for loc in [[x, y-1], [x-1, y], [x+1, y], [x, y+1]]:
                    pathfinder(loc[0], loc[1], target, steps+1, steps_taken+[loc])

        pathfinder(entity.location.x, entity.location.y, target, 0, [])

        if self.all_steps != []:
            step = self.all_steps[0]
            x1, y1, _ = entity.location.get_xyz()
            entity.move_to_location(Location(step[0], step[1], entity.location.z))
            x2, y2, _ = entity.location.get_xyz()
            self.entity_move_effects(entity, Location(x1, y1, entity.location.z), Location(x2, y2, entity.location.z))

    def move_entity_randomly(self, entity):
        # randomly generate direction
        add_to_x = randint(-1, 1)
        add_to_y = (randint(-1, 1) if add_to_x == 0 else 0)
        # move by size of one square in one direction
        new_location = Location(entity.location.x + add_to_x,
                                entity.location.y + add_to_y,
                                entity.location.z)
        # if it's possible to move into that location, then do so
        if (new_location.is_within_bounds(0, 0, self.levels[self.player.location.z].tiles_x,
                                          self.levels[self.player.location.z].tiles_y) and
                self.levels[self.player.location.z].get_tile(new_location).accessible):
            x1, y1, _ = entity.location.get_xyz()
            entity.move_to_location(new_location)
            x2, y2, _ = entity.location.get_xyz()
            self.entity_move_effects(entity, Location(x1, y1, entity.location.z), Location(x2, y2, entity.location.z))

    def quit(self, _):
        self.is_game_over = True
        self.should_quit = True

    def draw_game_over_screen(self):
        self.erase()
        self.canvas.create_text(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2,
                                text="YOU LOST!\n YOUR SCORE: "+str(self.player.exp),
                                font=("Times New Roman", 40), fill="white", anchor="c")
        self.canvas.create_text(WINDOW_HEIGHT // 2, WINDOW_HEIGHT // 1.5,
                                text="Press any [Esc] to quit", fill="white", anchor="w")
        self.canvas.update()


class HUD:
    def __init__(self, health, experience, level):
        self.health = health
        self.health_bar_loc = Location(50, WINDOW_HEIGHT - HUD_HEIGHT // 2)
        self.health_repr = None
        self.exp = experience
        self.exp_bar_loc = Location(WINDOW_WIDTH - 300, WINDOW_HEIGHT - HUD_HEIGHT // 2)
        self.exp_repr = None
        self.level = level
        self.level_bar_loc = Location(WINDOW_WIDTH // 2, WINDOW_HEIGHT - HUD_HEIGHT // 2)
        self.level_repr = None

    def update(self, player):
        self.health = player.health
        self.exp = player.exp
        self.level = player.level

    def draw(self, graphics):
        graphics.delete(self.health_repr)
        hp = "{:.2f}".format(self.health)
        self.health_repr = graphics.create_text(self.health_bar_loc.x, self.health_bar_loc.y,
                                                text=("Health: "+hp), fill="white", anchor="w",
                                                font=("Times New Roman", 15))
        graphics.delete(self.exp_repr)
        self.exp_repr = graphics.create_text(self.exp_bar_loc.x, self.exp_bar_loc.y,
                                             text=("Exp: "+str(self.exp)), fill="white", anchor="w",
                                             font=("Times New Roman", 15))
        graphics.delete(self.level_repr)
        self.level_repr = graphics.create_text(self.level_bar_loc.x, self.level_bar_loc.y,
                                               text=("Level: "+str(self.level)), fill="white", anchor="w",
                                               font=("Times New Roman", 15))


if __name__ == "__main__":
    game = Game()
    game.run()

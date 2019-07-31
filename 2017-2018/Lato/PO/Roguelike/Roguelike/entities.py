from abc import ABC, abstractmethod
from settings import *
from random import randint


class Entity(ABC):
    def __init__(self, location, max_health=100, exp=0, attack_min=0, attack_max=0, vis_range=0):
        self.location = location
        self.health = max_health
        self.max_health = max_health
        self.exp = exp
        self.attack_min = attack_min
        self.attack_max = attack_max
        self.is_alive = True
        self.visibility_range = vis_range

    def move_north(self, dist=1):
        self.location.y -= dist

    def move_south(self, dist=1):
        self.location.y += dist

    def move_west(self, dist=1):
        self.location.x -= dist

    def move_east(self, dist=1):
        self.location.x += dist

    def move_up(self, dist=1):
        self.location.z -= dist

    def move_down(self, dist=1):
        self.location.z += dist

    def move_to_location(self, location):
        self.location.x = location.x
        self.location.y = location.y
        self.location.z = location.z

    def increase_health(self, amount):
        self.health = min(self.max_health, self.health + amount)

    def decrease_health(self, amount):
        self.health = self.health - amount
        if self.health <= 0:
            self.is_alive = False

    def attack(self, entity):
        entity.decrease_health(randint(self.attack_min, self.attack_max))

    def set_attack_min(self, value):
        self.attack_min = max(0, value)

    def set_attack_max(self, value):
        self.attack_max = max(0, value)

    def gain_experience(self, amount):
        self.exp += amount

    def give_experience_to(self, entity):
        entity.gain_experience(self.exp)
        self.exp = 0

    @abstractmethod
    def draw(self, graphics):
        pass

    @abstractmethod
    def erase(self, graphics):
        pass


class Player(Entity):
    def __init__(self, location):
        Entity.__init__(self, location, vis_range=VISIBILITY_RANGE)
        self.set_attack_min(20)
        self.set_attack_max(40)
        self.level = 0
        self.fg = None

    def gain_experience(self, amount):
        Entity.gain_experience(self, amount)
        prev_level = self.level
        self.level = self.exp // EXP_PER_LEVEL
        if self.level > prev_level:
            for i in range(self.level - prev_level):
                self.attack_max += 10
                self.attack_min += 10
            self.health = self.max_health

    def draw(self, graphics):
        graphics.delete(self.fg)
        self.fg = graphics.create_text((int((self.location.x + 0.5) * SQUARE_SIZE),
                                        int((self.location.y + 0.5) * SQUARE_SIZE)),
                                       text="P", fill="white", font=DRAWING_FONT)

    def erase(self, graphics):
        graphics.delete(self.fg)


class Skeleton(Entity):
    def __init__(self, location):
        Entity.__init__(self, location, max_health=50, exp=35, vis_range=3)
        self.set_attack_min(5)
        self.set_attack_max(15)
        self.fg = None

    def draw(self, graphics):
        graphics.delete(self.fg)
        color = "white" if self.is_alive else "grey"
        self.fg = graphics.create_text((int((self.location.x + 0.5) * SQUARE_SIZE),
                                        int((self.location.y + 0.5) * SQUARE_SIZE)),
                                       text="s", fill=color, font=DRAWING_FONT)

    def erase(self, graphics):
        graphics.delete(self.fg)

    def attack(self, entity):
        chance = randint(0, 4)
        if chance == 0:
            self.steal_health(entity, 20)
        else:
            Entity.attack(self, entity)

    def steal_health(self, entity, amount):
        corrected_amount = min(entity.health, amount)
        self.increase_health(corrected_amount)
        entity.decrease_health(corrected_amount)

    """def interact_with(self, entity):
        # should give player some items if it's not alive
        if not self.is_alive:
            pass"""

class Item:
    def __init__(self, defense, attack, name):
        self.defense = defense
        self.attack = attack
        self.name = name
        self.fg = None

    def __str__(self):
        return self.name + ": Defense = " + str(self.defense) + ", Attack = " + str(self.attack)

    def draw(self, graphics, x, y, color):
        self.fg = graphics.create_text(x, y, text=self.name, fill=color, anchor="w")

    def erase(self, graphics):
        graphics.delete(self.fg)


class Weapon(Item):
    def __init__(self, attack, name):
        Item.__init__(self, 0, attack, name)

    def generate_name(self):
        pass

    def randomize(self):
        pass


class Armor(Item):
    def __init__(self, defense, name):
        Item.__init__(self, defense, 0, name)

    def generate_name(self):
        pass

    def randomize(self):
        pass


class Potion(Item):
    def __init__(self, name):
        Item.__init__(self, 0, 0, name)

    def use(self):
        pass
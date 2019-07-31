class Person
  attr_reader :nick
  attr_reader :groups
  def initialize(nick, phone_number, groups)
    @nick = nick
    @phone_number = phone_number
    @groups = []
    groups.each {|group| @groups.push(group.downcase)}
  end

  def belongs_to?(group)
    @groups.each do | persons_group |
      if group.downcase == persons_group
        return true
      end
    end
    false
  end

  def to_s
    puts(@nick, @phone_number, @groups)
  end
end

class Notebook
  def initialize
    @people = []
  end

  def contains?(person)
    @people.each do |someone|
      return true if person.nick == someone.nick
    end
    false
  end

  def add_person(person)
    #@people.push(person) unless self.contains?(person)
    if self.contains?(person)
      puts("Person already registered")
    else
      @people.push(person)
    end
  end

  def search_by_nick(nick)
    @people.each {|person| return person if person.nick == nick}
    nil
  end

  def find_all_groups
    groups = []
    @people.each do |person|
      person.groups.each do | group |
        groups.push(group) unless groups.include? group
      end
    end
    groups
  end

  def find_all_people_of_group(group)
    people = []
    @people.each do |person|
      people.push(person) if person.belongs_to?(group.downcase)
    end
    people
  end

end

$menu_msg = """Choose an option:
1. Add person to notebook
2. Find person by name
3. Find all groups
4. Find all people belonging in the selected group
0. Quit"""

def main
  notebook = Notebook.new

  # fill with trash
  person1 = Person.new('John Doe', '123456789', ['Family', 'Favourite'])
  person2 = Person.new('Jane Doe', '213456798', ['Family', 'Sister'])
  #person3 = Person.new('Asqf', '1231312', ['Test-group1', 'test-group2', 'test-group3'])
  notebook.add_person(person1)
  notebook.add_person(person2)
  #notebook.add_person(person3)

  # user interface
  keep_going = true
  while keep_going
    puts $menu_msg

    option = gets.chomp

    case option
    when '1'
      print "Enter a name: "
      name = gets.chomp
      print "Enter a phone number: "
      number = gets.chomp
      print "Enter groups(sep by ' '): "
      groups = gets.chomp.split
      person = Person.new(name, number, groups)
      notebook.add_person(person)
    when '2'
      print "Enter a name: "
      name = gets.chomp
      person = notebook.search_by_nick(name)
      if person
        puts person
      else
        puts "Person not found."
      end
    when '3'
      puts notebook.find_all_groups
    when '4'
      puts "Enter a group"
      group = gets.chomp
      puts notebook.find_all_people_of_group(group)
    when '0'
      keep_going = false
    else
      puts "Oi, that's not a number between 0-5."
    end
  end
end

main

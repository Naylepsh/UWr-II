require "dbm"

class Contact
  def initialize(name, phone_nums=[], mails=[], nicks=[])
    @name = name
    @phone_nums = []
    @mails = []
    @nicks = []
    add_phone_nums(phone_nums)
    add_mails(mails)
    add_nicks(nicks)
  end

  def add_phone_nums(phone_nums)
    phone_nums.each {|num| @phone_nums.push(num) if phone_validator(num)}
  end

  def add_mails(mails)
    mails.each {|mail| @mails.push(mail) if mail_validator(mail)}
  end

  def add_nicks(nicks)
    # nicks = [[site0, nick0], [site1, nick1], ...]
    nicks.each {|pair| @nicks.push(pair) if site_nick_validator(pair)}
  end

  def phone_validator(phone)
    phone == "" || /^[0-9]{9}$/.match?(phone)
  end

  def mail_validator(mail)
    mail == "" || /\A([\w+\-].?)+@[a-z\d\-]+(\.[a-z]+)*\.[a-z]+\z/i.match?(mail)
  end

  def site_nick_validator(pair)
    pair.kind_of?(Array) && pair.length == 2
  end

  def to_s
    s = @name + "\n"
    s += "\tphone numbers: " + @phone_nums.to_s + "\n"
    s += "\tmails: " + @mails.to_s + "\n"
    s += "\tnicks: " + "\n"
    @nicks.each do |pair|
      s += "\t\t" + pair.to_s + "\n"
    end
    s
  end
end

class Notebook
  def initialize
    @contacts = []
  end

  def add_contact(contact)
    @contacts.push(contact)
  end

  def save(filename)
    i = 0
    DBM.open(filename) do |db|
      @contacts.each {|contact| db[i] = contact; i+=1}
    end
  end

  def load(filename)
    @contacts = []
    DBM.open(filename) do |db|
      db.each {|_, v|  @contacts.push(v)}
    end
  end

  def to_s
    @contacts.each {|contact| puts contact}
  end
end


filename = "contacts.dbm"
contact0 = Contact.new("john doe", ["123456789", "123459876", "0000"],
                ["test@top.kek"],
                [["test_site.com", "testest"],
                ["some_site.org", "Billy Herrington"]])
#print contact0
notebook0 = Notebook.new
notebook0.add_contact(contact0)
notebook0.save(filename)
notebook1 = Notebook.new
notebook1.load(filename)
puts notebook1
using System.Linq;
using System.Collections.Generic;
using System;

namespace Ex_5
{
  class Program
  {
    static void Main(string[] args)
    {
      List<Person> people = Person.GetPeopleFromFile(@".\people.txt");
      List<BankAccount> bankAccounts = BankAccount.GetPeopleFromFile(@".\bank-accounts.txt");
      var peopleAccounts =
        from person in people
        join bankAccount in bankAccounts on person.Pesel equals bankAccount.Pesel
        select new
        {
          name = person.Name,
          surname = person.Surname,
          pesel = person.Pesel,
          bankAccountNumber = bankAccount.AccountNumber
        };
      foreach (var personAccount in peopleAccounts)
      {
        Console.WriteLine(personAccount);
      }
    }
  }

  public class Person
  {
    public string Name { get; set; }
    public string Surname { get; set; }
    public string Pesel { get; set; }

    public Person(string name, string surname, string pesel)
    {
      Name = name;
      Surname = surname;
      Pesel = pesel;
    }

    public static List<Person> GetPeopleFromFile(string path)
    {
      string[] peopleData = System.IO.File.ReadAllLines(path);
      List<Person> people = new List<Person>();
      foreach (string line in peopleData)
      {
        string[] personData = line.Split(' ');
        people.Add(new Person(personData[0], personData[1], personData[2]));
      }
      return people;
    }
  }

  public class BankAccount
  {
    public string Pesel { get; set; }
    public string AccountNumber { get; set; }

    public BankAccount(string pesel, string accountNumber)
    {
      Pesel = pesel;
      AccountNumber = accountNumber;
    }

    public static List<BankAccount> GetPeopleFromFile(string path)
    {
      string[] bankAccountsData = System.IO.File.ReadAllLines(path);
      List<BankAccount> bankAccounts = new List<BankAccount>();
      foreach (string line in bankAccountsData)
      {
        string[] bankAccountData = line.Split(' ');
        bankAccounts.Add(new BankAccount(bankAccountData[0], bankAccountData[1]));
      }
      return bankAccounts;
    }
  }
}

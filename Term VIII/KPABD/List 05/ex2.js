function randomDate() {
  const start = new Date(1970, 0, 1);
  const end = new Date();
  return new Date(
    start.getTime() + Math.random() * (end.getTime() - start.getTime())
  );
}

const book1 = db.books.insertOne({
  ISBN: "83-246-0279-8",
  title: "Microsoft Access. Podręcznik administratora",
  author: "Helen Feddema",
  year_published: 2006,
  price: 69,
  copies: [
    {
      signature: "S0001",
    },
  ],
});

const book2 = db.books.insertOne({
  ISBN: "83-246-0653-X",
  title: "SQL Server 2005. Programowanie. Od podstaw",
  author: "Robert Vieira",
  year_published: 2007,
  price: 97,
  copies: [
    {
      signature: "S0002",
    },
    {
      signature: "S0003",
    },
  ],
});

const reader1 = db.readers.insertOne({
  PESEL: "12345678900",
  last_name: "Doe",
  city: "New York",
  birth_date: randomDate(),
  last_borrowing: new Date(),
  borrowings: [
    {
      signature: "S0001",
      date: randomDate(),
      days: 42,
    },
    {
      signature: "S0002",
      date: randomDate(),
      days: 24,
    },
  ],
});

const reader2 = db.readers.insertOne({
  PESEL: "12345678901",
  last_name: "Sato",
  city: "Nanporo",
  birth_date: randomDate(),
  last_borrowing: new Date(),
  borrowings: [
    {
      signature: "S0003",
      date: randomDate(),
      days: 11,
    },
    {
      signature: "S0002",
      date: randomDate(),
      days: 111,
    },
  ],
});

const reader3 = db.readers.insertOne({
  PESEL: "12345678902",
  last_name: "Soares",
  city: "Sao Paulo",
  birth_date: randomDate(),
  last_borrowing: new Date(),
});

// inserts that should fail validations
// multiple failures
db.books.insertOne({
  ISBN: "VERY LONG ISBN THAT SHOULD NOT BE VALID",
  title: "Some Title",
  author: "Someone",
  year_published: "this is not a valid year",
  price: "this is not a valid price",
});

// pesel too short
db.readers.insertOne({
  PESEL: "1",
  last_name: "Sato",
  city: "Nanporo",
  birth_date: randomDate(),
  last_borrowing: new Date(),
});

// pesel too long
db.readers.insertOne({
  PESEL: "11111111111111111111111111111",
  last_name: "Sato",
  city: "Nanporo",
  birth_date: randomDate(),
  last_borrowing: new Date(),
});

// cleanup
db.borrowings.drop();
db.readers.drop();
db.books_copies.drop();
db.books.drop();

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
});

const book2 = db.books.insertOne({
  ISBN: "83-246-0653-X",
  title: "SQL Server 2005. Programowanie. Od podstaw",
  author: "Robert Vieira",
  year_published: 2007,
  price: 97,
});

const book_copy1 = db.books_copies.insertOne({
  signature: "S0001",
  book: book1.insertedId,
});

const book_copy2 = db.books_copies.insertOne({
  signature: "S0002",
  book: book2.insertedId,
});

const book_copy3 = db.books_copies.insertOne({
  signature: "S0003",
  book: book2.insertedId,
});

const reader1 = db.readers.insertOne({
  PESEL: "12345678900",
  last_name: "Doe",
  city: "New York",
  birth_date: randomDate(),
  last_borrowing: new Date(),
});

const reader2 = db.readers.insertOne({
  PESEL: "12345678901",
  last_name: "Sato",
  city: "Nanporo",
  birth_date: randomDate(),
  last_borrowing: new Date(),
});

const borrowing1 = db.borrowings.insertOne({
  reader: reader1.insertedId,
  book_copy: book_copy2.insertedId,
  date: randomDate(),
  days: 42,
});

const borrowing2 = db.borrowings.insertOne({
  reader: reader1.insertedId,
  book_copy: book_copy3.insertedId,
  date: randomDate(),
  days: 24,
});

const borrowing3 = db.borrowings.insertOne({
  reader: reader2.insertedId,
  book_copy: book_copy1.insertedId,
  date: randomDate(),
  days: 11,
});

const borrowing4 = db.borrowings.insertOne({
  reader: reader2.insertedId,
  book_copy: book_copy2.insertedId,
  date: randomDate(),
  days: 111,
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

// no reader
db.borrowings.insertOne({
  book_copy: book_copy1.insertedId,
  date: randomDate(),
  days: 11,
});

// cleanup
db.borrowings.drop();
db.readers.drop();
db.books_copies.drop();
db.books.drop();

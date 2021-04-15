db.createCollection("books", {
  validator: {
    $jsonSchema: {
      bsonType: "object",
      required: ["ISBN", "title", "author", "year_published", "price"],
      properties: {
        ISBN: {
          bsonType: "string",
          maxLength: 20,
          uniqueItems: true,
          description: "has to be an unique string of max length of 20",
        },
        title: {
          bsonType: "string",
          maxLength: 300,
          description: "has to be a string of max length of 300",
        },
        author: {
          bsonType: "string",
          maxLength: 200,
          description: "has to be a string of max length of 200",
        },
        year_published: {
          bsonType: "double", // should be an integer but for whatever reason mongo casts it as a double
          description: "has to be an integer",
        },
        price: {
          bsonType: "double",
          description: "has to be a double",
        },
        lent_last_month: {
          bsonType: "bool",
          description: "has to be a boolean",
        },
      },
    },
  },
});

db.createCollection("books_copies", {
  validator: {
    $jsonSchema: {
      bsonType: "object",
      required: ["signature", "book"],
      properties: {
        signature: {
          bsonType: "string",
          maxLength: 8,
          uniqueItems: true,
          description: "has to be an unique string of max length of 8",
        },
        book: {
          bsonType: "objectId",
        },
      },
    },
  },
});

db.createCollection("readers", {
  validator: {
    $jsonSchema: {
      bsonType: "object",
      required: ["PESEL", "last_name", "city", "birth_date"],
      properties: {
        PESEL: {
          bsonType: "string",
          // there is no exact length option I guess...
          minLength: 11,
          maxLength: 11,
          uniqueItems: true,
          description: "has to be an unique string of length of 11",
        },
        last_name: {
          bsonType: "string",
          maxLength: 30,
          description: "has to be a string of max length of 30",
        },
        city: {
          bsonType: "string",
          maxLength: 30,
          description: "has to be a string of max length of 30",
        },
        birth_date: {
          bsonType: "date",
          description: "has to be a date",
        },
      },
    },
  },
});

db.createCollection("borrowings", {
  validator: {
    $jsonSchema: {
      bsonType: "object",
      required: ["reader", "book_copy", "date", "days"],
      properties: {
        reader: {
          bsonType: "objectId",
          description: "has to be an id pointing to reader document",
        },
        book_copy: {
          bsonType: "objectId",
          description: "has to be an id pointing to book copy document",
        },
        date: {
          bsonType: "date",
          description: "has to be a date",
        },
        days: {
          bsonType: "double",
          description: "has to be an integer",
        },
      },
    },
  },
});

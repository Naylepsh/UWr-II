const db = {};

// sorting
db.readers.find().sort({ birth_date: 1 }).skip(1).limit(2).pretty();

// nested queries
db.readers.find({ "borrowings.days": { $gte: 100 } }).pretty();

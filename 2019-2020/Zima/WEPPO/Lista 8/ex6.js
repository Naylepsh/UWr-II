/*
create table ppl (
  id serial primary key,
  name varchar(60) not null
);
*/

const pgp = require('pg-promise')(/* options */);
const db = pgp('postgres://postgres:@localhost/weppo');

// function fillDB() {
//   return db.task('fillDatabase', async t => {
//     for (let i = 0; i < 1200000; i++) {
//       await db.one('INSERT INTO ppl(name) VALUES($1) RETURNING id', 'kowalski'+i, r => r.id);
//     }
//   })
// }

// fillDB();

const s = Date.now();
db.one('SELECT * FROM ppl WHERE name = $1', 'kowalski1200000').then( res => {
  console.log(res);
  console.log(Date.now() - s);
})

// before column index
// { id: 1200001, name: 'kowalski1200000' }
// 322

// create unique index name_id ON ppl (name);

// after column index
// { id: 1200001, name: 'kowalski1200000' }
// 73
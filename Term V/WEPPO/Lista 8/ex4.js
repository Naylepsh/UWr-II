const pgp = require('pg-promise')(/* options */);
const db = pgp('postgres://postgres:@localhost/weppo');

/*
CREATE TABLE workplaces (
  id SERIAL PRIMARY KEY,
  name VARCHAR (255)
);

CREATE TABLE people (
  id SERIAL PRIMARY KEY,
  name VARCHAR (255),
  workplace_id INTEGER REFERENCES workplaces(id)
);
*/

async function clearDB() {
  return db.task('clearDB', async t => {
    let count = await t.result('DELETE FROM people', null, a => a.rowCount);
     return count + await t.result('DELETE FROM workplaces', null, a => a.rowCount);
  });
}

async function insertPerson(person, workplace) {
  return db.task('insertUser', async t => {
    let workplaceObj = await t.oneOrNone('SELECT id FROM workplaces WHERE name = $1', workplace.name);
    const workplaceId = workplaceObj ?
     workplaceObj.id : 
     await t.one('INSERT INTO workplaces(name) VALUES($1) RETURNING id', workplace.name, w => w.id );
    return await t.none('INSERT INTO people(name, workplace_id) VALUES($1, $2)', [person.name, workplaceId])
  });
}

async function printPeople() {
  return db.task('printPeople', async t => {
    const people = await t.any('SELECT * FROM people');
    for (const person of people) {
      const workplace = await t.one('SELECT * FROM workplaces WHERE id = $1', person.workplace_id);
      console.log('person:', person, 'workplace:', workplace);
    }
  })
}

// clearDB().then( res => {
  const person = { name: 'Jane Doe' };
  const workplace = { name: 'Johnushex' };
  insertPerson(person, workplace).then( res => {
    printPeople();
  });
// })
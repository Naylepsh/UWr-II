const pgp = require('pg-promise')(/* options */);
const db = pgp('postgres://postgres:@localhost/weppo');

/*
create table osoba (
  id serial primary key,
  imie varchar(60) not null,
  nazwisko varchar(60) not null
);

create table miejsce_pracy (
  id serial primary key,
  nazwa varchar(60) not null
);

create table osoba_miejsce_pracy (
  osoba_id int references osoba (id),
  miejsce_pracy_id int references miejsce_pracy (id),
  constraint osoba_miejsce_pracy_pkey primary key (osoba_id, miejsce_pracy_id)
);
*/


async function insert(person, workplace) {
  return db.task('insert', async t => {
    let workplaceObj = await t.oneOrNone('SELECT id FROM miejsce_pracy WHERE nazwa = $1', workplace.name);
    const workplaceId = workplaceObj ?
      workplaceObj.id : 
      await t.one('INSERT INTO miejsce_pracy(nazwa) VALUES($1) RETURNING id', workplace.name, w => w.id);
    
    let personObj = await t.oneOrNone('SELECT id FROM osoba WHERE imie = $1 AND nazwisko = $2', [person.name, person.surname]);
    const personId = personObj ?
      personObj.id :
      await t.one('INSERT INTO osoba(imie, nazwisko) VALUES($1, $2) RETURNING id', [person.name, person.surname], p => p.id);
    return await t.none('INSERT INTO osoba_miejsce_pracy VALUES($1, $2)', [personId, workplaceId])
  });
}

async function retrieve() {
  return db.task('retrieve', async t => {
    const rows = await t.any('SELECT * from osoba_miejsce_pracy');
    for (const row of rows) {
      console.log(row);
    }
  })
}

const person = { name: 'John', surname: 'Doe' };
const workplace = { name: 'SOmething' };
insert(person, workplace).then( result => {
  console.log(result)
  retrieve();
});

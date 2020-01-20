var pgp = require('pg-promise')(/* options */)
var db = pgp('postgres://postgres:@localhost/weppo')

// db.none('CREATE TABLE Users(id SERIAL PRIMARY KEY, name TEXT UNIQUE)')

// ex 2
async function getInsertUserId(name) {
  return db.task('getInsertUserId', async t => {
      const userId = await t.oneOrNone('SELECT id FROM Users WHERE name = $1', name, u => u && u.id);
      return userId || await t.one('INSERT INTO Users(name) VALUES($1) RETURNING id', name, u => u.id);
  });
}

async function getUsers() {
  return db.task('getUsers', async t => {
    const users = await t.any('SELECT * FROM users');
    return users;
  })
}

function printUsers() {
  getUsers().then( data => console.log(data)).catch( err => console.error(err));
}

printUsers();

getInsertUserId('john')
    .then(userId => {
        console.log('inserted user:', userId)
    })
    .catch(error => {
        // something went wrong;
    });

// ex 3
async function updateUser(id, newName) {
  db.task('updateUser', async t => {
    await t.oneOrNone('UPDATE users SET name = $2 WHERE id = $1', [id, newName]);
  })
}

async function deleteUser(id) {
  db.task('deleteUser', async t => {
    await t.oneOrNone('DELETE FROM users WHERE id = $1', id);
  })
}

updateUser(3, 'johnny');
deleteUser(1);
printUsers();

db.$pool.end();
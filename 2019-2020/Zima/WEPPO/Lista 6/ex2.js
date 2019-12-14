const express = require('express');
const app = express();

app.set('view engine', 'ejs');

const PORT = 3000;

app.get('/', (req, res) => {
  const foo = 'foo';
  const bar = 'bar';
  res.render('ex2/index', {foo, bar});
});

app.listen(PORT, () => {
  console.log(`server started at port ${PORT}`);
});

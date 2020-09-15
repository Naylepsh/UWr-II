const express = require('express');
const app = express();
const cookieParser = require('cookie-parser');

app.use(cookieParser());
app.set('view engine', 'ejs');

const PORT = 3000;

app.get('/', (req, res) => {
  res.render('ex3/index', {cookies: Object.keys(req.cookies)});
});

app.get('/add/:cookie', (req, res) => {
  const cookieValue = new Date().toString();
  res.cookie(req.params.cookie, cookieValue, {maxAge: 360000}); // cookie with expiration date
  res.redirect('/');
});

app.post('/delete/:cookie', (req, res) => {
  const cookie = req.params.cookie;
  res.clearCookie(cookie);
  res.redirect('/');
});

app.listen(PORT, () => {
  console.log(`server started at port ${PORT}`);
});
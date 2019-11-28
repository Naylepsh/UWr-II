const express = require('express'),
app = express(),
bodyParser = require('body-parser');

const PORT = 3000;

app.set('view engine', 'ejs');

app.use(bodyParser.urlencoded({extended: true}));

app.get('/', (req, res) => {
  res.render('form', {numOfExcersises: 10});
});

app.post('/', (req, res) => {
  if (
    !req.body.firstName ||
    !req.body.lastName  ||
    !req.body.subject   
  ) {
    return res.redirect('/');
  }
  const firstName = req.body.firstName;
  const lastName  = req.body.lastName;
  const subject   = req.body.subject;
  let exercises = [];
  for (let [name, score] of req.body.exercises.entries()) {
    score = score ? score : 0;
    exercises.push({
      name,
      score
    });
  }
  res.render('print', { firstName, lastName, subject, exercises});
});


app.listen(PORT, () => {
  console.log(`server started at port ${PORT}`);
});
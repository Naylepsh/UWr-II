const express = require('express'),
app = express();


app.get('/', (req, res) => {
  // res.header('Content-disposition', 'attachment; filename="foo.txt"');
  res.setHeader('Content-disposition', 'attachment; filename="foo.txt"');
  res.send('Lorem ipsum');
});

app.listen(3000, () => {
  console.log('served started at port 3000');
})


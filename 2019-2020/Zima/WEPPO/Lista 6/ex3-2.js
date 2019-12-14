const express = require('express');
const app = express();
const cookieParser = require('cookie-parser');
const session = require('express-session');
const FileStore = require('session-file-store')(session);

app.set('view engine', 'ejs');
app.use(cookieParser());
app.use(session({
  store: new FileStore,
  secret: 'not so secret',
  resave: true,
  saveUninitialized: true
}));

const PORT = 3000;

app.get("/delete", (req,res)=>{
  try {
    req.session.destroy()
  } catch (e) {
    console.error(e);
  } finally {
    res.redirect('/')
  }
})

app.get('/', (req, res) => {
  if (req.session.views) {
    req.session.views++;
    res.setHeader('Content-Type', 'text/html');
    res.write('<p>views: ' + req.session.views + '</p>');
    res.end();
  } else {
    req.session.views = 1;
    res.end('Refresh page!');
  }
});

app.listen(PORT, () => {
  console.log(`server started at port ${PORT}`);
});
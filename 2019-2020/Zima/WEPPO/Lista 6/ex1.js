const express = require('express');
const app = express();
const fs = require('fs');
const {promisify} = require('util');
const crypto = require('crypto');
const path = require('path');
const multer = require('multer');
const upload = multer({ storage: multer.diskStorage({
  destination: (req, res, cb) => {
    cb(null, './uploads');
  },
  filename: (req, file, cb) => {
    cb(null, generateRandomFilename(file));
  }
})});

app.set('view engine', 'ejs');
app.use(express.static(__dirname + '/uploads'));

const PORT = 3000;
const readdir = promisify(fs.readdir);

function generateRandomFilename(file) {
  try {
    const name = crypto.randomBytes(16).toString('hex') + path.extname(file.originalname);
    return name;
  } catch (err) {
    console.log(err);
    return;
  }
}

app.get('/', async (req, res) => {
  try {
    const imageFilenames = await readdir('./uploads/', );
    res.render('ex1/index', {imageFilenames});
  } catch (e) {
    console.error(e);
    res.send('something went wrong:', e);
  }
  
})

app.post('/', upload.single('image'), (req, res) => {
  res.redirect('/');
})

app.listen(PORT, () => {
  console.log(`server started at port ${PORT}`);
});
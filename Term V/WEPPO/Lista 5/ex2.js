const fs = require('fs');
const https = require('https');
 
(async function () {
    const pfx = await fs.promises.readFile('cert.pfx');
    const server = https.createServer({
        pfx: pfx,
        passphrase: '123'
    },
    (req, res) => {
        res.setHeader('Content-type', 'text/html; charset=utf-8');
        res.end(`hello world ${new Date()}`);
    });
    server.listen(3000);
    console.log('started');
})();
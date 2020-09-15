const readline = require('readline');
const fs = require('fs');

let lineReader = readline.createInterface({
  input: fs.createReadStream('logs.txt')
});

let IPs = {};
lineReader.on('line', line => {
  const IP = line.split(' ')[1];
  if (IP in IPs) {
    IPs[IP]++;
  } else {
    IPs[IP] = 0;
  }
});

lineReader.on('close', () => {
  let xs = [];
  for (const IP in IPs) {
    xs.push({IP: IP, n: IPs[IP]});
  }
  xs.sort( (a,b) =>  a.n - b.n);
  for (const IP of xs.reverse().slice(0, 3)) {
    console.log(IP.IP, IP.n);
  } 
});


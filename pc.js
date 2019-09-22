#!/usr/bin/env node

const fs = require('fs');
const { exec } = require('child_process');

const callback = (error, stdout, stderr) => {
  if (error) {
    console.error(`exec error: ${error}`);
    return;
  }
  console.log(`stdout: ${stdout}`);
  console.log(`stderr: ${stderr}`);
};

try {
  const args = process.argv.slice(2);
  if (!fs.existsSync('./node_modules/@fnal/protocol-compiler/pc')) {
    exec('node ./node_modules/@fnal/protocol-compiler/build.js', callback);
  }

  exec(
    `./node_modules/@fnal/protocol-compiler/pc${
      args.length > 0 ? ` ${args.join(' ')}` : ''
    }`,
    callback
  );
} catch (err) {
  console.error(err);
}

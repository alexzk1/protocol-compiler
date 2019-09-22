#!/usr/bin/env node

const { exec } = require('child_process');

exec(
  `make${process.platform === 'darwin' ? ' -f Makefile.darwin' : ''}`,
  (error, stdout, stderr) => {
    if (error) {
      console.error(`exec error: ${error}`);
      return;
    }
    console.log(`stdout: ${stdout}`);
    console.log(`stderr: ${stderr}`);
  }
);

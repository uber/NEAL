#! /usr/bin/env node

var babylon = require("babylon");
var fs = require("fs");

var source = fs.readFileSync(process.stdin.fd, 'utf-8');
var ast = babylon.parse(source);

console.log(JSON.stringify(ast))

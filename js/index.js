'use strict';

process.env['PATH'] = process.env['PATH'] + ':' + process.env['LAMBDA_TASK_ROOT'];
const exec = require('child_process').exec;

const maxBuffer = 1024 * 500;

let formatError = (statusCode, message) => {
  return '[' + statusCode + '] ' + message
}

exports.handler = function(event, context, callback) {
  const prefixedLambdaName = context.functionName;
  const appName = prefixedLambdaName.split('_', 1);
  const lbdName = prefixedLambdaName.split('_').slice(1);
  const input = JSON.stringify(event)
    .replace(/\\/g, "\\\\")
    .replace(/\$/g, "\\$")
    .replace(/'/g, "\\'")
    .replace(/"/g, "\\\"");

  console.log('input: \"', input, '\"')

  const command = './' + appName + ' lbd ' + lbdName + ' \"' + input + '\"';
  exec(command, {maxBuffer: maxBuffer}, (error, stdout, stderr) => {
    console.log('stdout: \"' + stdout + '\"');
    console.log('stderr: \"' + stderr + '\"');
    if (error !== null) {
      console.log('exec error: ' + error);
      callback(formatError(500, error + '", stderr: "' + stderr + '"'));
    } else {
      const result = JSON.parse(stdout);
      if (result.status === 200) {
        callback(null, result.body);
      } else {
        callback(formatError(result.status, result.body));
      }
    }

 });
}

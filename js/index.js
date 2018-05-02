'use strict';

process.env['PATH'] = process.env['PATH'] + ':' + process.env['LAMBDA_TASK_ROOT'];
const exec = require('child_process').exec;

const maxBuffer = 1024 * 500;

let formatError = (statusCode, message) => {
  return '[' + statusCode + '] ' + message
}

exports.handler = function(event, context, callback) {
  const prefixedLambdaName = context.functionName;
  // we extract the qmuli application name and the actual qmuli lambda name
  // from the composite AWS lambda name (separated by '_')
  const appName = prefixedLambdaName.split('_', 1);
  const lbdName = prefixedLambdaName.split('_').slice(1);
  const payload = JSON.stringify(event)
    .replace(/\\/g, "\\\\")
    .replace(/\$/g, "\\$")
    .replace(/'/g, "\\'")
    .replace(/"/g, "\\\"");

  console.log('lambda payload: \"', payload, '\"')

  // the executable built in the AWS environment using Docker will always
  // be named 'lambda'
  const command = './lambda ' + appName + ' lbd execute --lambda-name ' + lbdName + ' --lambda-payload \"' + payload + '\"';
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

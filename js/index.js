'use strict';

process.env['PATH'] = process.env['PATH'] + ':' + process.env['LAMBDA_TASK_ROOT'];
const spawn = require('child_process').spawn;

// const maxBuffer = 1024 * 500;

exports.handler = (event, context, callback) => {
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
  var child = spawn('./lambda', [appName, 'lbd', 'execute', '--lambda-name', lbdName, '--lambda-payload', payload]);
  child.stdout.on('data', out => {
    out.toString().split("\n").map(line => {
      if (line != "") // sometimes we get transient spurious empty lines from this node.js stdout event mechanism
        console.log(line);
    });
  });

  child.stderr.on('data', out => {
    console.log(out.toString());
  });
  child.on('exit', exitCode => {
    callback(null, exitCode);
  });

}


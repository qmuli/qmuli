process.env['PATH'] = process.env['PATH'] + ':' + process.env['LAMBDA_TASK_ROOT'];
var exec = require('child_process').exec;
exports.handler = function(event, context) {
  var prefixedLambdaName = context.functionName;
  var appName = prefixedLambdaName.split('-', 1);
  var lbdName = prefixedLambdaName.split('-').slice(1);
  var input = JSON.stringify(event)
    .replace(/\\/g, "\\\\")
    .replace(/\$/g, "\\$")
    .replace(/'/g, "\\'")
    .replace(/"/g, "\\\"");
 console.log('input: \"', input, '\"')
 var child = exec('./' + appName + ' lbd ' + lbdName + ' \"' + input + '\"', {maxBuffer: 1024 * 500}, function(error, stdout, stderr) {
   console.log('stdout: \"' + stdout + '\"');
   console.log('stderr: \"' + stderr + '\"');
   if (error !== null) { console.log('exec error: ' + error); }
   context.done(null, JSON.parse(stdout));
 });
}

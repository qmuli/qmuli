{-# LANGUAGE OverloadedStrings #-}

module Qi.Deploy.JS where

import qualified Data.Text as T

js :: T.Text
js = T.concat ["\
  \ process.env['PATH'] = process.env['PATH'] + ':' + process.env['LAMBDA_TASK_ROOT']\n\
  \ var exec = require('child_process').exec;\n\
  \ exports.handler = function(event, context) {\n\
  \  var prefixedLambdaName = context.functionName;\n\
  \  var appName = prefixedLambdaName.split('-', 1);\n\
  \  var lbdName = prefixedLambdaName.split('-').slice(1);\n\
  \  var input = JSON.stringify(event)\n\
  \    .replace(/\\\\/g, \"\\\\\\\\\")\n\
  \    .replace(/\\$/g, \"\\\\$\")\n\
  \    .replace(/'/g, \"\\\\'\")\n\
  \    .replace(/\"/g, \"\\\\\\\"\");\n\
  \  console.log(\"input:\", input)\n\
  \  var child = exec('./' + appName + ' lbd ' + lbdName + ' \"' + input + '\"', {maxBuffer: 1024 * 500}, function(error, stdout, stderr) {\n\
  \    console.log('stdout: ' + stdout);\n\
  \    console.log('stderr: ' + stderr);\n\
  \    if (error !== null) { console.log('exec error: ' + error); }\n\
  \    context.done(null, JSON.parse(stdout));\n\
  \  });\n\
  \ }\n\
  \ "]

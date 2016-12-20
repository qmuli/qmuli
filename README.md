Qmuli 
======
pronounced as [**cumuli**: plural for heap, accumulation](http://www.merriam-webster.com/dictionary/cumuli?pronunciation&lang=en_us&dir=c&file=cumulu02)

![CircleCI Status](https://circleci.com/gh/qmuli/qmuli.svg?style=shield&circle-token=:circle-token)


###TL;DR: [Serverless AWS framework](https://serverless.com/) for [Haskell](https://www.haskell.org/)


Purpose
-------

Qmuli is an experimental effort in creating a unified environment, in which one could specify both resource configuration **and** lambda 
behavior of a cloud architecture built on AWS side-by-side in one language without the artificial boundary imposed by the current 
generation of AWS tools.
Advantages of such unification are numerous:

* a more convenient way to specify AWS resources configuration than with regular CloudFormation templates codified in JSON, in a language 
subset that is as declarative as JSON but much more powerful and expressive
* ability of performing static analisys on **both** configuration **and** behaviors **at the same time**, which is more powerful and 
encompassing than doing this on each individually
* possibility of automatic infrastructure diagram/graph generation from this high level DSL code
* no need to duplicate the resource configuration information in the lambda logic that drives the behavior, as it is available and easily 
accessible during the building phase


Motivating Example
------------------

Let's say we want to create a simple contrived example of architecture that would automatically copy the content of any new file uploaded 
to `incoming` bucket into the `outgoing` bucket:



    +-------------+          +-------------------+         +-------------+
    |             |          |  Lambda that      |         |             |
    |  Incoming   | S3 event |  receives the S3  |         |  Outgoing   |
    |  S3 bucket  +--------->|  event and copies +-------->|  S3 bucket  |
    |             |          |  the S3 object    |         |             |
    +-------------+          +-------------------+         +-------------+


In order to accomplish this with regular tools provided by AWS, we would create these resources using one of 3 methods:

- using the AWS console
- using the AWS command line interface (CLI)
- using CloudFormation template

Using console is great for beginners to learn how to provision resources or for quick ad-hoc changes, but involves lots of clicking around
and therefore is not very practical for non-trivial deployments and hard to replicate exactly.

Using AWS CLI allows for better replication/reproducing and maintenance and is much more practical for non-trivial deployments. One could
script the CLI commands to create reproducible provisioning sequences, but one needs to somehow guarantee that the sequence is correct to
satisfy the inter-dependencies among the resources and that the delays are correct to ensure dependencies have been provisioned before
the dependents are attempted to be provisioned.

Using CloudFormation is a step up in that it allows to completely describe the infrastructure that needs to be provisioned in a declarative
json document that could be version-controlled and used as a specification. However writing and maintaining the json template specification
can get unwieldy and lambda behaviors still need to be specified separately and may result in errors if the behavior specification assumes
permissions or resources that don't match or exist in the CloudFormation template.

Qmuli tries to solve potential problems stemming from mismatched resource and behavior specification by unifying them. Then it statically
analyzes, while compiling, prior to actual deployment, whether all the pieces fit and would work correctly together once deployed.

Below is an example of how one would express the above architecture as a "qmulus" (i.e. a unified specification for architecture resources
and behavior) in a single file:

```haskell
main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do


      -- create an "input" s3 bucket
      incoming <- s3Bucket "incoming"

      -- create an "output" s3 bucket
      outgoing <- s3Bucket "outgoing"

      -- create a lambda, which will copy an s3 object from "incoming" to "outgoing" buckets
      -- upon an S3 "Put" event.
      -- Attach the lambda to the "incoming" bucket such way so each time a file is uploaded to
      -- the bucket, the lambda is called with the information about the newly uploaded file.
      -- The lambda creation function takes the Lambda name, s3BucketId to attach to, lambda 
      -- function itself and a lambda profile, that specifies attributes like memory size and
      -- timeout, and has meaningful defaults for those.
      void $ s3BucketLambda "copyS3Object" incoming (copyContentsLambda outgoing) $
        def & lpMemorySize .~ M1536

    copyContentsLambda
      :: S3BucketId
      -> S3Event
      -> LambdaProgram ()
    copyContentsLambda sinkBucketId event = do

      let incomingS3Obj = event ^. s3eObject
          outgoingS3Obj = s3oBucketId .~ sinkBucketId $ incomingS3Obj

      -- get the content of the newly uploaded file
      content <- getS3ObjectContent incomingS3Obj

      -- write the content into a new file in the "output" bucket
      putS3ObjectContent outgoingS3Obj content

```

Compiling this qmulus results in a multi-purpose executable binary, which can be used as a CLI tool for management tasks like provisioning
as well as the binary executable that gets packaged and used in all lambdas.

Note: see more involved [DynamoDB backed RESTful API example](https://github.com/qmuli/qmuli/blob/master/examples/apigw-lambda-dynamodb/src/Main.hs)

Getting started
---------------

Thanks to [the recent addition of a dockerized Lambda build](https://github.com/qmuli/qmuli/pull/5/commits), a qmulus **now does not need** to be 
built on an Amazon Linux AMI in order to be compatible with running it on AWS Lambda. One only needs a system with `stack` and `docker` installed in 
order to build everything necessary for a successful deployment.


###Clone and build the library and examples
```sh
git clone --recursive -j8 https://github.com/qmuli/qmuli.git
cd qmuli
stack install
```

###Running an example
The above example is available as the "simple-s3-copy" qmulus.

The `simple-s3-copy <my-globally-unique-name> cf deploy` command does the following:

- generates the CloudFormation (CF) json template
- packages/zips up the executable to be used by lambda
- uploads those to the qmulus S3 bucket (named with <my-unique-name>)


After that is deployed, just create a new CF stack:

`simple-s3-copy <same-globally-unique-name-as-above> cf create`

And voila, you should now have the example deployed and working.
Try uploading a small file into the 'incoming' bucket, you should see the same file copied automatically to the 'outgoing' bucket.


To monitor the status of the stack and view the stack outputs:

`simple-s3-copy <same-globally-unique-name-as-above> cf describe`


To destroy a stack:

`simple-s3-copy <same-globally-unique-name-as-above> cf destroy`


Future work
-----------

The idea is to use this project as an experiment platform to design a toolset that would allow very rapid and painless development for 
serverless architectures, and of course, would leverage all the great stuff that Haskell has to offer. The plan is to add all the usual 
AWS SaaS puzzle-pieces like ApiGateway, Cognito, Dynamo, SQS, SNS, etc and make them easily composable. Furthermore, using free/operational monad based DSLs would allow for various ways to statically analyze architecture + lambda behaviors and infer various properties that would allow for optimizations, correctness checking, generating artifacts like visual diagrams, etc in addition to making code safer by not directly using the IO.


Contributors
------------

* Alex Babkin ([@ababkin](https://github.com/ababkin))
* Arnaud Bailly ([@abailly](https://github.com/abailly))

Acknowledgments
---------------

Big kudos to

* David Reaver ([@jdreaver](https://github.com/jdreaver)), the creator and maintainer of the 
[stratosphere](https://github.com/frontrowed/stratosphere) package
* Brendan Hay ([@brendanhay](https://github.com/brendanhay)), the creator and maintainer of the
[amazonka](https://github.com/brendanhay/amazonka) package

as well as to wonderful Haskell community, without whom the task of creating Qmuli would be very hard to impossible.

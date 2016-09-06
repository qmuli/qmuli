Qmu.li
======


Background
----------

AWS currently gives tech companies a great way to cut time and cost to develop and delpoy infrastructure for their services and products by taking care of some software service layers in addition to the low level 
physical and OS infrastrucure layer. There is a case to be made however for potential headroom for improvement by creating a suite of convenient high level tools that would reduce the development friction and safety even further
by providing a unified expressive high-level integrated domain specific language (DSL). One would be able to completely describe their entire infrastructure in this DSL as opposed to having to specify the AWS resources 
configuration (like CloudFormation json templates) separately from the code that would run on some of the resources (AWS Lambda), triggered by events originating in other AWS resources for example.

Purpose
-------

Qmu.li is an experimental effort to explore this possibility of creating a unified environment, in which one could specify both configuration **and** behavior of a cloud architecture side-by-side in one statically typed language 
without the artificial boundary imposed by the current generation of AWS tools.
Advantages of such unification are numerous:
- a more convenient way of specifying AWS resources configuration than with regular CF templates codified in JSON, in a language subset that is as declarative as JSON but much more powerful and expressive
- ability of performing static analisys on **both** configuration **and** behavior **at the same time**, which is more powerful and encompassing than doing this on each one of the two separately
- possibility of automatic infrastructure diagram/graph generation from this high level DSL code
- no need to duplicate the resource configuration information in the 'code' that drives the behavior, as it is available and easily accessible during the building phase

Name
----

Qmu.li is a derivative of "Cumuli", a plural for Cumulus, which means "heap" or "pile" in Latin. We will call a unified specification for configuration and behavior a "qmulus"

Language
--------

DSLs provided to a user are high level DSLs embedded into Haskell - a modern statically-typed pure functional language.


Example
-------

Below is an example of how one would express a "qmulus" that would automatically copy the content of any new file uploaded into s3 bucket named "bucket1" into an s3 object in "bucket2":

```haskell
config :: ConfigProgram ()
config = do
  input <- createS3Bucket "bucket1"
  output <- createS3Bucket "bucket2"
  createS3BucketLambda "mylambda" input (copyContentsLambda output)

copyContentsLambda :: S3BucketIdentifier -> S3Event -> LambdaProgram ()
copyContentsLambda outputBucket S3Event{s3Object} = do
  content <- getS3ObjectContent s3Object
  putS3ObjectContent outputS3Object content

  where
    outputS3Object = S3Object outputBucket (S3Key "out")
```

DSL
---

The "Config DSL" would have functions like `createS3Bucket` or `createS3BucketLambda` for creating/defining the AWS resources like s3 bucket or a lambda that gets triggered by an s3 event. 
The "Lambda DSL" would consist of functions like `getS3ObjectContent` and `putS3ObjectContent` used in the above qmulus for operations like accessing and writing content of s3 objects.

Once the qmulus is built successfully, a binary file becomes available, which exposes various commands for deploying it onto the cloud.


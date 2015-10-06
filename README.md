mismi
=====

> Source of the Amazon
> - http://en.wikipedia.org/wiki/Amazon_River

![mismi](http://upload.wikimedia.org/wikipedia/commons/a/a4/Nevado_Mismi.jpg)


### mismi-s3

##### Current situation
Use all `aws` implementations by default, where functionality falls short then fallback to
the `amazonka-s3` implementations.

##### Future plans
Use `amazonka-s3` implementations by default and fall back to the `aws` implementations
where amazonka falls short.

##### STS
The `"X-Amz-Security-Token"` header is only supported by amazonka and therefore all
required sts functionality will have to be implemented in terms of `amazonka-s3`.

Running amazonka `AWST` - [Mismi.Control.Amazonka](https://github.com/ambiata/mismi/blob/master/mismi-core/src/Mismi/Control/Amazonka.hs#L81)
```
runAWSWithCreds :: Region -> AccessKey -> SecretKey -> Maybe SecurityToken -> Maybe UTCTime -> AWS a -> EitherT AWSError IO a
```


### Debugging

#### Amazonka - environment variable

Set `AWS_DEBUG` to `true` to enable amazonka debugging

#### Amazonka - manually

See `Mismi.Control` to add a logger to the runner
```
runAWS :: Region -> AWS a -> EitherT AWSError IO a
runAWS r a = do
  lgr <- newLogger Trace stdout
  e <- liftIO $ AWS.getEnv r Discover <&> envLogger .~ lgr
  runAWSWithEnv e a
```

### Command line

The `mismi-s3` module provides a command-line tool for interacting with s3 resources
```
cd mismi-s3
./cabal build
alias s3="dist/build/s3/s3"

s3 --help

Available commands:
  upload                   Upload a file to s3.
  download                 Download a file from s3.
  copy                     Copy a file from an S3 address to another S3 address.
  move                     Move an S3 address to another S3 address
  exists                   Check if an address exists.
  delete                   Delete an address.
  write                    Write to an address.
  read                     Read from an address.
  cat                      Stream data from an address.
  size                     Get the size of an address.
  sync                     Sync between two prefixes.
  ls                       Stream a recursively list of objects on a prefix
```

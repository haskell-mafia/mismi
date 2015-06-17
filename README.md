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

### Debugging

#### Amazonka
See `Mismi.Control.Amazonka` to add a logger to the runner
```
runAWS :: Region -> AWS a -> EitherT AWSError IO a
runAWS r a = do
  lgr <- newLogger Trace stdout
  e <- liftIO $ AWS.getEnv r Discover <&> envLogger .~ lgr
  runAWSWithEnv e a
```

#### Aws
Add the `Debug` logger to the aws `Configuration`
```
runS3WithDefaults :: S3Action a -> IO a
runS3WithDefaults action =
  baseConfiguration' >>= \cfg -> do
    let c = cfg { logger = defaultLog Aws.Aws.Debug }
    runS3WithCfg c Sydney action
```

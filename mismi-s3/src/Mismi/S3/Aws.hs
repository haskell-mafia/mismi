-- | Legacy Mismi code based on `aws` package
--   Import this is you are not ready to migrate your code to newer 'amazonka'-based 'Mismi.S3'
--   NOTE: These modules will be depricated and removed
module Mismi.S3.Aws (
    module X
  ) where

import           Mismi.S3.Aws.Control as X
import           Mismi.S3.Aws.Data as X
import           Mismi.S3.Aws.Default as X
import           Mismi.S3.Data as X

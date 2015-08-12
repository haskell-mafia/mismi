-- | Legacy Mismi code based on `aws` package
--   Import this is you are not ready to migrate your code to newer 'amazonka'-based 'Mismi.SQS'
--   NOTE: These modules will be depricated and removed
module Mismi.SQS.Aws (
    module X
  ) where

import           Mismi.SQS.Aws.Data as X
import           Mismi.SQS.Aws.Control as X
import           Mismi.SQS.Aws.Commands as X
import           Mismi.SQS.Data as X

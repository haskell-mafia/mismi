{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.Amazonka (
    module A
  ) where

import           Network.AWS as A
import           Network.AWS.Auth as A
import           Network.AWS.Data as A
import           Network.AWS.Error as A
import           Network.AWS.Waiter as A
#if MIN_VERSION_amazonka(1,4,0)
import           Network.AWS.Prelude as A
#else
import           Network.AWS.Prelude as A hiding ((&))
#endif

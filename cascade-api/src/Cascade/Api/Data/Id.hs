{-|
Module      : Cascade.Api.Data.Id
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.Id
    ( Id (..)
    ) where

import           Control.Lens.TH ( makeWrapped )
import           Data.Aeson      ( FromJSON, ToJSON )
import           Libjwt.Classes  ( JwtRep )
import           Servant.API     ( FromHttpApiData, ToHttpApiData )

newtype Id (entity :: Type)
  = Id { unId :: UUID }
  deriving stock (Generic)
  deriving newtype
  ( Eq
  , FromHttpApiData
  , FromJSON
  , JwtRep ByteString
  , Ord
  , Show
  , ToHttpApiData
  , ToJSON
  )

makeWrapped ''Id

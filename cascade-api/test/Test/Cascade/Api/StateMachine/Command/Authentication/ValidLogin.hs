{-|
Module      : Test.Cascade.Api.StateMachine.Command.Authentication.ValidLogin
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Authentication.ValidLogin (validLogin) where

import qualified Cascade.Api.Data.Authentication    as Authentication
import           Cascade.Api.Network.TestClient      ( AuthToken )
import qualified Cascade.Api.Network.TestClient.Api.Authentication
                                                    as Cascade.Api.Authentication
import           Cascade.Api.Servant.Authentication  ( headerAndPayloadCookieName
                                                     , signatureCookieName
                                                     )
import           Cascade.Api.Test.Prelude            ( )
import           Control.Lens                        ( (?~)
                                                     , (^.)
                                                     , (^?)
                                                     , (^@..)
                                                     , _1
                                                     , _2
                                                     , at
                                                     , findOf
                                                     , folded
                                                     , has
                                                     , ix
                                                     , view
                                                     )
import qualified Data.Sequence                      as Seq
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import           Network.HTTP.Types.Header           ( hSetCookie )
import           Test.Cascade.Api.StateMachine.Model ( Model )
import qualified Test.Cascade.Api.StateMachine.Model.Lens
                                                    as Model.Lens
import           Web.Cookie                          ( parseSetCookie
                                                     , setCookieName
                                                     , setCookieValue
                                                     )

newtype ValidLogin (v :: Type -> Type) = ValidLogin Authentication.RawCredential deriving stock Show

instance HTraversable ValidLogin where
  htraverse _ = pure . coerce

validLogin :: MonadGen g => MonadTest m => MonadIO m => Command g m Model
validLogin = Command generator execute [Require require, Update update]

generator :: MonadGen g => Model Symbolic -> Maybe (g (ValidLogin Symbolic))
generator model = case validLogins of
  [] -> Nothing
  _  -> Just <| Gen.element validLogins
 where
  validLogins = do
    (username, password) <- model ^@.. Model.Lens.indexPasswordByUsername
    pure . ValidLogin <| Authentication.RawCredential { .. }

require :: Model Symbolic -> ValidLogin Symbolic -> Bool
require model (ValidLogin Authentication.RawCredential { username, password }) = hasUserBeenCreated && isPasswordCorrect
 where
  hasUserBeenCreated = model |> has (#user . #byUsername . ix username)
  isPasswordCorrect  = Just password == model ^? #user . #byUsername . ix username . #password

execute :: MonadIO m => MonadTest m => ValidLogin Concrete -> m AuthToken
execute (ValidLogin credential) = do
  label "[Authentication/Login Valid]"
  ensure =<< evalIO (Cascade.Api.Authentication.login credential)

update :: Model v -> ValidLogin v -> Var AuthToken v -> Model v
update model (ValidLogin Authentication.RawCredential { username }) token = model |> #authToken . #byUsername . at username ?~ token

ensure :: MonadTest m => Cascade.Api.Authentication.LoginResponse -> m AuthToken
ensure response = do
  response ^. #responseStatusCode . #statusCode === 204
  let setCookies = (response ^. #responseHeaders) |> Seq.filter ((== hSetCookie) . view _1) |> fmap (parseSetCookie . view _2)

  headerAndPayload <- setCookies |> findOf folded ((== headerAndPayloadCookieName) . setCookieName) |> fmap setCookieValue |> evalMaybe
  sig              <- setCookies |> findOf folded ((== signatureCookieName) . setCookieName) |> fmap setCookieValue |> evalMaybe
  pure (headerAndPayload, sig)

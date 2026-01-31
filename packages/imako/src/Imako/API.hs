{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{- | Servant API for Imako.

Provides JSON endpoints for the SolidJS frontend.
-}
module Imako.API (
  ImakoAPI,
  apiServer,
) where

import Data.LVar qualified as LVar
import Data.Time (Day, getZonedTime, localDay, zonedTimeToLocalTime)
import Imako.Core (AppView, mkAppView)
import Ob qualified
import Servant

-- | The Imako API type
type ImakoAPI = "api" :> "view" :> Get '[JSON] AppView

-- | Create the API server
apiServer :: FilePath -> LVar.LVar Ob.Vault -> Server ImakoAPI
apiServer vaultPath vaultVar = getView
  where
    getView :: Handler AppView
    getView = liftIO $ do
      vault <- LVar.get vaultVar
      today <- getLocalToday
      pure $ mkAppView today vaultPath vault

    getLocalToday :: IO Day
    getLocalToday = localDay . zonedTimeToLocalTime <$> getZonedTime

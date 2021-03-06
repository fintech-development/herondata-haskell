{-
   Heron Data API

   No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)

   OpenAPI Version: 3.0.0
   Heron Data API API version: 2021-07-19
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : HeronData.API.Testing
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module HeronData.API.Testing where

import HeronData.Core
import HeronData.MimeTypes
import HeronData.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Testing

-- *** apiHelloWorldAuthenticatedGet

-- | @GET \/api\/hello_world\/authenticated@
-- 
-- Test authentication
-- 
-- AuthMethod: 'AuthBasicBasicAuth'
-- 
apiHelloWorldAuthenticatedGet
  :: HeronDataRequest ApiHelloWorldAuthenticatedGet MimeNoContent InlineResponse2004 MimeJSON
apiHelloWorldAuthenticatedGet =
  _mkRequest "GET" ["/api/hello_world/authenticated"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)

data ApiHelloWorldAuthenticatedGet  
-- | @application/json@
instance Produces ApiHelloWorldAuthenticatedGet MimeJSON


-- *** apiHelloWorldGet

-- | @GET \/api\/hello_world@
-- 
-- Test endpoint (no authentication)
-- 
-- AuthMethod: 'AuthBasicBasicAuth'
-- 
apiHelloWorldGet
  :: HeronDataRequest ApiHelloWorldGet MimeNoContent InlineResponse2004 MimeJSON
apiHelloWorldGet =
  _mkRequest "GET" ["/api/hello_world"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)

data ApiHelloWorldGet  
-- | @application/json@
instance Produces ApiHelloWorldGet MimeJSON


{-
   Heron Data API

   No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)

   OpenAPI Version: 3.0.0
   Heron Data API API version: 2021-07-19
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : HeronData.API.Merchants
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module HeronData.API.Merchants where

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


-- ** Merchants

-- *** apiMerchantsExtractPost

-- | @POST \/api\/merchants\/extract@
-- 
-- Extract Merchant from a transaction description
-- 
-- AuthMethod: 'AuthBasicBasicAuth'
-- 
apiMerchantsExtractPost
  :: (Consumes ApiMerchantsExtractPost MimeJSON, MimeRender MimeJSON InlineObject2)
  => InlineObject2 -- ^ "inlineObject2"
  -> HeronDataRequest ApiMerchantsExtractPost MimeJSON InlineResponse2005 MimeJSON
apiMerchantsExtractPost inlineObject2 =
  _mkRequest "POST" ["/api/merchants/extract"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `setBodyParam` inlineObject2

data ApiMerchantsExtractPost 
instance HasBodyParam ApiMerchantsExtractPost InlineObject2 

-- | @application/json@
instance Consumes ApiMerchantsExtractPost MimeJSON

-- | @application/json@
instance Produces ApiMerchantsExtractPost MimeJSON


-- *** apiMerchantsHeronIdGet

-- | @GET \/api\/merchants\/{heron_id}@
-- 
-- Get Merchant by heron_id
-- 
-- AuthMethod: 'AuthBasicBasicAuth'
-- 
apiMerchantsHeronIdGet
  :: HeronDataRequest ApiMerchantsHeronIdGet MimeNoContent InlineResponse2007 MimeJSON
apiMerchantsHeronIdGet =
  _mkRequest "GET" ["/api/merchants/{heron_id}"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)

data ApiMerchantsHeronIdGet  
-- | @application/json@
instance Produces ApiMerchantsHeronIdGet MimeJSON


-- *** apiMerchantsSearchGet

-- | @GET \/api\/merchants\/search@
-- 
-- Search Merchants by name
-- 
-- AuthMethod: 'AuthBasicBasicAuth'
-- 
apiMerchantsSearchGet
  :: Name -- ^ "name" -  Full or partial name, minimum 3 characters
  -> HeronDataRequest ApiMerchantsSearchGet MimeNoContent InlineResponse2006 MimeJSON
apiMerchantsSearchGet (Name name) =
  _mkRequest "GET" ["/api/merchants/search"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `addQuery` toQuery ("name", Just name)

data ApiMerchantsSearchGet  
-- | @application/json@
instance Produces ApiMerchantsSearchGet MimeJSON


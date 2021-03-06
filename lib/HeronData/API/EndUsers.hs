{-
   Heron Data API

   No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)

   OpenAPI Version: 3.0.0
   Heron Data API API version: 2021-07-19
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : HeronData.API.EndUsers
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module HeronData.API.EndUsers where

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


-- ** EndUsers

-- *** apiEndUsersBalanceGet

-- | @GET \/api\/end_users\/balance@
-- 
-- Get EndUser balance
-- 
-- Get balance for given EndUser on a per account level
-- 
-- AuthMethod: 'AuthBasicBasicAuth'
-- 
apiEndUsersBalanceGet
  :: HeronDataRequest ApiEndUsersBalanceGet MimeNoContent InlineResponse2002 MimeJSON
apiEndUsersBalanceGet =
  _mkRequest "GET" ["/api/end_users/balance"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)

data ApiEndUsersBalanceGet  

-- | /Optional Param/ "end_user_heron_id" - Heron-generated id for end user; either end_user_id or end_user_heron_id is required
instance HasOptionalParam ApiEndUsersBalanceGet EndUserHeronId where
  applyOptionalParam req (EndUserHeronId xs) =
    req `addQuery` toQuery ("end_user_heron_id", Just xs)

-- | /Optional Param/ "timestamp_max" - Filter for transactions with timestamp earlier than the input value
instance HasOptionalParam ApiEndUsersBalanceGet TimestampMax where
  applyOptionalParam req (TimestampMax xs) =
    req `addQuery` toQuery ("timestamp_max", Just xs)

-- | /Optional Param/ "timestamp_min" - Filter for transactions with timestamp after the input value
instance HasOptionalParam ApiEndUsersBalanceGet TimestampMin where
  applyOptionalParam req (TimestampMin xs) =
    req `addQuery` toQuery ("timestamp_min", Just xs)

-- | /Optional Param/ "end_user_id" - end_user_id for statistics; either end_user_id or end_user_heron_id is required
instance HasOptionalParam ApiEndUsersBalanceGet EndUserId where
  applyOptionalParam req (EndUserId xs) =
    req `addQuery` toQuery ("end_user_id", Just xs)
-- | @application/json@
instance Produces ApiEndUsersBalanceGet MimeJSON


-- *** apiEndUsersForecastGet

-- | @GET \/api\/end_users\/forecast@
-- 
-- Get EndUser forecasts
-- 
-- Get forecast amounts for a given EndUser and category
-- 
-- AuthMethod: 'AuthBasicBasicAuth'
-- 
apiEndUsersForecastGet
  :: HeronDataRequest ApiEndUsersForecastGet MimeNoContent [EndUserForecastOutputSchema] MimeJSON
apiEndUsersForecastGet =
  _mkRequest "GET" ["/api/end_users/forecast"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)

data ApiEndUsersForecastGet  

-- | /Optional Param/ "category_label" - Label of category to be forecasted; either category_heron_id or category_label must be present
instance HasOptionalParam ApiEndUsersForecastGet CategoryLabel where
  applyOptionalParam req (CategoryLabel xs) =
    req `addQuery` toQuery ("category_label", Just xs)

-- | /Optional Param/ "end_user_heron_id" - Heron-generated id for end user; either end_user_id or end_user_heron_id is required
instance HasOptionalParam ApiEndUsersForecastGet EndUserHeronId where
  applyOptionalParam req (EndUserHeronId xs) =
    req `addQuery` toQuery ("end_user_heron_id", Just xs)

-- | /Optional Param/ "date_granularity" - Aggregate results over time, i.e., aggregate by week or by month
instance HasOptionalParam ApiEndUsersForecastGet DateGranularity where
  applyOptionalParam req (DateGranularity xs) =
    req `addQuery` toQuery ("date_granularity", Just xs)

-- | /Optional Param/ "category_heron_id" - Heron ID of category to be forecasted; either category_heron_id or category_label must be present
instance HasOptionalParam ApiEndUsersForecastGet CategoryHeronId where
  applyOptionalParam req (CategoryHeronId xs) =
    req `addQuery` toQuery ("category_heron_id", Just xs)

-- | /Optional Param/ "end_user_id" - end_user_id for statistics; either end_user_id or end_user_heron_id is required
instance HasOptionalParam ApiEndUsersForecastGet EndUserId where
  applyOptionalParam req (EndUserId xs) =
    req `addQuery` toQuery ("end_user_id", Just xs)
-- | @application/json@
instance Produces ApiEndUsersForecastGet MimeJSON


-- *** apiEndUsersGet

-- | @GET \/api\/end_users@
-- 
-- List EndUsers
-- 
-- Get a list of EndUsers
-- 
-- AuthMethod: 'AuthBasicBasicAuth'
-- 
apiEndUsersGet
  :: HeronDataRequest ApiEndUsersGet MimeNoContent InlineResponse2001 MimeJSON
apiEndUsersGet =
  _mkRequest "GET" ["/api/end_users"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)

data ApiEndUsersGet  

-- | /Optional Param/ "page" - Pagination page number
instance HasOptionalParam ApiEndUsersGet Page where
  applyOptionalParam req (Page xs) =
    req `addQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "name" - Filter by name associated with end user
instance HasOptionalParam ApiEndUsersGet Name where
  applyOptionalParam req (Name xs) =
    req `addQuery` toQuery ("name", Just xs)

-- | /Optional Param/ "limit" - Pagination limit per page
instance HasOptionalParam ApiEndUsersGet Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "status" - Filter by status of end user
instance HasOptionalParam ApiEndUsersGet Status where
  applyOptionalParam req (Status xs) =
    req `addQuery` toQuery ("status", Just xs)

-- | /Optional Param/ "heron_id" - Unique ID generated by Heron
instance HasOptionalParam ApiEndUsersGet HeronId where
  applyOptionalParam req (HeronId xs) =
    req `addQuery` toQuery ("heron_id", Just xs)

-- | /Optional Param/ "end_user_id" - Filter by ID associated with end_user_id field in transactions
instance HasOptionalParam ApiEndUsersGet EndUserId where
  applyOptionalParam req (EndUserId xs) =
    req `addQuery` toQuery ("end_user_id", Just xs)

-- | /Optional Param/ "order_by" - Order to return end users by
instance HasOptionalParam ApiEndUsersGet OrderBy where
  applyOptionalParam req (OrderBy xs) =
    req `addQuery` toQuery ("order_by", Just xs)
-- | @application/json@
instance Produces ApiEndUsersGet MimeJSON


-- *** apiEndUsersPost

-- | @POST \/api\/end_users@
-- 
-- Create EndUser
-- 
-- Create a new end user. If transactions have previously been sent for this `end_user_id`, use `PUT end_users` to update instead. 
-- 
-- AuthMethod: 'AuthBasicBasicAuth'
-- 
apiEndUsersPost
  :: (Consumes ApiEndUsersPost MimeJSON, MimeRender MimeJSON InlineObject1)
  => InlineObject1 -- ^ "inlineObject1"
  -> HeronDataRequest ApiEndUsersPost MimeJSON InlineObject1 MimeJSON
apiEndUsersPost inlineObject1 =
  _mkRequest "POST" ["/api/end_users"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `setBodyParam` inlineObject1

data ApiEndUsersPost 
instance HasBodyParam ApiEndUsersPost InlineObject1 

-- | @application/json@
instance Consumes ApiEndUsersPost MimeJSON

-- | @application/json@
instance Produces ApiEndUsersPost MimeJSON


-- *** apiEndUsersPut

-- | @PUT \/api\/end_users@
-- 
-- Update EndUser
-- 
-- Confirm an end user is ready for async procesing by sending a status of \"ready\". The `end_user_id` must have previously been sent with at least one Transaction. 
-- 
-- AuthMethod: 'AuthBasicBasicAuth'
-- 
apiEndUsersPut
  :: (Consumes ApiEndUsersPut MimeJSON, MimeRender MimeJSON InlineObject)
  => InlineObject -- ^ "inlineObject"
  -> HeronDataRequest ApiEndUsersPut MimeJSON InlineObject MimeJSON
apiEndUsersPut inlineObject =
  _mkRequest "PUT" ["/api/end_users"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `setBodyParam` inlineObject

data ApiEndUsersPut 
instance HasBodyParam ApiEndUsersPut InlineObject 

-- | @application/json@
instance Consumes ApiEndUsersPut MimeJSON

-- | @application/json@
instance Produces ApiEndUsersPut MimeJSON


-- *** apiEndUsersStatisticsGet

-- | @GET \/api\/end_users\/statistics@
-- 
-- Get EndUser statistics
-- 
-- Get summarized statistics for a given EndUser
-- 
-- AuthMethod: 'AuthBasicBasicAuth'
-- 
apiEndUsersStatisticsGet
  :: HeronDataRequest ApiEndUsersStatisticsGet MimeNoContent InlineResponse2003 MimeJSON
apiEndUsersStatisticsGet =
  _mkRequest "GET" ["/api/end_users/statistics"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)

data ApiEndUsersStatisticsGet  

-- | /Optional Param/ "to_date" - Filter for transactions with timestamp before the input value (as date)
instance HasOptionalParam ApiEndUsersStatisticsGet ToDate where
  applyOptionalParam req (ToDate xs) =
    req `addQuery` toQuery ("to_date", Just xs)

-- | /Optional Param/ "group_by" - Pivot results by merchant or by category
instance HasOptionalParam ApiEndUsersStatisticsGet GroupBy where
  applyOptionalParam req (GroupBy xs) =
    req `addQuery` toQuery ("group_by", Just xs)

-- | /Optional Param/ "end_user_heron_id" - Heron-generated id for end user; either end_user_id or end_user_heron_id is required
instance HasOptionalParam ApiEndUsersStatisticsGet EndUserHeronId where
  applyOptionalParam req (EndUserHeronId xs) =
    req `addQuery` toQuery ("end_user_heron_id", Just xs)

-- | /Optional Param/ "from_date" - Filter for transactions with timestamp after the input value (as date)
instance HasOptionalParam ApiEndUsersStatisticsGet FromDate where
  applyOptionalParam req (FromDate xs) =
    req `addQuery` toQuery ("from_date", Just xs)

-- | /Optional Param/ "date_granularity" - Aggregate results over time, e.g., by week or by quarter
instance HasOptionalParam ApiEndUsersStatisticsGet DateGranularity2 where
  applyOptionalParam req (DateGranularity2 xs) =
    req `addQuery` toQuery ("date_granularity", Just xs)

-- | /Optional Param/ "end_user_id" - end_user_id for statistics; either end_user_id or end_user_heron_id is required
instance HasOptionalParam ApiEndUsersStatisticsGet EndUserId where
  applyOptionalParam req (EndUserId xs) =
    req `addQuery` toQuery ("end_user_id", Just xs)

-- | /Optional Param/ "category_heron_ids" - Filter by specific categories
instance HasOptionalParam ApiEndUsersStatisticsGet CategoryHeronIds where
  applyOptionalParam req (CategoryHeronIds xs) =
    req `addQuery` toQueryColl MultiParamArray ("category_heron_ids", Just xs)

-- | /Optional Param/ "merchant_heron_ids" - Filter by specific merchants
instance HasOptionalParam ApiEndUsersStatisticsGet MerchantHeronIds where
  applyOptionalParam req (MerchantHeronIds xs) =
    req `addQuery` toQueryColl MultiParamArray ("merchant_heron_ids", Just xs)
-- | @application/json@
instance Produces ApiEndUsersStatisticsGet MimeJSON


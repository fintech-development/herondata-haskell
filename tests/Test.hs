{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import HeronData.Model
import HeronData.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy Category)
      propMimeEq MimeJSON (Proxy :: Proxy CategoryFeedback)
      propMimeEq MimeJSON (Proxy :: Proxy DeleteTransactionParams)
      propMimeEq MimeJSON (Proxy :: Proxy EndUser)
      propMimeEq MimeJSON (Proxy :: Proxy EndUserBalanceInputParams)
      propMimeEq MimeJSON (Proxy :: Proxy EndUserForecastInputParams)
      propMimeEq MimeJSON (Proxy :: Proxy EndUserForecastOutputSchema)
      propMimeEq MimeJSON (Proxy :: Proxy EndUserReadParams)
      propMimeEq MimeJSON (Proxy :: Proxy EndUserStatsInputParams)
      propMimeEq MimeJSON (Proxy :: Proxy GetTransactionParams)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject1)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject2)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject3)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject4)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse200)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2001)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2001Meta)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2002)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2002Accounts)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2002Summary)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2003)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2003Statistics)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2004)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2005)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2006)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2007)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2008)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2008Meta)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2008Summary)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse201)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse201Summary)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse202)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse413)
      propMimeEq MimeJSON (Proxy :: Proxy Merchant)
      propMimeEq MimeJSON (Proxy :: Proxy MerchantCategory)
      propMimeEq MimeJSON (Proxy :: Proxy MerchantExtract)
      propMimeEq MimeJSON (Proxy :: Proxy MerchantFeedback)
      propMimeEq MimeJSON (Proxy :: Proxy PaymentProcessor)
      propMimeEq MimeJSON (Proxy :: Proxy TransactionAnnotation)
      propMimeEq MimeJSON (Proxy :: Proxy TransactionFeedbackParams)
      propMimeEq MimeJSON (Proxy :: Proxy TransactionFeedbackSchema)
      propMimeEq MimeJSON (Proxy :: Proxy TransactionGetOutput)
      propMimeEq MimeJSON (Proxy :: Proxy TransactionInput)
      propMimeEq MimeJSON (Proxy :: Proxy TransactionPostOutput)
      

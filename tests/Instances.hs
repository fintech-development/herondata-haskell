{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import HeronData.Model
import HeronData.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary Category where
  arbitrary = sized genCategory

genCategory :: Int -> Gen Category
genCategory n =
  Category
    <$> arbitraryReducedMaybe n -- categoryCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- categoryDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- categoryHeronId :: Maybe Text
    <*> arbitrary -- categoryLabel :: Text
  
instance Arbitrary CategoryFeedback where
  arbitrary = sized genCategoryFeedback

genCategoryFeedback :: Int -> Gen CategoryFeedback
genCategoryFeedback n =
  CategoryFeedback
    <$> arbitraryReducedMaybe n -- categoryFeedbackHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- categoryFeedbackLabel :: Maybe Text
  
instance Arbitrary DeleteTransactionParams where
  arbitrary = sized genDeleteTransactionParams

genDeleteTransactionParams :: Int -> Gen DeleteTransactionParams
genDeleteTransactionParams n =
  DeleteTransactionParams
    <$> arbitraryReducedMaybe n -- deleteTransactionParamsAccountId :: Maybe Text
    <*> arbitraryReducedMaybe n -- deleteTransactionParamsEndUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- deleteTransactionParamsHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- deleteTransactionParamsOnlyDuplicates :: Maybe Bool
    <*> arbitraryReducedMaybe n -- deleteTransactionParamsRequestId :: Maybe Text
  
instance Arbitrary EndUser where
  arbitrary = sized genEndUser

genEndUser :: Int -> Gen EndUser
genEndUser n =
  EndUser
    <$> arbitraryReducedMaybe n -- endUserConfidence :: Maybe Double
    <*> arbitraryReducedMaybe n -- endUserCreated :: Maybe DateTime
    <*> arbitraryReducedMaybeValue n -- endUserCriteria :: Maybe A.Value
    <*> arbitrary -- endUserEndUserId :: Text
    <*> arbitraryReducedMaybe n -- endUserHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- endUserName :: Maybe Text
    <*> arbitrary -- endUserStatus :: E'Status
  
instance Arbitrary EndUserBalanceInputParams where
  arbitrary = sized genEndUserBalanceInputParams

genEndUserBalanceInputParams :: Int -> Gen EndUserBalanceInputParams
genEndUserBalanceInputParams n =
  EndUserBalanceInputParams
    <$> arbitraryReducedMaybe n -- endUserBalanceInputParamsEndUserHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserBalanceInputParamsEndUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserBalanceInputParamsTimestampMax :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- endUserBalanceInputParamsTimestampMin :: Maybe DateTime
  
instance Arbitrary EndUserForecastInputParams where
  arbitrary = sized genEndUserForecastInputParams

genEndUserForecastInputParams :: Int -> Gen EndUserForecastInputParams
genEndUserForecastInputParams n =
  EndUserForecastInputParams
    <$> arbitraryReducedMaybe n -- endUserForecastInputParamsCategoryHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserForecastInputParamsCategoryLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserForecastInputParamsDateGranularity :: Maybe E'DateGranularity
    <*> arbitraryReducedMaybe n -- endUserForecastInputParamsEndUserHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserForecastInputParamsEndUserId :: Maybe Text
  
instance Arbitrary EndUserForecastOutputSchema where
  arbitrary = sized genEndUserForecastOutputSchema

genEndUserForecastOutputSchema :: Int -> Gen EndUserForecastOutputSchema
genEndUserForecastOutputSchema n =
  EndUserForecastOutputSchema
    <$> arbitraryReducedMaybe n -- endUserForecastOutputSchemaDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserForecastOutputSchemaPredicted :: Maybe Double
    <*> arbitraryReducedMaybe n -- endUserForecastOutputSchemaPredictedLower :: Maybe Double
    <*> arbitraryReducedMaybe n -- endUserForecastOutputSchemaPredictedUpper :: Maybe Double
  
instance Arbitrary EndUserReadParams where
  arbitrary = sized genEndUserReadParams

genEndUserReadParams :: Int -> Gen EndUserReadParams
genEndUserReadParams n =
  EndUserReadParams
    <$> arbitraryReducedMaybeValue n -- endUserReadParamsCriteria :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- endUserReadParamsEndUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserReadParamsHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserReadParamsLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- endUserReadParamsName :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserReadParamsOrderBy :: Maybe E'OrderBy
    <*> arbitraryReducedMaybe n -- endUserReadParamsPage :: Maybe Int
    <*> arbitraryReducedMaybe n -- endUserReadParamsStatus :: Maybe E'Status2
  
instance Arbitrary EndUserStatsInputParams where
  arbitrary = sized genEndUserStatsInputParams

genEndUserStatsInputParams :: Int -> Gen EndUserStatsInputParams
genEndUserStatsInputParams n =
  EndUserStatsInputParams
    <$> arbitraryReducedMaybe n -- endUserStatsInputParamsCategoryHeronIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- endUserStatsInputParamsDateGranularity :: Maybe E'DateGranularity2
    <*> arbitraryReducedMaybe n -- endUserStatsInputParamsEndUserHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserStatsInputParamsEndUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endUserStatsInputParamsFromDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- endUserStatsInputParamsGroupBy :: Maybe E'GroupBy
    <*> arbitraryReducedMaybe n -- endUserStatsInputParamsMerchantHeronIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- endUserStatsInputParamsToDate :: Maybe Date
  
instance Arbitrary GetTransactionParams where
  arbitrary = sized genGetTransactionParams

genGetTransactionParams :: Int -> Gen GetTransactionParams
genGetTransactionParams n =
  GetTransactionParams
    <$> arbitraryReducedMaybe n -- getTransactionParamsCategoryHeronId :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- getTransactionParamsConfidenceMax :: Maybe Double
    <*> arbitraryReducedMaybe n -- getTransactionParamsConfidenceMin :: Maybe Double
    <*> arbitraryReducedMaybe n -- getTransactionParamsCreatedDateMax :: Maybe Date
    <*> arbitraryReducedMaybe n -- getTransactionParamsCreatedDateMin :: Maybe Date
    <*> arbitraryReducedMaybe n -- getTransactionParamsDescriptionKeyword :: Maybe Text
    <*> arbitraryReducedMaybe n -- getTransactionParamsDescriptionRegex :: Maybe Text
    <*> arbitraryReducedMaybe n -- getTransactionParamsEndUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- getTransactionParamsFromDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- getTransactionParamsHasMatchingTransaction :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getTransactionParamsHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- getTransactionParamsIncludeDuplicates :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getTransactionParamsIsRecurring :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getTransactionParamsLastUpdatedMax :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- getTransactionParamsLastUpdatedMin :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- getTransactionParamsLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- getTransactionParamsMaxAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- getTransactionParamsMinAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- getTransactionParamsOrderBy :: Maybe E'OrderBy2
    <*> arbitraryReducedMaybe n -- getTransactionParamsPage :: Maybe Int
    <*> arbitraryReducedMaybe n -- getTransactionParamsReferenceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- getTransactionParamsRequestId :: Maybe Text
    <*> arbitraryReducedMaybe n -- getTransactionParamsTimestampDateMax :: Maybe Date
    <*> arbitraryReducedMaybe n -- getTransactionParamsTimestampDateMin :: Maybe Date
    <*> arbitraryReducedMaybe n -- getTransactionParamsToDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- getTransactionParamsTransactionCode :: Maybe Text
  
instance Arbitrary InlineObject where
  arbitrary = sized genInlineObject

genInlineObject :: Int -> Gen InlineObject
genInlineObject n =
  InlineObject
    <$> arbitraryReducedMaybe n -- inlineObjectEndUser :: Maybe EndUser
  
instance Arbitrary InlineObject1 where
  arbitrary = sized genInlineObject1

genInlineObject1 :: Int -> Gen InlineObject1
genInlineObject1 n =
  InlineObject1
    <$> arbitraryReducedMaybe n -- inlineObject1EndUser :: Maybe EndUser
  
instance Arbitrary InlineObject2 where
  arbitrary = sized genInlineObject2

genInlineObject2 :: Int -> Gen InlineObject2
genInlineObject2 n =
  InlineObject2
    <$> arbitraryReducedMaybe n -- inlineObject2Description :: Maybe Text
  
instance Arbitrary InlineObject3 where
  arbitrary = sized genInlineObject3

genInlineObject3 :: Int -> Gen InlineObject3
genInlineObject3 n =
  InlineObject3
    <$> arbitraryReducedMaybe n -- inlineObject3Transactions :: Maybe [TransactionInput]
  
instance Arbitrary InlineObject4 where
  arbitrary = sized genInlineObject4

genInlineObject4 :: Int -> Gen InlineObject4
genInlineObject4 n =
  InlineObject4
    <$> arbitraryReducedMaybe n -- inlineObject4Transaction :: Maybe TransactionFeedbackSchema
  
instance Arbitrary InlineResponse200 where
  arbitrary = sized genInlineResponse200

genInlineResponse200 :: Int -> Gen InlineResponse200
genInlineResponse200 n =
  InlineResponse200
    <$> arbitraryReducedMaybe n -- inlineResponse200Categories :: Maybe [Category]
  
instance Arbitrary InlineResponse2001 where
  arbitrary = sized genInlineResponse2001

genInlineResponse2001 :: Int -> Gen InlineResponse2001
genInlineResponse2001 n =
  InlineResponse2001
    <$> arbitraryReducedMaybe n -- inlineResponse2001Meta :: Maybe InlineResponse2001Meta
    <*> arbitraryReducedMaybe n -- inlineResponse2001EndUsers :: Maybe [EndUser]
  
instance Arbitrary InlineResponse2001Meta where
  arbitrary = sized genInlineResponse2001Meta

genInlineResponse2001Meta :: Int -> Gen InlineResponse2001Meta
genInlineResponse2001Meta n =
  InlineResponse2001Meta
    <$> arbitraryReducedMaybe n -- inlineResponse2001MetaLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2001MetaNextUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2001MetaPage :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2001MetaPages :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2001MetaPrevUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2001MetaTotal :: Maybe Int
  
instance Arbitrary InlineResponse2002 where
  arbitrary = sized genInlineResponse2002

genInlineResponse2002 :: Int -> Gen InlineResponse2002
genInlineResponse2002 n =
  InlineResponse2002
    <$> arbitraryReducedMaybe n -- inlineResponse2002Accounts :: Maybe [InlineResponse2002Accounts]
    <*> arbitraryReducedMaybe n -- inlineResponse2002Summary :: Maybe InlineResponse2002Summary
  
instance Arbitrary InlineResponse2002Accounts where
  arbitrary = sized genInlineResponse2002Accounts

genInlineResponse2002Accounts :: Int -> Gen InlineResponse2002Accounts
genInlineResponse2002Accounts n =
  InlineResponse2002Accounts
    <$> arbitraryReducedMaybe n -- inlineResponse2002AccountsAccountId :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2002AccountsBalance :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse2002AccountsLastTransactionTimestamp :: Maybe DateTime
  
instance Arbitrary InlineResponse2002Summary where
  arbitrary = sized genInlineResponse2002Summary

genInlineResponse2002Summary :: Int -> Gen InlineResponse2002Summary
genInlineResponse2002Summary n =
  InlineResponse2002Summary
    <$> arbitraryReducedMaybe n -- inlineResponse2002SummaryBalance :: Maybe Double
    <*> arbitraryReducedMaybe n -- inlineResponse2002SummaryTimestamp :: Maybe DateTime
  
instance Arbitrary InlineResponse2003 where
  arbitrary = sized genInlineResponse2003

genInlineResponse2003 :: Int -> Gen InlineResponse2003
genInlineResponse2003 n =
  InlineResponse2003
    <$> arbitraryReducedMaybe n -- inlineResponse2003Statistics :: Maybe [InlineResponse2003Statistics]
  
instance Arbitrary InlineResponse2003Statistics where
  arbitrary = sized genInlineResponse2003Statistics

genInlineResponse2003Statistics :: Int -> Gen InlineResponse2003Statistics
genInlineResponse2003Statistics n =
  InlineResponse2003Statistics
    <$> arbitraryReducedMaybe n -- inlineResponse2003StatisticsDt :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2003StatisticsHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2003StatisticsLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2003StatisticsValue :: Maybe Double
  
instance Arbitrary InlineResponse2004 where
  arbitrary = sized genInlineResponse2004

genInlineResponse2004 :: Int -> Gen InlineResponse2004
genInlineResponse2004 n =
  InlineResponse2004
    <$> arbitraryReducedMaybe n -- inlineResponse2004Hello :: Maybe Text
  
instance Arbitrary InlineResponse2005 where
  arbitrary = sized genInlineResponse2005

genInlineResponse2005 :: Int -> Gen InlineResponse2005
genInlineResponse2005 n =
  InlineResponse2005
    <$> arbitraryReducedMaybe n -- inlineResponse2005DescriptionClean :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2005Merchant :: Maybe MerchantExtract
    <*> arbitraryReducedMaybe n -- inlineResponse2005PaymentProcessor :: Maybe PaymentProcessor
  
instance Arbitrary InlineResponse2006 where
  arbitrary = sized genInlineResponse2006

genInlineResponse2006 :: Int -> Gen InlineResponse2006
genInlineResponse2006 n =
  InlineResponse2006
    <$> arbitraryReducedMaybe n -- inlineResponse2006Merchants :: Maybe [Merchant]
  
instance Arbitrary InlineResponse2007 where
  arbitrary = sized genInlineResponse2007

genInlineResponse2007 :: Int -> Gen InlineResponse2007
genInlineResponse2007 n =
  InlineResponse2007
    <$> arbitraryReducedMaybe n -- inlineResponse2007Merchant :: Maybe Merchant
  
instance Arbitrary InlineResponse2008 where
  arbitrary = sized genInlineResponse2008

genInlineResponse2008 :: Int -> Gen InlineResponse2008
genInlineResponse2008 n =
  InlineResponse2008
    <$> arbitraryReducedMaybe n -- inlineResponse2008Meta :: Maybe InlineResponse2008Meta
    <*> arbitraryReducedMaybe n -- inlineResponse2008Summary :: Maybe InlineResponse2008Summary
    <*> arbitraryReducedMaybe n -- inlineResponse2008Transactions :: Maybe [TransactionGetOutput]
  
instance Arbitrary InlineResponse2008Meta where
  arbitrary = sized genInlineResponse2008Meta

genInlineResponse2008Meta :: Int -> Gen InlineResponse2008Meta
genInlineResponse2008Meta n =
  InlineResponse2008Meta
    <$> arbitraryReducedMaybe n -- inlineResponse2008MetaNextUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2008MetaPage :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2008MetaPages :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2008MetaPerPage :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2008MetaPrevUrl :: Maybe Text
  
instance Arbitrary InlineResponse2008Summary where
  arbitrary = sized genInlineResponse2008Summary

genInlineResponse2008Summary :: Int -> Gen InlineResponse2008Summary
genInlineResponse2008Summary n =
  InlineResponse2008Summary
    <$> arbitraryReducedMaybe n -- inlineResponse2008SummaryReturnedTransactions :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2008SummaryTransactions :: Maybe Int
  
instance Arbitrary InlineResponse201 where
  arbitrary = sized genInlineResponse201

genInlineResponse201 :: Int -> Gen InlineResponse201
genInlineResponse201 n =
  InlineResponse201
    <$> arbitraryReducedMaybe n -- inlineResponse201Summary :: Maybe InlineResponse201Summary
    <*> arbitraryReducedMaybe n -- inlineResponse201Transactions :: Maybe [TransactionPostOutput]
  
instance Arbitrary InlineResponse201Summary where
  arbitrary = sized genInlineResponse201Summary

genInlineResponse201Summary :: Int -> Gen InlineResponse201Summary
genInlineResponse201Summary n =
  InlineResponse201Summary
    <$> arbitraryReducedMaybe n -- inlineResponse201SummaryRequestId :: Maybe Text
  
instance Arbitrary InlineResponse202 where
  arbitrary = sized genInlineResponse202

genInlineResponse202 :: Int -> Gen InlineResponse202
genInlineResponse202 n =
  InlineResponse202
    <$> arbitraryReducedMaybe n -- inlineResponse202Message :: Maybe Text
  
instance Arbitrary InlineResponse413 where
  arbitrary = sized genInlineResponse413

genInlineResponse413 :: Int -> Gen InlineResponse413
genInlineResponse413 n =
  InlineResponse413
    <$> arbitraryReducedMaybe n -- inlineResponse413Code :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse413Description :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse413Name :: Maybe Text
  
instance Arbitrary Merchant where
  arbitrary = sized genMerchant

genMerchant :: Int -> Gen Merchant
genMerchant n =
  Merchant
    <$> arbitraryReducedMaybe n -- merchantCategories :: Maybe [MerchantCategory]
    <*> arbitraryReducedMaybe n -- merchantHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- merchantIconUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- merchantLogoUrl :: Maybe Text
    <*> arbitrary -- merchantName :: Text
    <*> arbitrary -- merchantUrl :: Text
  
instance Arbitrary MerchantCategory where
  arbitrary = sized genMerchantCategory

genMerchantCategory :: Int -> Gen MerchantCategory
genMerchantCategory n =
  MerchantCategory
    <$> arbitrary -- merchantCategoryCode :: Text
    <*> arbitraryReducedMaybe n -- merchantCategoryDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- merchantCategorySlug :: Maybe Text
  
instance Arbitrary MerchantExtract where
  arbitrary = sized genMerchantExtract

genMerchantExtract :: Int -> Gen MerchantExtract
genMerchantExtract n =
  MerchantExtract
    <$> arbitraryReducedMaybe n -- merchantExtractCategories :: Maybe [MerchantCategory]
    <*> arbitraryReducedMaybe n -- merchantExtractConfidence :: Maybe Double
    <*> arbitraryReducedMaybe n -- merchantExtractHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- merchantExtractIconUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- merchantExtractLogoUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- merchantExtractName :: Maybe Text
    <*> arbitraryReducedMaybe n -- merchantExtractUrl :: Maybe Text
  
instance Arbitrary MerchantFeedback where
  arbitrary = sized genMerchantFeedback

genMerchantFeedback :: Int -> Gen MerchantFeedback
genMerchantFeedback n =
  MerchantFeedback
    <$> arbitraryReducedMaybe n -- merchantFeedbackHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- merchantFeedbackIsCorrect :: Maybe Bool
    <*> arbitraryReducedMaybe n -- merchantFeedbackName :: Maybe Text
  
instance Arbitrary PaymentProcessor where
  arbitrary = sized genPaymentProcessor

genPaymentProcessor :: Int -> Gen PaymentProcessor
genPaymentProcessor n =
  PaymentProcessor
    <$> arbitraryReducedMaybe n -- paymentProcessorHeronId :: Maybe Text
    <*> arbitrary -- paymentProcessorName :: Text
    <*> arbitrary -- paymentProcessorUrl :: Text
  
instance Arbitrary TransactionAnnotation where
  arbitrary = sized genTransactionAnnotation

genTransactionAnnotation :: Int -> Gen TransactionAnnotation
genTransactionAnnotation n =
  TransactionAnnotation
    <$> arbitraryReducedMaybe n -- transactionAnnotationAnnotator :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionAnnotationConfidence :: Maybe Double
    <*> arbitraryReducedMaybe n -- transactionAnnotationHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionAnnotationLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionAnnotationModelVersion :: Maybe Text
  
instance Arbitrary TransactionFeedbackParams where
  arbitrary = sized genTransactionFeedbackParams

genTransactionFeedbackParams :: Int -> Gen TransactionFeedbackParams
genTransactionFeedbackParams n =
  TransactionFeedbackParams
    <$> arbitraryReducedMaybe n -- transactionFeedbackParamsTriggerWebhook :: Maybe Bool
  
instance Arbitrary TransactionFeedbackSchema where
  arbitrary = sized genTransactionFeedbackSchema

genTransactionFeedbackSchema :: Int -> Gen TransactionFeedbackSchema
genTransactionFeedbackSchema n =
  TransactionFeedbackSchema
    <$> arbitraryReducedMaybe n -- transactionFeedbackSchemaCategory :: Maybe CategoryFeedback
    <*> arbitraryReducedMaybe n -- transactionFeedbackSchemaMerchant :: Maybe MerchantFeedback
  
instance Arbitrary TransactionGetOutput where
  arbitrary = sized genTransactionGetOutput

genTransactionGetOutput :: Int -> Gen TransactionGetOutput
genTransactionGetOutput n =
  TransactionGetOutput
    <$> arbitraryReducedMaybe n -- transactionGetOutputAccountId :: Maybe Text
    <*> arbitrary -- transactionGetOutputAmount :: Double
    <*> arbitraryReducedMaybe n -- transactionGetOutputBalance :: Maybe Double
    <*> arbitraryReducedMaybe n -- transactionGetOutputCategories :: Maybe [TransactionAnnotation]
    <*> arbitraryReducedMaybe n -- transactionGetOutputCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- transactionGetOutputCurrency :: Maybe Text
    <*> arbitrary -- transactionGetOutputDescription :: Text
    <*> arbitraryReducedMaybe n -- transactionGetOutputDescriptionClean :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionGetOutputDuplicateOfId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionGetOutputEndUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionGetOutputHasMatchingTransaction :: Maybe Bool
    <*> arbitraryReducedMaybe n -- transactionGetOutputHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionGetOutputIsPotentialDuplicate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- transactionGetOutputIsRecurring :: Maybe Bool
    <*> arbitraryReducedMaybe n -- transactionGetOutputLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- transactionGetOutputMccCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionGetOutputMerchant :: Maybe Merchant
    <*> arbitraryReducedMaybe n -- transactionGetOutputPaymentProcessor :: Maybe PaymentProcessor
    <*> arbitraryReducedMaybe n -- transactionGetOutputReferenceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionGetOutputRequestId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionGetOutputTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- transactionGetOutputTransactionCode :: Maybe Text
  
instance Arbitrary TransactionInput where
  arbitrary = sized genTransactionInput

genTransactionInput :: Int -> Gen TransactionInput
genTransactionInput n =
  TransactionInput
    <$> arbitraryReducedMaybe n -- transactionInputAccountId :: Maybe Text
    <*> arbitrary -- transactionInputAmount :: Double
    <*> arbitraryReducedMaybe n -- transactionInputBalance :: Maybe Double
    <*> arbitraryReducedMaybe n -- transactionInputCategoriesDefault :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionInputCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionInputDate :: Maybe Date
    <*> arbitrary -- transactionInputDescription :: Text
    <*> arbitraryReducedMaybe n -- transactionInputEndUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionInputMccCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionInputMerchantName :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionInputReferenceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionInputTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- transactionInputTransactionCode :: Maybe Text
  
instance Arbitrary TransactionPostOutput where
  arbitrary = sized genTransactionPostOutput

genTransactionPostOutput :: Int -> Gen TransactionPostOutput
genTransactionPostOutput n =
  TransactionPostOutput
    <$> arbitraryReducedMaybe n -- transactionPostOutputAccountId :: Maybe Text
    <*> arbitrary -- transactionPostOutputAmount :: Double
    <*> arbitraryReducedMaybe n -- transactionPostOutputBalance :: Maybe Double
    <*> arbitraryReducedMaybe n -- transactionPostOutputCategories :: Maybe [TransactionAnnotation]
    <*> arbitraryReducedMaybe n -- transactionPostOutputCurrency :: Maybe Text
    <*> arbitrary -- transactionPostOutputDescription :: Text
    <*> arbitraryReducedMaybe n -- transactionPostOutputDescriptionClean :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionPostOutputDuplicateOfId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionPostOutputEndUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionPostOutputHasMatchingTransaction :: Maybe Bool
    <*> arbitraryReducedMaybe n -- transactionPostOutputHeronId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionPostOutputIsPotentialDuplicate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- transactionPostOutputIsRecurring :: Maybe Bool
    <*> arbitraryReducedMaybe n -- transactionPostOutputMccCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionPostOutputMerchant :: Maybe Merchant
    <*> arbitraryReducedMaybe n -- transactionPostOutputPaymentProcessor :: Maybe PaymentProcessor
    <*> arbitraryReducedMaybe n -- transactionPostOutputReferenceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionPostOutputRequestId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionPostOutputTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- transactionPostOutputTransactionCode :: Maybe Text
  



instance Arbitrary E'DateGranularity where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'DateGranularity2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'GroupBy where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'OrderBy where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'OrderBy2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status2 where
  arbitrary = arbitraryBoundedEnum


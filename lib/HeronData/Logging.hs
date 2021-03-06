{-
   Heron Data API

   No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)

   OpenAPI Version: 3.0.0
   Heron Data API API version: 2021-07-19
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : HeronData.Logging
Logging functions
-}
{-# LANGUAGE CPP #-}

#ifdef USE_KATIP

module HeronData.Logging
  ( module HeronData.LoggingKatip
  ) where

import HeronData.LoggingKatip

#else

module HeronData.Logging
  ( module HeronData.LoggingMonadLogger
  ) where

import HeronData.LoggingMonadLogger

#endif

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Locking ( FileState , available , readOnly , unavailable ) where

import Data.Aeson.Types
import GHC.Generics

data FileState = Available
               | ReadOnly
               | Unavailable
               deriving (Show, Generic)

instance ToJSON FileState
instance FromJSON FileState

available   = Available
readOnly    = ReadOnly
unavailable = Unavailable
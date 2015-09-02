{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.Selenium.Monad where

import Prelude
import Data.List
import Test.Config (Config())
import Control.Monad.Reader.Trans
import Control.Monad.Reader.Class
import Selenium.Monad

type Check a = Selenium () (config :: Config) a


-- READER
getConfig :: Check Config
getConfig = _.config <$> ask



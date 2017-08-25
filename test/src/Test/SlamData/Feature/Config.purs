{-
Copyright 2017 SlamData, Inc.

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

module Test.SlamData.Feature.Config where

type Config =
  { restoreCmd ∷ String
  , selenium ∷
      { jar ∷ String
      , waitTime ∷ Int
      }
  , slamdataUrl ∷ String
  , database ∷
      { name ∷ String
      , type ∷ String
      , host ∷ String
      , port ∷ String
      }
  , upload ∷
      { filePaths ∷ Array String }
  , download ∷
      { folder ∷ String }
  , whoami ∷ String
  }

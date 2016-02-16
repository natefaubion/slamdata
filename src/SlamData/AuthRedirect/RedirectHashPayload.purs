module SlamData.AuthRedirect.RedirectHashPayload
  ( RedirectHashPayload()
  , uriHashParser
  , parseUriHash
  ) where

import Prelude
import Data.Either as E
import Data.List as L
import Data.Maybe as M
import Data.String as S
import Text.Parsing.StringParser as P
import Text.Parsing.StringParser.Combinators as PC
import Text.Parsing.StringParser.String as PS
import Data.Tuple as T
import Data.StrMap as SM

import OIDCCryptUtils.Types as OIDC

type RedirectHashPayload =
  { idToken :: OIDC.IdToken
  , authUser :: String
  , state :: OIDC.BoundStateJWS
  , prompt :: String
  }

parameterParser :: P.Parser (T.Tuple String String)
parameterParser = do
  key <- PC.many (PS.noneOf ['=']) <#> L.fromList >>> S.fromCharArray
  PS.char '='
  val <- PC.many (PS.noneOf ['&']) <#> L.fromList >>> S.fromCharArray
  pure $ T.Tuple key val

parametersParser :: P.Parser (SM.StrMap String)
parametersParser =
  PC.sepBy parameterParser (PS.char '&')
    <#> SM.fromFoldable

-- | Parse the payload from the URI hash. Example:
-- |
-- |     #id_token=foo&authuser=0&hd=slamdata.com&state=bar&prompt=consent
-- |
uriHashParser :: P.Parser RedirectHashPayload
uriHashParser = do
  PS.char '#'
  params <- parametersParser

  let lookup key = SM.lookup key params # M.maybe (P.fail $ "missing " <> key) pure
  idToken <- lookup "id_token" <#> OIDC.IdToken
  authUser <- lookup "authuser"
  state <- lookup "state" <#> OIDC.BoundStateJWS
  prompt <- lookup "prompt"

  pure
    { idToken
    , authUser
    , state
    , prompt
    }

parseUriHash
  :: String
  -> E.Either P.ParseError RedirectHashPayload
parseUriHash =
  P.runParser uriHashParser

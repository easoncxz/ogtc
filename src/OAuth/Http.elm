
module OAuth.Http exposing
  ( get
  , post
  )

import Http exposing (Request)
import Json.Decode as JD exposing (Decoder)

import OAuth.Models exposing (Token(..))

auth : Token -> Http.Header
auth (BearerToken t) =
  Http.header "Authorization" ("Bearer " ++ t)

buildRequest :
  Token
  -> { method : String
     , url : String
     , body : Http.Body
     , expect : Http.Expect a
     }
  -> Http.Request a
buildRequest token { method, url, body, expect } =
  Http.request
    { method = method
    , headers = [ auth token ]
    , url = url
    , body = body
    , expect = expect
    , timeout = Nothing
    , withCredentials = False
    }

get : Token -> String -> Http.Expect a -> Request a
get token url expect =
  buildRequest token
    { method = "GET"
    , url = url
    , body = Http.emptyBody
    , expect = expect
    }

post : Token -> String -> Http.Body -> Http.Expect a -> Request a
post token url body expect =
  buildRequest token
    { method = "POST"
    , url = url
    , body = body
    , expect = expect
    }

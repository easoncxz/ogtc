
module OAuth.Http exposing
  ( get
  )

import Http exposing (Request)
import Json.Decode as JD exposing (Decoder)

import OAuth.Models exposing (Token(..))

get : Token -> String -> Decoder a -> Request a
get (BearerToken token) url decoder =
  Http.request
    { method = "GET"
    , headers =
      [ Http.header "Authorization" ("Bearer " ++ token) ]
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

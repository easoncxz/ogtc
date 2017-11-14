
module OAuth.Models exposing (..)

type Token
  = BearerToken String

bearerToken : String -> Token
bearerToken = BearerToken

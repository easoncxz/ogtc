
module OAuth exposing
  ( Token
  , bearerToken
  )

type Token
  = BearerToken String

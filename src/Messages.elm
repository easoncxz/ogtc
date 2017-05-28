
module Messages exposing (..)

import Time as T
import Navigation as Nav

import Models exposing (..)

type Msg
  = NoOp
  | UpdateOAuthKey String
  | UpdateAccessToken String

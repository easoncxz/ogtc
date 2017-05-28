
module Messages exposing (..)

import Time as T
import Navigation as Nav

import Models exposing (..)
import Marshallers

type Msg
  = NoOp
  | UpdateOAuthKey String
  | UpdateAccessToken String
  | QueryTasklists
  | ReceiveQueryTasklists Marshallers.ListGTaskLists
  | SelectTaskList ZTaskList


module Messages exposing (..)

import Time as T
import Navigation as Nav
import Material

import Models exposing (..)
import Marshallers

type Msg
  = NoOp
  | UpdateOAuthKey String
  | UpdateAccessToken String
  | QueryTasklists
  | ReceiveQueryTasklists Marshallers.ListGTaskLists
  | SelectTaskList ZTaskList
  | ReceiveQueryTasks Marshallers.ListGTasks
  | SetOAuthClientId (Maybe String)
  | RequestOAuthClientId
  | ReceiveOAuthClientId (Maybe String)
  | ReceiveOAuthAccessToken (Maybe String)
  | Mdl (Material.Msg Msg)

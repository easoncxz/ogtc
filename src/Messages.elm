
module Messages exposing (..)

import Navigation
import Material

import Models exposing (ZTaskList)
import Marshallers

type Msg
  = NoOp
  | Mdl (Material.Msg Msg)
  | UpdateOAuthClientId String
  | UpdateLocation Navigation.Location
  | AuthPageMsg AuthPageMsg
  | HomePageMsg HomePageMsg

type AuthPageMsg
  = ReceiveOAuthAccessToken (Maybe String)

type HomePageMsg
  = Logout
  | ReceiveQueryTasklists Marshallers.ListGTaskLists
  | SelectTaskList ZTaskList
  | ReceiveQueryTasks
      String  -- tasklist id
      Marshallers.ListGTasks

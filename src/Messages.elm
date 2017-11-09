
module Messages exposing (..)

import Material

import Models exposing (ZTaskList)
import Marshallers

type Msg
  = NoOp
  | Mdl (Material.Msg Msg)
  | ReceiveOAuthClientId (Maybe String)
  | AuthPageMsg AuthPageMsg
  | HomePageMsg HomePageMsg

type AuthPageMsg
  = UpdateOAuthClientId String
  | ReceiveOAuthAccessToken (Maybe String)

type HomePageMsg
  = Logout
  | ReceiveQueryTasklists Marshallers.ListGTaskLists
  | SelectTaskList ZTaskList
  | ReceiveQueryTasks
      String  -- tasklist id
      Marshallers.ListGTasks

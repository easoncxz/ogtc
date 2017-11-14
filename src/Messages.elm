
module Messages exposing (..)

import Navigation
import Material

import Models exposing (ZTaskList)
import GoogleTasks.Models as GoogleModels

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
  | ReceiveQueryTasklists GoogleModels.ListGTaskLists
  | SelectTaskList ZTaskList
  | ReceiveQueryTasks
      String  -- tasklist id
      GoogleModels.ListGTasks

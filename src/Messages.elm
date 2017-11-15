
module Messages exposing (..)

import Navigation
import Material

import Models exposing (ZTaskList)
import GoogleTasks.Models as GoogleModels exposing
  ( ListGTasks
  , ListGTaskLists
  )

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
  | ReceiveQueryTasklists ListGTaskLists
  | SelectTaskList ZTaskList
  | ReceiveQueryTasks String ListGTasks
  | UpdateNewTaskListTitle String
  | CreateNewTaskList

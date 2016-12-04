
module Messages exposing (..)

import Time as T
import Navigation as Nav

import Models exposing (..)

type Msg
  = NoOp
  | UpdateTaskListList ListGTaskLists
  | UpdateOAuthKey String
  | UpdateAccessToken String
  | MakeRoll
  | ReadRoll Int
  | RefreshClock
  | UpdateTime T.Time
  | ToggleClockEnabled
  | UpdateOneLocalStorage String
  | FetchLocalStorage

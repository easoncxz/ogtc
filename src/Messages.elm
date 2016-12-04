
module Messages exposing (..)

import Navigation as Nav

import Models exposing (..)

type Msg
  = NoOp
  | UpdateTaskListList ListGTaskLists
  | UpdateOAuthKey String
  | UpdateAccessToken String

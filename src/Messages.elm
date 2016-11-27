
module Messages exposing (..)

import Navigation as Nav

import Models exposing (..)

type Msg
  = NoOp
  | CreateTaskList String
  | DeleteTaskList ZTaskListId
  | SwitchToTaskList ZTaskListId
  | CreateTask String
  | DeleteTask ZTaskId
  | UpdateTaskListPrompt String
  | UpdateTaskPrompt String
  | UpdateOAuthKey String
  | UpdateOAuthSecret String
  | UpdateLocation Nav.Location
  | UpdateNextUrl String

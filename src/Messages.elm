
module Messages exposing (..)

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


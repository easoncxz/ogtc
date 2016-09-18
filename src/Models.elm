
module Models exposing (..)

type alias Model =
  { taskList   : ZTaskList
  , taskPrompt : String
  }

type alias ZTask =
  { title  : String
  , status : TaskStatus
  }

type alias ZTaskList =
  { name : String
  , tasks : List ZTask
  }

type Id a = Id String

type TaskStatus
  = NeedsAction
  | Completed

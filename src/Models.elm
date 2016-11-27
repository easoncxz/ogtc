
module Models exposing (..)

type alias Model =
  { taskLists  : List ZTaskList
  , currentTaskList : Maybe ZTaskListId
  , taskListPrompt : String
  , taskPrompt : String
  , nextIds : NextIds
  , oauthKey : String
  , oauthSecret : String
  }

type ZTaskId = ZTaskId Int
type alias ZTask =
  { title  : String
  , status : TaskStatus
  , id     : ZTaskId
  }

type ZTaskListId = ZTaskListId Int
type alias ZTaskList =
  { name  : String
  , tasks : List ZTask
  , id    : ZTaskListId
  }

type alias NextIds =
  { task : Int
  , taskList : Int
  }

type TaskStatus
  = NeedsAction
  | Completed

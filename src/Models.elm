
module Models exposing (..)

import Navigation as Nav

type alias Model =
  { taskLists  : List ZTaskList
  , currentTaskList : Maybe ZTaskListId
  , taskListPrompt : String
  , taskPrompt : String
  , nextIds : NextIds
  , oauthKey : String
  , oauthSecret : String
  , location : Nav.Location
  , nextUrl : String
  , accessToken : Maybe String
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

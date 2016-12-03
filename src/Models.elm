
module Models exposing (..)

import Date exposing (Date)
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

type alias GTaskLink =
  { type_       : String
  , description : String
  , link        : String
  }

type alias GTask =
  { kind      : String
  , id        : String
  , etag      : String
  , title     : String
  , updated   : Date
  , selfLink  : String
  , parent    : Maybe String
  , position  : String
  , notes     : Maybe String
  , status    : TaskStatus
  , due       : Maybe Date
  , completed : Maybe Date
  , deleted   : Bool
  , hidden    : Bool
  , links     : Maybe (List GTaskLink)
  }

type alias ListGTasks =
  { kind          : String
  , etag          : String
  , nextPageToken : Maybe String
  , items         : List GTask
  }

type alias GTaskList =
  { kind     : String
  , id       : String
  , etag     : String
  , title    : String
  , selfLink : String
  , updated  : Date
  }

type alias ListGTaskLists =
  { kind          : String
  , etag          : String
  , nextPageToken : Maybe String
  , items         : List GTaskList
  }

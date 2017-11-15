
module GoogleTasks.Models exposing
  ( TaskStatus(..)
  , GTask
  , GTaskList
  , GTaskLink
  , ListGTasks
  , ListGTaskLists
  )

import Date exposing (Date)

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
  , etag      : Maybe String
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
  , etag          : Maybe String
  , nextPageToken : Maybe String
  , items         : List GTask
  }

type alias GTaskList =
  { kind     : String
  , id       : String
  , etag     : Maybe String
  , title    : String
  , selfLink : String
  , updated  : Date
  }

type alias ListGTaskLists =
  { kind          : String
  , etag          : Maybe String
  , nextPageToken : Maybe String
  , items         : List GTaskList
  }

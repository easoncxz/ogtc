
module GoogleTasks.Models exposing
  ( TaskStatus(..)
  , GTask
  , GTaskList
  , GTaskLink
  , ListGTasks
  , ListGTaskLists
  )

import Time.DateTime as DT exposing (DateTime)

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
  , updated   : DateTime
  , selfLink  : String
  , parent    : Maybe String
  , position  : String
  , notes     : Maybe String
  , status    : TaskStatus
  , due       : Maybe DateTime
  , completed : Maybe DateTime
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
  , updated  : DateTime
  }

type alias ListGTaskLists =
  { kind          : String
  , etag          : Maybe String
  , nextPageToken : Maybe String
  , items         : List GTaskList
  }

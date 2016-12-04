
module Models exposing (..)

import Time exposing (Time)
import Date exposing (Date)
import Navigation as Nav

type alias Model =
  { taskLists  : List GTaskList
  , currentTaskList : Maybe String
  , oauthKey : String
  , accessToken : Maybe String
  , dice : Int
  , clockEnabled : Bool
  , time : Maybe Time
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

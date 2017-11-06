
module Models exposing (..)

import Material
import Navigation as Nav

import Marshallers exposing (GTask, GTaskList)

type alias Model =
  { taskLists  : Maybe (List ZTaskList)
  , currentTaskList : Maybe ZTaskList
  , currentTask : Maybe GTask
  , oauthKey : Maybe String
  , accessToken : Maybe String
  , mdl : Material.Model
  }

type alias ZTaskList =
  { meta: GTaskList
  , tasks: Maybe (List GTask)
  }

fromGTaskList : GTaskList -> ZTaskList
fromGTaskList gTaskList =
  { meta = gTaskList
  , tasks = Nothing
  }

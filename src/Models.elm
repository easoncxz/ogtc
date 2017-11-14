
module Models exposing (..)

import Material
import Navigation as Nav

import GoogleTasks.Models exposing (GTask, GTaskList)

type AppPage
  = AuthPage
  | HomePage HomePageModel

type alias HomePageModel =
  { accessToken : String
  , taskLists  : Maybe (List ZTaskList)
  , currentTaskList : Maybe String
  }

type alias Model =
  { mdl : Material.Model
  , oauthClientId : String
  , page : AppPage
  , location : Nav.Location
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

findZTaskListById : Maybe String -> Maybe (List ZTaskList) -> Maybe ZTaskList
findZTaskListById maybeListId maybeTaskLists =
  case (maybeListId, maybeTaskLists) of
    (Just id, Just lists) ->
      List.head <|
        List.filter
          (\l -> l.meta.id == id)
          lists
    _ ->
      Nothing

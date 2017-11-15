
module Models exposing (..)

import Material
import Navigation as Nav

import GoogleTasks.Models exposing (GTask, GTaskList)
import GoogleTasks.RestApi as RestApi

type AppPage
  = AuthPage
  | HomePage HomePageModel

type alias HomePageModel =
  { api : RestApi.Client
  , taskLists  : Maybe (List ZTaskList)
  , currentTaskList : Maybe String
  , newTaskListTitle : String
  }

type alias Model =
  { mdl : Material.Model
  , location : Nav.Location
  , page : AppPage
  , oauthClientId : String
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

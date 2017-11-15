
module GoogleTasks.RestApi exposing (..)

import Http exposing (Request)

import OAuth.Models exposing (Token)
import OAuth.Http as OHttp

import GoogleTasks.Decoders as GD
import GoogleTasks.Models exposing
  ( GTask
  , GTaskList
  , GTaskLink
  , ListGTasks
  , ListGTaskLists
  )

type alias TaskListsApi =
  { list : Request ListGTaskLists
  , get : String -> Request GTaskList
  }

type alias TasksApi =
  { list : String -> Request ListGTasks
  -- , more
  }

type alias Client =
  { taskLists : TaskListsApi
  , tasks : TasksApi
  }

makeClient : Token -> Client
makeClient token =
  { taskLists = makeTaskListsApi token
  , tasks = makeTasksApi token
  }

makeTaskListsApi : Token -> TaskListsApi
makeTaskListsApi token =
  let
    url relative =
      "https://www.googleapis.com/tasks/v1/users/@me/lists" ++ relative
  in
    { list =
        OHttp.get token (url "") GD.listGTaskLists
    , get = \id ->
        OHttp.get token (url ("/" ++ id)) GD.gTaskList
    }

makeTasksApi : Token -> TasksApi
makeTasksApi token =
  { list = \taskListId ->
      OHttp.get
        token
        ("https://www.googleapis.com/tasks/v1/lists/"
          ++ {- Http.encodeUri -} taskListId
          ++ "/tasks")
        GD.listGTasks
  -- , more
  }

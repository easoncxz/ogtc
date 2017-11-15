
module GoogleTasks.RestApi exposing (..)

import Http exposing (Request)

import OAuth.Models exposing (Token)
import OAuth.Http as OHttp

import GoogleTasks.Encoders as GE
import GoogleTasks.Decoders as GD
import GoogleTasks.Models exposing
  ( GTask
  , GTaskList
  , GTaskLink
  , ListGTasks
  , ListGTaskLists
  , TransientGTaskList
  )

type alias TaskListsApi =
  { list : Request ListGTaskLists
  , get : String -> Request GTaskList
  , insert : TransientGTaskList -> Request GTaskList
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
        OHttp.get token (url "")
          (Http.expectJson GD.listGTaskLists)
    , get = \id ->
        OHttp.get token (url ("/" ++ id))
          (Http.expectJson GD.gTaskList)
    , insert = \transient ->
        OHttp.post token (url "")
          (Http.jsonBody <| GE.transientGTask transient)
          (Http.expectJson GD.gTaskList)
    }

makeTasksApi : Token -> TasksApi
makeTasksApi token =
  { list = \taskListId ->
      OHttp.get
        token
        ("https://www.googleapis.com/tasks/v1/lists/"
          ++ {- Http.encodeUri -} taskListId
          ++ "/tasks")
        (Http.expectJson GD.listGTasks)
  -- , more
  }

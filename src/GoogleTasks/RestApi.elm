
module GoogleTasks.RestApi exposing (..)

import Http exposing (Request)

import OAuth.Models exposing (Token)
import OAuth.Http as OHttp

import GoogleTasks.Decoders as GDecoders
import GoogleTasks.Models exposing
  ( GTask
  , GTaskList
  , GTaskLink
  , ListGTasks
  , ListGTaskLists
  )

type alias TaskListsApi =
  { list : Request ListGTaskLists
  }

taskListsApi : Token -> TaskListsApi
taskListsApi token =
  { list =
      OHttp.get
        token
        "https://www.googleapis.com/tasks/v1/users/@me/lists"
        GDecoders.listGTaskLists
  }

type alias TasksApi =
  { list : String -> Request ListGTasks
  }

tasksApi : Token -> TasksApi
tasksApi token =
  { list = \taskListId ->
      OHttp.get
        token
        ("https://www.googleapis.com/tasks/v1/lists/"
          ++ {- Http.encodeUri -} taskListId
          ++ "/tasks")
        GDecoders.listGTasks
  }

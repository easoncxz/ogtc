
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
  -- , more
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
  { list =
      OHttp.get
        token
        "https://www.googleapis.com/tasks/v1/users/@me/lists"
        GDecoders.listGTaskLists
  -- , more
  }

makeTasksApi : Token -> TasksApi
makeTasksApi token =
  { list = \taskListId ->
      OHttp.get
        token
        ("https://www.googleapis.com/tasks/v1/lists/"
          ++ {- Http.encodeUri -} taskListId
          ++ "/tasks")
        GDecoders.listGTasks
  -- , more
  }


module GoogleTasks.Decoders exposing
  ( taskStatus
  , gTask
  , gTaskList
  , gTaskLink
  , listGTasks
  , listGTaskLists
  )

import Json.Decode as JD exposing
  (Decoder, field, string, int, field, bool)
import Json.Decode.Pipeline as JDP exposing
  (required, optional)

import GoogleTasks.Models exposing
  ( TaskStatus(..)
  , GTask
  , GTaskList
  , GTaskLink
  , ListGTasks
  , ListGTaskLists
  )
import Decoders exposing
  ( must, maybe, date )

taskStatus : String -> Decoder TaskStatus
taskStatus s =
  case s of
    "needsAction" ->
      JD.succeed NeedsAction
    "completed" ->
      JD.succeed Completed
    bad ->
      JD.fail <| "Unrecognizable task status: " ++ bad

gTaskLink : Decoder GTaskLink
gTaskLink =
  JD.map3 GTaskLink
    (field "type" string)
    (field "description" string)
    (field "link" string)

gTask : Decoder GTask
gTask =
  JD.succeed GTask
    |> required "kind" (string
      |> JD.andThen (must ((==) "tasks#task")))
    |> required "id" string
    |> maybe "etag" string
    |> required "title" string
    |> required "updated" date
    |> required "selfLink" string
    |> maybe "parent" string
    |> required "position" string
    |> maybe "notes" string
    |> required "status" (string |> JD.andThen taskStatus)
    |> maybe "due" date
    |> maybe "completed" date
    |> optional "deleted" bool False
    |> optional "hidden" bool False
    |> maybe "links" (JD.list gTaskLink)

listGTasks : Decoder ListGTasks
listGTasks =
  JD.succeed ListGTasks
    |> required "kind" (string
      |> JD.andThen (must ((==) "tasks#tasks")))
    |> maybe "etag" string
    |> maybe "nextPageToken" string
    |> required "items" (JD.list gTask)

gTaskList : Decoder GTaskList
gTaskList =
  JD.succeed GTaskList
    |> required "kind" (string
      |> JD.andThen (must ((==) "tasks#taskList")))
    |> required "id" string
    |> maybe "etag" string
    |> required "title" string
    |> required "selfLink" string
    |> required "updated" date

listGTaskLists : Decoder ListGTaskLists
listGTaskLists =
  JD.succeed ListGTaskLists
    |> required "kind" (string
      |> JD.andThen (must ((==) "tasks#taskLists")))
    |> maybe "etag" string
    |> maybe "nextPageToken" string
    |> required "items" (JD.list gTaskList)

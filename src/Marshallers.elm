
module Marshallers exposing (..)

import Date
import Json.Decode as JD exposing
  (Decoder, field, string, int, field, bool)
import Json.Decode.Pipeline as JDP exposing (required, optional)

import Models exposing
  ( GTaskLink
  , GTask
  , ListGTasks
  , GTaskList
  , ListGTaskLists
  , TaskStatus(..)
  )

maybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybe key decoder =
  optional key (JD.map Just decoder) Nothing

must : (a -> Bool) -> a -> Decoder a
must isAcceptable a =
  if a |> isAcceptable then
    JD.succeed a
  else
    JD.fail ("The value (" ++ toString a ++
      ") is considered not acceptable here")

date : Decoder Date.Date
date =
  let
    dateFromString : String -> Decoder Date.Date
    dateFromString s =
      case Date.fromString s of
        Ok d ->
          JD.succeed d
        Err msg ->
          JD.fail msg
  in
    JD.string |> JD.andThen dateFromString

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
    |> required "etag" string
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
    |> required "etag" string
    |> maybe "nextPageToken" string
    |> required "items" (JD.list gTask)

gTaskList : Decoder GTaskList
gTaskList =
  JD.succeed GTaskList
    |> required "kind" (string
      |> JD.andThen (must ((==) "tasks#tasklist")))
    |> required "id" string
    |> required "etag" string
    |> required "title" string
    |> required "selfLink" string
    |> required "updated" date

listGTaskLists : Decoder ListGTaskLists
listGTaskLists =
  JD.succeed ListGTaskLists
    |> required "kind" (string
      |> JD.andThen (must ((==) "tasks#taskLists")))
    |> required "etag" string
    |> maybe "nextPageToken" string
    |> required "items" (JD.list gTaskList)

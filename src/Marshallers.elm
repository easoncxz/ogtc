
module Marshallers exposing (..)

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
import MoreDecoders as MD exposing (must, date)

gTaskLink : Decoder GTaskLink
gTaskLink =
  JD.map3 GTaskLink
    (field "type" string)
    (field "description" string)
    (field "link" string)

taskStatus : String -> Decoder TaskStatus
taskStatus s =
  case s of
    "needsAction" ->
      JD.succeed NeedsAction
    "completed" ->
      JD.succeed Completed
    bad ->
      JD.fail <| "Unrecognizable task status: " ++ bad

maybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybe key decoder =
  optional key (JD.map Just decoder) Nothing

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

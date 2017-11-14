
module GoogleTasks.Encoders exposing (..)

import Time.DateTime as DT
import Json.Encode as JE

import GoogleTasks.Models exposing
  ( TaskStatus(..)
  , GTask
  , GTaskList
  , GTaskLink
  , ListGTasks
  , ListGTaskLists
  )

encodeDate : DT.DateTime -> JE.Value
encodeDate = DT.toISO8601 >> JE.string

taskStatus : TaskStatus -> JE.Value
taskStatus s =
  case s of
    NeedsAction ->
      JE.string "needsAction"
    Completed ->
      JE.string "completed"

gTaskLink : GTaskLink -> JE.Value
gTaskLink l =
  JE.object
    [ ("type", JE.string l.type_)
    , ("description", JE.string l.description)
    , ("link", JE.string l.link)
    ]

gTask : GTask -> JE.Value
gTask t =
  JE.object <|
    [ ("kind", JE.string t.kind)
    , ("id", JE.string t.id)
    ] ++
    (case t.etag of
      Nothing ->
        []
      Just e ->
        [ ("etag", JE.string e) ]
    ) ++
    [ ("title", JE.string t.title)
    , ("updated", encodeDate t.updated)
    , ("selfLink", JE.string t.selfLink)
    ] ++
    (case t.parent of
      Nothing ->
        []
      Just id ->
        [ ("parent", JE.string id) ]
    ) ++
    [ ("position", JE.string t.position)
    ] ++
    (case t.notes of
      Nothing ->
        []
      Just n ->
        [ ("notes", JE.string n) ]
    ) ++
    [ ("status", taskStatus t.status)
    ] ++
    (case t.due of
      Nothing ->
        []
      Just d ->
        [ ("due", encodeDate d) ]
    ) ++
    (case t.completed of
      Nothing ->
        []
      Just d ->
        [ ("completed", encodeDate d) ]
    ) ++
    [ ("deleted", JE.bool t.deleted)
    , ("hidden", JE.bool t.hidden)
    ] ++
    (case t.links of
      Nothing ->
        []
      Just links ->
        [ ("links", JE.list (List.map gTaskLink links)) ]
    )

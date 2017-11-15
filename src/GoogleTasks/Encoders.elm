
module GoogleTasks.Encoders exposing (..)

import Json.Encode as JE

import Encoders
import GoogleTasks.Models exposing
  ( TaskStatus(..)
  , GTask
  , GTaskList
  , GTaskLink
  , ListGTasks
  , ListGTaskLists
  )

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
    , ("updated", Encoders.date t.updated)
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
        [ ("due", Encoders.date d) ]
    ) ++
    (case t.completed of
      Nothing ->
        []
      Just d ->
        [ ("completed", Encoders.date d) ]
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

listGTasks : ListGTasks -> JE.Value
listGTasks l =
  JE.object <|
    [ ("kind", JE.string l.kind)
    ] ++
    (case l.etag of
      Nothing ->
        []
      Just e ->
        [ ("etag", JE.string e) ]
    ) ++
    (case l.nextPageToken of
      Nothing ->
        []
      Just t ->
        [ ("nextPageToken", JE.string t) ]
    ) ++
    [ ("items", JE.list (List.map gTask l.items))
    ]

gTaskList : GTaskList -> JE.Value
gTaskList l =
  JE.object <|
    [ ("kind", JE.string l.kind)
    , ("id", JE.string l.id)
    ] ++
    (case l.etag of
      Nothing ->
        []
      Just e ->
        [ ("etag", JE.string e) ]
    ) ++
    [ ("title", JE.string l.title)
    , ("selfLink", JE.string l.selfLink)
    , ("updated", Encoders.date l.updated)
    ]

listGTaskLists : ListGTaskLists -> JE.Value
listGTaskLists l =
  JE.object <|
    [ ("kind", JE.string l.kind)
    ] ++
    (case l.etag of
      Nothing ->
        []
      Just e ->
        [ ("etag", JE.string e) ]
    ) ++
    (case l.nextPageToken of
      Nothing ->
        []
      Just t ->
        [ ("nextPageToken", JE.string t) ]
    ) ++
    [ ("items", JE.list (List.map gTaskList l.items))
    ]

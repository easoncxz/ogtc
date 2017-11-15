
module GoogleTasks.Encoders exposing (..)

import Json.Encode as JE

import Encoders as Enc
import GoogleTasks.Models exposing
  ( TaskStatus(..)
  , GTask
  , GTaskList
  , GTaskLink
  , ListGTasks
  , ListGTaskLists
  , TransientGTaskList
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
  Enc.object <|
    [ Enc.justaField "kind" JE.string t.kind
    , Enc.justaField "id" JE.string t.id
    , Enc.maybeField "etag" JE.string t.etag
    , Enc.justaField "title" JE.string t.title
    , Enc.justaField "updated" Enc.date t.updated
    , Enc.justaField "selfLink" JE.string t.selfLink
    , Enc.maybeField "parent" JE.string t.parent
    , Enc.justaField "position" JE.string t.position
    , Enc.maybeField "notes" JE.string t.notes
    , Enc.justaField "status" taskStatus t.status
    , Enc.maybeField "due" Enc.date t.due
    , Enc.maybeField "completed" Enc.date t.completed
    , Enc.justaField "deleted" JE.bool t.deleted
    , Enc.justaField "hidden" JE.bool t.hidden
    , Enc.maybeField "links" (JE.list << List.map gTaskLink) t.links
    ]

listGTasks : ListGTasks -> JE.Value
listGTasks l =
  Enc.object <|
    [ Enc.justaField "kind" JE.string l.kind
    , Enc.maybeField "etag" JE.string l.etag
    , Enc.maybeField "nextPageToken" JE.string l.nextPageToken
    , Enc.justaField "items" JE.list (List.map gTask l.items)
    ]

gTaskList : GTaskList -> JE.Value
gTaskList l =
  Enc.object <|
    [ Enc.justaField "kind" JE.string l.kind
    , Enc.justaField "id" JE.string l.id
    , Enc.maybeField "etag" JE.string l.etag
    , Enc.justaField "title" JE.string l.title
    , Enc.justaField "selfLink" JE.string l.selfLink
    , Enc.justaField "updated" Enc.date l.updated
    ]

transientGTask : TransientGTaskList -> JE.Value
transientGTask tl =
  JE.object [ ("title", JE.string tl.title) ]

listGTaskLists : ListGTaskLists -> JE.Value
listGTaskLists l =
  Enc.object <|
    [ Enc.justaField "kind" JE.string l.kind
    , Enc.maybeField "etag" JE.string l.etag
    , Enc.maybeField "nextPageToken" JE.string l.nextPageToken
    , Enc.justaField "items" JE.list (List.map gTaskList l.items)
    ]

module GoogleTasks.DecoderTests exposing (all)

import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Time.DateTime as DT
import Test exposing (..)
import Expect

import GoogleTasks.Models exposing (TaskStatus(..))
import GoogleTasks.Encoders as GEncoders
import GoogleTasks.Decoders as GDecoders

all : Test
all = describe "GoogleTasks Decoders"
  [ describe "GTaskLink decoder"
    [ test "simple case" <| \() ->
      let
        raw =
          """
          { "type": "email"
          , "description": "This is a free-form field"
          , "link": "https://google.com"
          }
          """
      in case JD.decodeString GDecoders.gTaskLink raw of
        Ok l ->
          Expect.equal "https://google.com" l.link
        Err _ ->
          Expect.fail "parsing URL field failed"
    ]
  , describe "GTask decoder"
    [ test "one real case" <| \() ->
      let
        raw =
          """
            {
              "kind": "tasks#task",
              "id": "my-id",
              "etag": "my-etag",
              "title": "task-title",
              "updated": "2016-08-15T08:00:23.000Z",
              "selfLink": "https://www.googleapis.com/tasks/v1/lists/not-even-an-id",
              "parent": "parent-task-id",
              "position": "pos-abcd",
              "notes": "notes here",
              "status": "needsAction"
            }
          """
      in case JD.decodeString GDecoders.gTask raw of
        Ok gtask ->
          Expect.all
            [ .id >> Expect.equal "my-id"
            , .etag >> Expect.equal (Just "my-etag")
            , .parent >> Expect.notEqual Nothing
            , .status >> Expect.equal NeedsAction
            , .updated >> DT.month >> Expect.equal 8
            , .updated >> DT.second >> Expect.equal 23
            , .updated >> DT.hour >> Expect.equal 08  -- timezone!
            ]
            gtask
        Err e ->
          Expect.fail (toString e)
    ]
  , describe "ListGTasks decoder"
    [ test "decode a ListGTasks" <| \() ->
      let
        real =
          """
            {
             "kind": "tasks#tasks",
             "etag": "my-etag",
             "items": [
               {
                 "kind": "tasks#task",
                 "id": "task id",
                 "title": "task to do",
                 "updated": "2016-11-25T01:03:25.000Z",
                 "selfLink": "https://www.googleapis.com/tasks/v1/lists/list-id/task/task-id",
                 "position": "somewhere",
                 "notes": "some sort of note",
                 "status": "needsAction"
                  }
              ]
            }
          """
      in case JD.decodeString GDecoders.listGTasks real of
        Ok lt ->
          Expect.all
            [ .nextPageToken >> Expect.equal Nothing
            , .items >> List.length >> Expect.equal 1
            , .kind >> Expect.equal "tasks#tasks"
            ]
            lt
        Err e ->
          Expect.fail e
    , test "when tasklist has no tasks" <| \() ->
      let
        real =
          -- This is really from Google's server:
          """
          {
           "kind": "tasks#tasks",
           "etag": "\\"foo/bar\\""
          }
          """
      in case JD.decodeString GDecoders.listGTasks real of
        Ok lt ->
          Expect.equal (List.length lt.items) 0
        Err e ->
          Expect.fail e
    ]
  , describe "GTaskList decoder"
    [ test "one real value" <| \() ->
      let
        real =
          """
            {
              "kind": "tasks#taskList",
              "id": "some-tasklist-id",
              "title": "some-title",
              "updated": "2017-05-26T15:18:49.000Z",
              "selfLink": "https://www.googleapis.com/tasks/v1/users/@me/lists/some-tasklist-id"
            }
          """
      in case JD.decodeString GDecoders.gTaskList real of
        Ok tl ->
          Expect.all
            [ .id >> Expect.equal "some-tasklist-id"
            , .etag >> Expect.equal Nothing
            ]
            tl
        Err e ->
          Expect.fail (toString e)
    ]
  , describe "ListGTaskLists decoder"
    [ test "simple case" <| \() ->
      let
        raw =
          """
          {
            "kind": "tasks#taskLists",
            "etag": "\\"quotes/slashes\\"",
            "items": [
              {
                "kind": "tasks#taskList",
                "id": "lowerUPPER",
                "title": "whatever",
                "updated": "2017-10-04T15:51:01.000Z",
                "selfLink": "https://www.googleapis.com/tasks/v1/users/@me/lists/lowerUPPER"
              }
            ]
          }
          """
      in case JD.decodeString GDecoders.listGTaskLists raw of
        Err e ->
          Expect.fail e
        Ok ls ->
          Expect.all
            [ .etag >> Expect.equal (Just "\"quotes/slashes\"")
            , .items >> List.length >> Expect.equal 1
            ]
            ls
    ]
  ]



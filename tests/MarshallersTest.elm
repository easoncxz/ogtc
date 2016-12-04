
module MarshallersTest exposing (..)

import Date
import Json.Decode as JD
import Test exposing (..)
import Expect

import Models exposing (TaskStatus(..))
import Marshallers as Ms

all : Test
all = describe "Marshallers"
  [ describe "GTaskLink decoder"
    [ test "simple case" <| \() ->
      case JD.decodeString
        Ms.gTaskLink
        """
          { "type": "email"
          , "description": "This is a free-form field"
          , "link": "https://google.com"
          }
        """ of
          Ok l ->
            Expect.equal "https://google.com" l.link
          Err _ ->
            Expect.fail "parsing URL field failed"
    , describe "GTask decoder"
      [ test "decode string with quotes" <| \() ->
        Expect.equal
          (Ok "It's \"real\".")
          (JD.decodeString
            JD.string
            """
              "It's \\"real\\"."
            """)
      , test "one real case" <| \() ->
        let
          real = """
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
        in
          case JD.decodeString Ms.gTask real of
            Ok gtask ->
              Expect.true
                ("one of the props are violated. gtask: " ++ toString gtask)
                (List.all
                  ((==) True)
                  (List.map
                    ((|>) gtask)
                    [ \t -> t.id == "my-id"
                    , \t -> t.parent /= Nothing
                    , \t -> t.status == NeedsAction
                    , \t -> (Date.month t.updated == Date.Aug)
                    , \t -> (Date.second t.updated == 23)
                    -- no timezone considerations here yet
                    ]))
            Err e ->
              Expect.fail (toString e)
      , test "decode a ListGTasks" <| \() ->
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
                   "etag": "another etag",
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
        in
          case JD.decodeString Ms.listGTasks real of
            Ok lt ->
              Expect.true
                "some prop didn't hold"
                (List.all
                  ((|>) lt)
                  [ \l -> l.nextPageToken == Nothing
                  , \l -> List.length l.items == 1
                  , \l -> l.kind == "tasks#tasks"
                  ])
            Err e ->
              Expect.fail (toString e)
      ]
    , describe "value enforcement" <|
      let
        good = "good"
        beGood s = s == good
      in
        [ test "rejection" <| \() ->
          case JD.decodeString
            (JD.string |> JD.andThen (Ms.must beGood))
            """\"not that good\"""" of
            Ok _ ->
              Expect.fail "It should've been rejected"
            Err reason ->
              Expect.true "the error message doesn't match" <|
                (reason |> String.contains good) &&
                (reason |> String.contains "not acceptable")
        , test "acceptance" <| \() ->
          Expect.equal
            (Ok "good")
            (JD.decodeString
              (JD.string |> JD.andThen (Ms.must beGood))
              """\"good\"""")
        ]
    , describe "decoding datetime"
      [ test "simple case" <| \() ->
        case JD.decodeString
          Ms.date
          "\"2016-11-25T01:03:25.000Z\"" of
            Ok d ->
              Expect.equal
                (Date.month d)
                Date.Nov
            Err msg ->
              Expect.fail "it shouldn't have failed"

      ]
    ]
  ]



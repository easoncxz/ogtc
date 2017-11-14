module MarshallersTest exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Time.DateTime as DT
import Test exposing (..)
import Expect

import DecoderHelpers as DH
import GoogleTasks.Models exposing (TaskStatus(..))
import GoogleTasks.Decoders as Ms

type alias TestPerson =
  { lastName : Maybe String
  }

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
                    , \t -> t.etag /= Nothing
                    , \t -> t.parent /= Nothing
                    , \t -> t.status == NeedsAction
                    , \t -> (DT.month t.updated == 8)
                    , \t -> (DT.second t.updated == 23)
                    -- no timezone considerations here yet
                    ]))
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
    , describe "GTaskList decoder"
      [ test "one real value" <| \() ->
        let
          real = """{
            "kind": "tasks#taskList",
            "id": "some-tasklist-id",
            "title": "some-title",
            "updated": "2017-05-26T15:18:49.000Z",
            "selfLink": "https://www.googleapis.com/tasks/v1/users/@me/lists/some-tasklist-id"
          }"""
        in
          case JD.decodeString Ms.gTaskList real of
            Ok tl ->
              Expect.true
                "some prop didn't hold"
                (List.all
                  ((|>) tl)
                  [ \tl -> tl.id == "some-tasklist-id"
                  , \tl -> tl.etag == Nothing
                  ])
            Err e ->
              Expect.fail (toString e)
      ]
    , describe "decoding datetime"
      [ test "simple case" <| \() ->
        case JD.decodeString
          DH.date
          "\"2016-11-25T01:03:25.000Z\"" of
            Ok d ->
              Expect.equal 11 (DT.month d)
            Err msg ->
              Expect.fail "it shouldn't have failed"

      ]
    , describe "DecoderHelpers"
      [ describe "must"
        [ test "rejection" <| \() ->
          case JD.decodeString
            (JD.string |> JD.andThen (DH.must ((==) "good")))
            """\"not that good\"""" of
            Ok _ ->
              Expect.fail "It should've been rejected"
            Err reason ->
              Expect.true "the error message doesn't match" <|
                (reason |> String.contains "good") &&
                (reason |> String.contains "not acceptable")
        , test "acceptance" <| \() ->
          Expect.equal
            (Ok "good")
            (JD.decodeString
              (JD.string |> JD.andThen (DH.must ((==) "good")))
              """\"good\"""")
        ]
      , describe "maybe"
        [ test "there is right" <| \() ->
          case JD.decodeString
                (JDP.decode TestPerson
                  |> DH.maybe "lastName" JD.string)
                """{"lastName": "Doe"}""" of
            Ok person ->
              Expect.equal
                person.lastName
                (Just "Doe")
            Err e ->
              Expect.fail (toString e)
        , test "there is something wrong" <| \() ->
          case JD.decodeString
                (JDP.decode TestPerson
                  |> DH.maybe "lastName" JD.string)
                """{"lastName": 123}""" of
            Ok person ->
              Expect.fail "decode should error-out but didn't"
            Err e ->
              Expect.pass
        , test "there is nothing, as allowed" <| \() ->
          case JD.decodeString
                (JDP.decode TestPerson
                  |> DH.maybe "lastName" JD.string)
                """{}""" of
            Ok person ->
              Expect.equal Nothing person.lastName
            Err e ->
              Expect.fail (toString e)
        ]
      , describe "date"
        [ test "respect ISO-8601 timezone marker" <| \() ->
            let
              utcStr = "\"2017-11-14T05:55:24.675Z\""
              localStr = "\"2017-11-14T18:55:24.675+13:00\""
              -- ISO-8601 support seems flaky. "+13" doesn't work
            in
              case JD.decodeString DH.date utcStr of
                Ok utc ->
                  case JD.decodeString DH.date localStr of
                    Ok local ->
                      Expect.equal utc local
                    Err e ->
                      Expect.fail e
                Err e ->
                  Expect.fail e
        ]
      ]
    ]
  ]



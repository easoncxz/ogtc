
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
      ]
    ]
  ]



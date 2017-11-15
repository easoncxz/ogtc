module DecodersTest exposing (all)

import Test exposing (..)
import Expect

import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Time.DateTime as DT

import Decoders as DH

type alias TestPerson =
  { lastName : Maybe String
  }

all : Test
all = describe "Decoders"
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
    [ test "decodes UTC time" <| \() ->
      case JD.decodeString
          DH.date
          "\"2017-11-14T10:05:08.440Z\"" of
        Ok d ->
          Expect.equal 10 (DT.hour d)
        Err e ->
          Expect.fail e
    , test "decodes non-UTC time" <| \() ->
      case JD.decodeString
          DH.date
          "\"2017-11-14T23:05:08.440+13:00\"" of
        Ok d ->
          -- Note that 23:00 at UTC+13 shows up as 10:00
          Expect.equal 10 (DT.hour d)
        Err e ->
          Expect.fail e
    ]
  ]

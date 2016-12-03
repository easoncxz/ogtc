
module JsonCoding exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JDP exposing
  ( required
  , optional
  , hardcoded
  )

import Test exposing (..)
import Expect

import MoreDecoders exposing (must)

type UPoint number
  = UPoint
    { x : number
    , y : number
    }

type alias APoint number =
  { x : number
  , y : number
  }

type alias ALarge number =
  { a1 : number
  , a2 : number
  , a3 : number
  , a4 : number
  , a5 : number
  , a6 : number
  , a7 : number
  , a8 : number
  , a9 : number
  , a10 : Maybe number
  , a11 : number
  , a12 : number
  }

-- Nesting type aliases
-- "A line can be defined by two points."
type alias ALine number =
  { a : APoint number
  , b : APoint number
  }

decodePoint : JD.Decoder (APoint Int)
decodePoint =
  JD.succeed APoint
    |> required "x" JD.int
    |> required "y" JD.int

-- See how it composes!
decodeALine : JD.Decoder (ALine Int)
decodeALine =
  JD.succeed ALine
    |> required "a" decodePoint
    |> required "b" decodePoint

decodeALargeOptionField : JD.Decoder (ALarge Int)
decodeALargeOptionField =
  JD.succeed ALarge
    |> required "a1" JD.int
    |> required "a2" JD.int
    |> required "a3" JD.int
    |> required "a4" JD.int
    |> required "a5" JD.int
    |> required "a6" JD.int
    |> required "a7" JD.int
    |> required "a8" JD.int
    |> required "a9" JD.int
    |> optional "a10" (JD.map Just JD.int) Nothing
    |> required "a11" JD.int
    |> required "a12" JD.int

all : Test
all =
  describe "JSON coding"
    [ describe "JSON decoding"
      [ describe "decoding primitives/builtins"
        [ test "good int" <| \() ->
          let
            json = """42"""
          in
            Expect.equal
              (Ok 42)
              (JD.decodeString JD.int json)
        , test "bad int" <| always <|
          let
            r = JD.decodeString JD.int "Bad thing"
          in
            case r of
              Ok i ->
                Expect.fail <| toString i ++ " is not an int"
              Err _ ->
                Expect.pass
        , test "good [int]" <| always <|
          Expect.equal
            (Ok [1, 2, 3])
            (JD.decodeString (JD.list JD.int) "[1, 2, 3]")
        ]
      , describe "decoding custom records, using Json.Decode[.Pipeline]"
        [ test "simple object" <| \() ->
          let
            decodeX = JD.field "x" JD.int
            decodeY = JD.field "y" JD.int
            decodePoint = JD.map2
              APoint
              decodeX
              decodeY
          in
            Expect.equal
              (Ok (APoint 1 2))
              (JD.decodeString
                decodePoint
                """{ "x": 1, "y": 2   }""")
        , test "object of alias, using Json.Decode.Pipeline" <| \() ->
          Expect.equal
            (Ok (APoint 1 2))
            (JD.decodeString
                decodePoint
                """{"x"   :1,"y"   :  2}""")
        , test "union-type objects" <| \() ->
          let
            p = UPoint { x = 1, y = 2 }
            point x y = UPoint { x = x, y = y }
            q = point 1 2
          in
            Expect.equal p q
        , test "object of union, using Json.Decode.Pipeline" <| \() ->
          let
            point x y = UPoint { x = x, y = y }
            decodePoint =
              JD.succeed point
                |> required "x" JD.int
                |> required "y" JD.int
          in
            Expect.equal
              (Ok (UPoint { x = 1, y = 2 }))
              (JD.decodeString
                decodePoint
                """{ "x": 1, "y": 2 }""")
        ]
      , describe "optionality in JD.Pipeline"
        [ test "key and value both missing is ok" <| \() ->
            Expect.equal
              (Ok
                { a1 = 1 , a2 = 1 , a3 = 1 , a4 = 1 , a5 = 1
                , a6 = 1 , a7 = 1 , a8 = 1 , a9 = 1 , a10 = Nothing
                , a11 = 1 , a12 = 1
                }) <|
              JD.decodeString
                decodeALargeOptionField
                """
                  { "a1"  : 1 , "a2"  : 1 , "a3"  : 1 , "a4"  : 1 , "a5"  : 1
                  , "a6"  : 1 , "a7"  : 1 , "a8"  : 1 , "a9"  : 1
                  , "a11" : 1 , "a12" : 1
                  }
                """
        , test "key and value both present is ok" <| \() ->
          Expect.equal
            (Ok
              { a1 = 1 , a2 = 1 , a3 = 1 , a4 = 1 , a5 = 1
              , a6 = 1 , a7 = 1 , a8 = 1 , a9 = 1 , a10 = Just 10
              , a11 = 1 , a12 = 1
              }) <|
            JD.decodeString
              decodeALargeOptionField
              """
                { "a1"  : 1 , "a2"  : 1 , "a3"  : 1 , "a4"  : 1 , "a5"  : 1
                , "a6"  : 1 , "a7"  : 1 , "a8"  : 1 , "a9"  : 1 , "a10" : 10
                , "a11" : 1 , "a12" : 1
                }
              """
        , test "key present but null-value is somehow also ok" <| \() ->
          Expect.equal
            (Ok { a1 = 1, a2 = 1, a3 = 1, a4 = 1,
              a5 = 1, a6 = 1, a7 = 1, a8 = 1,
              a9 = 1, a10 = Nothing,
              a11 = 1, a12 = 1 }) <|
            JD.decodeString
                decodeALargeOptionField
                """
                  { "a1"  : 1 , "a2"  : 1 , "a3"  : 1 , "a4"  : 1 , "a5"  : 1
                  , "a6"  : 1 , "a7"  : 1 , "a8"  : 1 , "a9"  : 1 , "a10" : null
                  , "a11" : 1 , "a12" : 1
                  }
                """
        , test "key present but value is non-null wrong-type is really not ok" <| \() ->
          case JD.decodeString
              decodeALargeOptionField
              """
                { "a1"  : 1 , "a2"  : 1 , "a3"  : 1 , "a4"  : 1 , "a5"  : 1
                , "a6"  : 1 , "a7"  : 1 , "a8"  : 1 , "a9"  : 1 , "a10" : "wat"
                , "a11" : 1 , "a12" : 1
                }
              """ of
            Ok r ->
              Expect.fail <| "value cannot be null: " ++ toString r
            Err msg ->
              Expect.pass
        ]
      , test "decoders can compose" <| \() ->
        Expect.equal
          (Ok
            { a = { x = 1 , y = 2 }
            , b = { x = 3 , y = 4 } })
          (JD.decodeString
            decodeALine
            """{"a" : {"x" : 1, "y":2}, "b": {"x":3, "y"  :4}} """)
      , describe "value enforcement" <|
        let
          good = "good"
          beGood s = s == good
        in
          [ test "rejection" <| \() ->
            case JD.decodeString
              (JD.string |> JD.andThen (must beGood))
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
                (JD.string |> JD.andThen (must beGood))
                """\"good\"""")
          ]
      ]
    ]

module GoogleTasks.EncoderTests exposing (all)

import Test exposing (..)
import Expect

import Json.Encode as JE
import Json.Decode as JD

import GoogleTasks.Decoders as GDecoders
import GoogleTasks.Encoders as GEncoders

all : Test
all = describe "GTask encoder"
  [ test "`(dec ->) enc -> dec -> enc` gives the same thing" <| \() ->
    let
      real =
        """
          {
            "kind": "tasks#task",
            "id": "ApparentlyUpperAndLowerCaseAscii",
            "etag": "\\"ThisReallyHas/SlashesAndBackSlashes\\"",
            "title": "Answer Myrtle!",
            "updated": "2017-10-04T15:51:01.000Z",
            "selfLink": "(some URL that mentions the IDs above)",
            "position": "00000000000005473837",
            "status": "completed",
            "due": "2017-07-29T00:00:00.000Z",
            "completed": "2017-10-04T15:50:53.000Z"
          }
        """  -- omitting `deleted` and `hidden`
    in case JD.decodeString GDecoders.gTask real of
        Err e ->
          Expect.fail e
        Ok gTask ->
          case JD.decodeString GDecoders.gTask <|
              JE.encode 0 <|
              GEncoders.gTask gTask of
            Err e ->
              Expect.fail e
            Ok gTaskAgain ->
              Expect.equal gTask gTaskAgain
  ]

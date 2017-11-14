
module DecoderHelpers exposing
  ( must
  , maybe
  , date
  )

import Date exposing (Date)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP

-- Please don't import ogtc-specific modules from this module.

-- | A Json.Decode.Pipeline -compatible decoder that has the
-- same semantics as Json.Decode.maybe
maybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybe key decoder =
  JDP.optional key (JD.map Just decoder) Nothing

must : (a -> Bool) -> a -> Decoder a
must isAcceptable a =
  if a |> isAcceptable then
    JD.succeed a
  else
    JD.fail ("The value (" ++ toString a ++
      ") is considered not acceptable here")

date : Decoder Date
date =
  let
    dateFromString : String -> Decoder Date
    dateFromString s =
      case Date.fromString s of
        Ok d ->
          JD.succeed d
        Err msg ->
          JD.fail msg
  in
    JD.string |> JD.andThen dateFromString



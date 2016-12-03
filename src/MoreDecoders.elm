
module MoreDecoders exposing (must, date)

import Date
import Json.Decode as D exposing (Decoder(..))

must : (a -> Bool) -> a -> Decoder a
must isAcceptable a =
  if a |> isAcceptable then
    D.succeed a
  else
    D.fail <|
      ("The value ("
      ++ toString a
      ++ ") is considered not acceptable here")

date : Decoder Date.Date
date = D.string |> D.andThen dateFromString

dateFromString : String -> Decoder Date.Date
dateFromString s =
  case Date.fromString s of
    Ok d ->
      D.succeed d
    Err msg ->
      D.fail msg


module MoreDecoders exposing (must)

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


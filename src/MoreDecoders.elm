
module MoreDecoders exposing (mustBe)

import Json.Decode as D exposing (Decoder(..))

mustBe : (a -> Result String a) -> Decoder a -> Decoder a
mustBe check decoder =
  let
    determineNextDecoder a =
      case check a of
        Ok v ->
          D.succeed v
        Err reason ->
          D.fail reason
  in
    decoder
      |> D.andThen determineNextDecoder

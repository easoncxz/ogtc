
module Encoders exposing (..)

import Time.DateTime as DT
import Json.Encode as JE

date : DT.DateTime -> JE.Value
date = DT.toISO8601 >> JE.string

listFromMaybe : Maybe a -> List a
listFromMaybe m =
  case m of
    Nothing ->
      []
    Just a ->
      [a]

-- For when some keys of the object are only *maybe* present
object : List (Maybe (String, JE.Value)) -> JE.Value
object maybes =
  JE.object <|
    List.concatMap listFromMaybe maybes

-- Just ("key", encoder src.key), except if src.key is Nothing
maybeField :
    String
    -> (a -> JE.Value)
    -> Maybe a
    -> Maybe (String, JE.Value)
maybeField key encoder src =
  Maybe.map (\field -> (key, encoder field)) src

-- Special-case of maybeField where we never have Nothing
justaField :
    String
    -> (a -> JE.Value)
    -> a    -- not `Maybe`
    -> Maybe (String, JE.Value)
justaField key encoder src =
  Just (key, encoder src)

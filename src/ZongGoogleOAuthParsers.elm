
module ZongGoogleOAuthParsers exposing (..)

import String

maybeToPair : List a -> Maybe (a, a)
maybeToPair xs =
  case xs of
    (x::y::[]) ->
      Just (x, y)
    _ ->
      Nothing

isSomething : Maybe a -> Bool
isSomething m =
  case m of
    Nothing ->
      False
    _ ->
      True

keyIs : String -> Maybe (String, a) -> Bool
keyIs good p =
  case p of
    Nothing ->
      False
    Just (k, _) ->
      k == good

type alias Fragment = String
type alias AccessToken = String

parseFragment : Fragment -> Maybe AccessToken
parseFragment fragment =
  case String.uncons fragment of
    Nothing ->
      Nothing
    Just (_, allParams) ->
      let
          keyValueStrings = String.split "&" allParams
          keyValueLists = List.map (String.split "=") keyValueStrings
          keyValueMaybePairs = List.map maybeToPair keyValueLists
          maybeFound = keyValueMaybePairs
            |> List.filter isSomething
            |> List.filter (keyIs "access_token")
            |> List.head
      in case maybeFound of
        Nothing ->
          Nothing
        Just Nothing ->
          Nothing
        Just (Just (k, token)) ->
          Just token


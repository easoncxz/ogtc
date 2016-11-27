
module ZongGoogleOAuthParsers exposing (..)

import String

snd : (a, b) -> b
snd (a, b) = b

maybeToPair : List a -> Maybe (a, a)
maybeToPair xs =
  case xs of
    (x::y::[]) ->
      Just (x, y)
    _ ->
      Nothing

checkKeyIs : a -> (a, b) -> Maybe (a, b)
checkKeyIs good (a, b) =
  if a == good then
    Just (a, b)
  else
    Nothing

type alias Fragment = String
type alias AccessToken = String

parseFragment : Fragment -> Maybe AccessToken
parseFragment fragment =
  String.uncons fragment
    |> Maybe.andThen (Just << snd)
    |> Maybe.andThen (Just << String.split "=")
    |> Maybe.andThen maybeToPair
    |> Maybe.andThen (checkKeyIs "access_token")
    |> Maybe.andThen (Just << snd)

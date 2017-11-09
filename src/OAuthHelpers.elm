
module OAuthHelpers exposing
  ( accessTokenFromLocation
  , makeAuthorizeUrl
  -- Tests:
  , parseFragment
  )

import String
import Navigation exposing (Location)

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

accessTokenFromLocation : Location -> Maybe String
accessTokenFromLocation l = parseFragment l.hash

type alias OAuthKey = String
type alias OAuthUrl = String
type alias OAuthState = String

makeAuthorizeUrl : OAuthKey -> OAuthUrl -> OAuthState -> String
makeAuthorizeUrl oauthKey url state =
    ("https://accounts.google.com/o/oauth2/v2/auth"
    ++ "?response_type=" ++ "token"
    ++ "&client_id="     ++ oauthKey
    ++ "&redirect_uri="  ++ url
    ++ "&scope="         ++ "https://www.googleapis.com/auth/tasks"
    )


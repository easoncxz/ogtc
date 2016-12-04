
import Debug
import Dom
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Navigation as Nav
import Task

import Messages exposing (Msg(..))
import Models exposing
  ( TaskStatus(..)
  , Model
  )
import OAuthHelpers exposing (accessTokenFromLocation)

undefined : () -> a
undefined _ = Debug.crash "Not implemented yet!"

main : Program Never Model Msg
main = Nav.program
  onLocationChange
  { init          = init
  , update        = update
  , subscriptions = subscriptions
  , view          = view
  }

onLocationChange : Nav.Location -> Msg
onLocationChange loc =
  case accessTokenFromLocation loc of
    Just t ->
      UpdateAccessToken t
    Nothing ->
      NoOp

init : Nav.Location -> (Model, Cmd Msg)
init loc =
  let
    model =
      { taskLists = []
      , currentTaskList = Nothing
      , oauthKey = "OAuth client key"
      , accessToken = accessTokenFromLocation loc
      }
  in
    (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    UpdateTaskListList tll ->
      undefined ()
    UpdateOAuthKey k ->
      ({ model | oauthKey = k }, Cmd.none)
    UpdateAccessToken t ->
      ({ model | accessToken = Just t }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

view : Model -> H.Html Msg
view model = H.text "hello"

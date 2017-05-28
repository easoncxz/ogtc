
port module Main exposing (main)

import Date as D
import Debug
import Dict exposing (Dict)
import Json.Decode as JD
import Navigation as Nav
import Random as R
import Task
import Time as T

import Messages exposing (Msg(..))
import Marshallers
import Models exposing (Model)
import OAuthHelpers exposing (accessTokenFromLocation)
import Views exposing (view)

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
      { taskLists = Nothing
      , currentTaskList = Nothing
      , currentTask = Nothing
      , oauthKey = Nothing
      , accessToken = accessTokenFromLocation loc
      }
  in
    (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    UpdateOAuthKey k ->
      ({ model | oauthKey = Just k }, Cmd.none)
    UpdateAccessToken t ->
      ({ model | accessToken = Just t }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

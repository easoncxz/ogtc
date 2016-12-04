
import Debug
import Navigation as Nav

import Messages exposing (Msg(..))
import Models exposing
  ( TaskStatus(..)
  , Model
  )
import OAuthHelpers exposing (accessTokenFromLocation)
import Views exposing (view)

undefined : () -> a
undefined _ = Debug.crash "Not implemented yet!"

main : Program Never Model Msg
main = Nav.program
  onLocationChange
  { init          = init
  , update        = update
  , subscriptions = always Sub.none
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
      , oauthKey = ""
      , accessToken = accessTokenFromLocation loc
      }
    cmd =
      if model.accessToken /= Nothing &&
          String.length model.oauthKey /= 0 then
        listTaskLists
      else
         Cmd.none
  in
    (model, cmd)

listTaskLists : Cmd Msg
listTaskLists = Cmd.none

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

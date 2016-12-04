
import Date as D
import Debug
import Navigation as Nav
import Random as R
import Task
import Time as T

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
      , oauthKey = ""
      , accessToken = accessTokenFromLocation loc
      , dice = 0
      , clockEnabled = False
      , time = Nothing
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

rollCmd : Cmd Msg
rollCmd = R.generate ReadRoll (R.int 1 6)

askForTime : Cmd Msg
askForTime = Task.perform UpdateTime T.now

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
    MakeRoll ->
      (model, rollCmd)
    ReadRoll n ->
      ({ model | dice = n }, Cmd.none)
    RefreshClock ->
      (model, askForTime)
    UpdateTime t ->
      ({ model | time = Just t }, Cmd.none)
    ToggleClockEnabled ->
      ({ model | clockEnabled = not model.clockEnabled }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.clockEnabled then
    T.every T.second UpdateTime
  else
    Sub.none


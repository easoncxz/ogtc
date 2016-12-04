
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
import Models exposing
  ( TaskStatus(..)
  , Model
  )
import OAuthHelpers exposing (accessTokenFromLocation)
import Views exposing (view)

port receiveLocalStorage : (JD.Value -> msg) -> Sub msg
port fetchLocalStorage : () -> Cmd msg

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
      , localStorage = Nothing
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
    UpdateOneLocalStorage s ->
      ({ model | localStorage = Just s }, Cmd.none)
    FetchLocalStorage ->
      (model, fetchLocalStorage ())

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if model.clockEnabled then
        T.every T.second UpdateTime
      else
        Sub.none
    , receiveLocalStorage <| \unsafeJson ->
      case JD.decodeValue (JD.dict JD.string) unsafeJson of
        Ok safeJson ->
          let
            key = "nonExistant243"
          in
            case Dict.get key safeJson of
              Just value ->
                UpdateOneLocalStorage value
              Nothing ->
                let
                  ignore = Debug.log "KeyError" <|
                    "key " ++ toString key ++
                    " not found in " ++
                    toString safeJson
                in
                  NoOp
        Err e ->
          let
            ignore = Debug.log "JSON decode error" e
          in
            NoOp
    ]


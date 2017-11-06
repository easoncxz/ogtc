
port module Main exposing (main)

import Date as D
import Debug
import Dict exposing (Dict)
import Http
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

port setOAuthClientId : Maybe String -> Cmd a
port requestOAuthClientId : () -> Cmd a
port receiveOAuthClientId : (Maybe String -> a) -> Sub a

port setOAuthAccessToken : Maybe String -> Cmd a
port requestOAuthAccessToken : () -> Cmd a
port receiveOAuthAccessToken : (Maybe String -> a) -> Sub a

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
    ( model
    , Cmd.batch
      [ requestOAuthClientId ()
      , case model.accessToken of
          Nothing ->
            requestOAuthAccessToken ()
          Just t ->
            setOAuthAccessToken (Just t)
      ]
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    UpdateOAuthKey k ->
      ({ model | oauthKey = Just k }, Cmd.none)
    UpdateAccessToken t ->
      ({ model | accessToken = Just t }, Cmd.none)
    QueryTasklists ->
      case model.accessToken of
        Nothing ->
          Debug.crash "We need an OAuth access token to make requests"
        Just accessToken ->
          (model, Http.send
            (\result -> case result of
                Ok listGTaskList ->
                  ReceiveQueryTasklists listGTaskList
                Err e ->
                  Debug.crash "HTTP error while getting tasklists")
            (Http.request
              { method = "GET"
              , headers =
                  [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
              , url = "https://www.googleapis.com/tasks/v1/users/@me/lists"
              , body = Http.emptyBody
              , expect = Http.expectJson Marshallers.listGTaskLists
              , timeout = Nothing
              , withCredentials = False
              }))
    ReceiveQueryTasklists listGTaskLists ->
      ( { model | taskLists = Just <|
            List.map Models.fromGTaskList listGTaskLists.items }
      , Cmd.none
      )
    SelectTaskList zTaskList ->
      ( { model | currentTaskList = Just zTaskList }
      , case model.accessToken of
          Nothing ->
            Debug.crash "We need an OAuth access token to make requests"
          Just accessToken ->
            Http.send
              (\result -> case result of
                  Ok listGTasks ->
                    ReceiveQueryTasks listGTasks
                  Err e ->
                    Debug.crash "HTTP error while getting tasks")
              (Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
                , url =
                    "https://www.googleapis.com/tasks/v1/lists/"
                    ++ zTaskList.meta.id
                    ++ "/tasks"
                , body = Http.emptyBody
                , expect = Http.expectJson Marshallers.listGTasks
                , timeout = Nothing
                , withCredentials = False
                }))
    ReceiveQueryTasks listGTasks ->
      ( { model | currentTaskList =
            Maybe.map
              (\curr ->
                { curr | tasks = Just listGTasks.items })
              model.currentTaskList }
      , Cmd.none
      )
    SetOAuthClientId oauthKey ->
      (model, setOAuthClientId oauthKey)
    RequestOAuthClientId ->
      (model, requestOAuthClientId ())
    ReceiveOAuthClientId oauthKey ->
      ({ model | oauthKey = oauthKey }, Cmd.none)
    ReceiveOAuthAccessToken t ->
      ({ model | accessToken = t }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ receiveOAuthClientId ReceiveOAuthClientId
    , receiveOAuthAccessToken ReceiveOAuthAccessToken
    ]

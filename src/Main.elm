
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
import Material
import Material.Layout as Layout

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
    tokenFromUrl = accessTokenFromLocation loc
  in
    ( { taskLists = Nothing
      , currentTaskList = Nothing
      , currentTask = Nothing
      , oauthKey = Nothing
      , accessToken = tokenFromUrl
      , mdl = Material.model
      }
    , Cmd.batch
        [ Material.init Mdl
        , requestOAuthClientId ()
        , case tokenFromUrl of
            Nothing ->
              -- try to read from localStorage instead
              requestOAuthAccessToken ()
            Just t ->
              Cmd.batch
                [ setOAuthAccessToken (Just t)
                , queryTasklists t
                ]
        ]
    )

queryTasklists : String -> Cmd Msg
queryTasklists accessToken =
  Http.send
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
      })

queryTasks : String -> Models.ZTaskList -> Cmd Msg
queryTasks accessToken zTaskList =
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
      })

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    Mdl msg_ ->
      Material.update Mdl msg_ model
    UpdateOAuthKey k ->
      ({ model | oauthKey = Just k }, Cmd.none)
    UpdateAccessToken t ->
      ({ model | accessToken = Just t }, Cmd.none)
    QueryTasklists ->
      case model.accessToken of
        Nothing ->
          Debug.crash "We need an OAuth access token to make requests"
        Just accessToken ->
          (model, queryTasklists accessToken)
    ReceiveQueryTasklists listGTaskLists ->
      ( { model | taskLists = Just <|
            List.map Models.fromGTaskList listGTaskLists.items }
      , Cmd.none
      )
    SelectTaskList zTaskList ->
      let
          (model_, cmd_) =
            update (Layout.toggleDrawer Mdl) model
      in
        ( { model_ | currentTaskList = Just zTaskList }
        , Cmd.batch
            [ cmd_
            , case model.accessToken of
                Nothing ->
                  Debug.crash "We need an OAuth access token to make requests"
                Just accessToken ->
                  queryTasks accessToken zTaskList
            ]
        )
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
    ReceiveOAuthAccessToken (Just t) ->
      ( { model | accessToken = Just t }, queryTasklists t)
    ReceiveOAuthAccessToken Nothing ->
      ( { model | accessToken = Nothing}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Material.subscriptions Mdl model
    , receiveOAuthClientId ReceiveOAuthClientId
    , receiveOAuthAccessToken ReceiveOAuthAccessToken
    ]

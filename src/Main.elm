
port module Main exposing (main)

import Date
import Debug
import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Navigation as Nav
import Task
import Material
import Material.Layout as Layout

import GoogleTasks.Decoders as Marshallers
import GoogleTasks.RestApi as RestApi
import OAuth.Models exposing (Token, bearerToken)
import OAuth.Authorization exposing (parseFragment)
import OAuth.Http as OHttp

import Models exposing (Model)
import Messages exposing (Msg(..), HomePageMsg(..), AuthPageMsg(..))
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
onLocationChange = UpdateLocation

init : Nav.Location -> (Model, Cmd Msg)
init loc =
  if loc.protocol /= "https:" then
    ( { mdl = Material.model
      , oauthClientId = ""
      , page = Models.AuthPage
      , location = loc
      } -- whatever, really
    , Nav.load
        <| "https://"
        ++ loc.hostname
        ++ loc.port_
        ++ loc.pathname
        ++ loc.hash
    )
  else let
    (page, cmd) =
      case parseFragment loc.hash of
        Nothing ->
          (Models.AuthPage, requestOAuthAccessToken ())
        Just t ->
          let
            api = RestApi.makeClient (bearerToken t)
          in
            ( Models.HomePage
              { api = api
              , taskLists = Nothing
              , currentTaskList = Nothing
              }
            , Cmd.batch
              [ setOAuthAccessToken (Just t)
              , queryTasklists api
              ]
            )
  in
    ( { mdl = Material.model
      , oauthClientId = ""
      , page = page
      , location = loc
      }
    , Cmd.batch
        [ Material.init Mdl
        , requestOAuthClientId ()
        , cmd
        ]
    )

queryTasklists : RestApi.Client -> Cmd Msg
queryTasklists api =
  Http.send
    (\result -> case result of
        Ok listGTaskList ->
          HomePageMsg <| ReceiveQueryTasklists listGTaskList
        Err e ->
          let
            _ = Debug.log "HTTP error while getting tasklists" e
          in HomePageMsg Logout)
    api.taskLists.list

queryTasks : RestApi.Client -> Models.ZTaskList -> Cmd Msg
queryTasks api zTaskList =
  Http.send
    (\result -> case result of
        Ok listGTasks ->
          HomePageMsg <| ReceiveQueryTasks zTaskList.meta.id listGTasks
        Err e ->
          let
            _ = Debug.log "HTTP error while getting tasks" e
          in HomePageMsg Logout)
    (api.tasks.list zTaskList.meta.id)

getTaskList : RestApi.TaskListsApi -> String -> Cmd Msg
getTaskList api id =
  Http.send
    (\result -> case result of
      Ok gTaskList ->
        HomePageMsg <| GotTaskList id gTaskList
      Err e ->
        Debug.log
          (toString e)
          (HomePageMsg Logout))
    (api.get id)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    Mdl msg_ ->
      Material.update Mdl msg_ model
    UpdateOAuthClientId id ->
      ({ model | oauthClientId = id }, setOAuthClientId (Just id))
    UpdateLocation loc ->
      ({ model | location = loc }, Cmd.none)
    AuthPageMsg authMsg ->
      case model.page of
        Models.HomePage _ ->
          (model, Cmd.none)
        Models.AuthPage ->
          case authMsg of
            ReceiveOAuthAccessToken Nothing ->
              (model, Cmd.none)
            ReceiveOAuthAccessToken (Just t) ->
              let api = RestApi.makeClient (bearerToken t)
              in
                ( { model | page = Models.HomePage
                    { api = api
                    , taskLists = Nothing
                    , currentTaskList = Nothing
                    }
                  }
                , queryTasklists api
                )
    HomePageMsg homeMsg ->
      case model.page of
        Models.AuthPage ->
          (model, Cmd.none)
        Models.HomePage { api, taskLists, currentTaskList } ->
          case homeMsg of
            Logout ->
              ( { model | page = Models.AuthPage }
              , Cmd.batch
                  [ setOAuthAccessToken Nothing
                  , Nav.newUrl "/"
                  ]
              )
            ReceiveQueryTasklists listGTaskLists ->
              ( { model
                | page = Models.HomePage
                    { api = api
                    , taskLists =
                        Just <|
                          List.map
                            Models.fromGTaskList
                            listGTaskLists.items
                    , currentTaskList = Nothing
                    }
                }
              , Cmd.none
              )
            SelectTaskList zl ->
              let
                (model_, cmd) = update (Layout.toggleDrawer Mdl) model
              in
                ( { model_ | page = Models.HomePage
                      { api = api
                      , taskLists = taskLists
                      , currentTaskList = Just zl.meta.id
                      }
                  }
                , Cmd.batch
                    [ cmd
                    , queryTasks api zl
                    , getTaskList api.taskLists zl.meta.id
                    ]
                )
            ReceiveQueryTasks listId listGTasks ->
              ( { model | page = Models.HomePage
                    { api = api
                    , taskLists =
                        Maybe.map
                          (\ls ->
                            List.map (\l ->
                                if l.meta.id == listId then
                                  { l | tasks = Just listGTasks.items }
                                else
                                  l
                            )
                            ls
                          )
                          taskLists
                    , currentTaskList = currentTaskList
                    }
                }
              , Cmd.none
              )
            GotTaskList id gTaskList ->
              ( { model | page = Models.HomePage
                    { api = api
                    , taskLists = taskLists
                    , currentTaskList = Just gTaskList.id
                    }
                }
              , Cmd.none
              )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Material.subscriptions Mdl model
    , receiveOAuthClientId <| \maybeId ->
        case maybeId of
          Nothing ->
            NoOp
          Just id ->
            UpdateOAuthClientId id
    , Sub.map AuthPageMsg <| receiveOAuthAccessToken ReceiveOAuthAccessToken
    ]

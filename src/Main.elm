
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

import GoogleTasks.Decoders as Marshallers
import Models exposing (Model)
import OAuth.Models exposing (Token, bearerToken)
import OAuth.Authorization exposing (accessTokenFromLocation)
import OAuth.Http as OHttp

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
      case accessTokenFromLocation loc of
        Nothing ->
          (Models.AuthPage, requestOAuthAccessToken ())
        Just t ->
          ( Models.HomePage
              { accessToken = t
              , taskLists = Nothing
              , currentTaskList = Nothing
              }
          , Cmd.batch
              [ setOAuthAccessToken (Just t)
              , queryTasklists (bearerToken t)
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

queryTasklists : Token -> Cmd Msg
queryTasklists token =
  Http.send
    (\result -> case result of
        Ok listGTaskList ->
          HomePageMsg <| ReceiveQueryTasklists listGTaskList
        Err e ->
          let
            _ = Debug.log "HTTP error while getting tasklists" e
          in HomePageMsg Logout)
    (OHttp.get
      token
      "https://www.googleapis.com/tasks/v1/users/@me/lists"
      Marshallers.listGTaskLists)

queryTasks : Token -> Models.ZTaskList -> Cmd Msg
queryTasks token zTaskList =
  Http.send
    (\result -> case result of
        Ok listGTasks ->
          HomePageMsg <| ReceiveQueryTasks zTaskList.meta.id listGTasks
        Err e ->
          let
            _ = Debug.log "HTTP error while getting tasks" e
          in HomePageMsg Logout)
    (OHttp.get
      token
      ("https://www.googleapis.com/tasks/v1/lists/"
        ++ zTaskList.meta.id
        ++ "/tasks")
      Marshallers.listGTasks)

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
              ( { model | page = Models.HomePage
                  { accessToken = t
                  , taskLists = Nothing
                  , currentTaskList = Nothing
                  }
                }
              , queryTasklists (bearerToken t)
              )
    HomePageMsg homeMsg ->
      case model.page of
        Models.AuthPage ->
          (model, Cmd.none)
        Models.HomePage { accessToken, taskLists, currentTaskList } ->
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
                    { accessToken = accessToken
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
                      { accessToken = accessToken
                      , taskLists = taskLists
                      , currentTaskList = Just zl.meta.id
                      }
                  }
                , Cmd.batch
                    [ cmd
                    , queryTasks (bearerToken accessToken) zl
                    ]
                )
            ReceiveQueryTasks listId listGTasks ->
              ( { model | page = Models.HomePage
                    { accessToken = accessToken
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

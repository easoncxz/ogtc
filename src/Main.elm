
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

import Messages exposing (Msg(..), HomePageMsg(..), AuthPageMsg(..))
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
onLocationChange _ = NoOp


init : Nav.Location -> (Model, Cmd Msg)
init loc =
  let
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
              , queryTasklists t
              ]
          )
  in
    ( { mdl = Material.model
      , oauthClientId = ""
      , page = page
      }
    , Cmd.batch
        [ Material.init Mdl
        , requestOAuthClientId ()
        , cmd
        ]
    )

queryTasklists : String -> Cmd Msg
queryTasklists accessToken =
  Http.send
    (\result -> case result of
        Ok listGTaskList ->
          HomePageMsg <| ReceiveQueryTasklists listGTaskList
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
          HomePageMsg <| ReceiveQueryTasks zTaskList.meta.id listGTasks
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
    UpdateOAuthClientId id ->
      ({ model | oauthClientId = id }, setOAuthClientId (Just id))
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
              , queryTasklists t
              )
    HomePageMsg homeMsg ->
      case model.page of
        Models.AuthPage ->
          (model, Cmd.none)
        Models.HomePage { accessToken, taskLists, currentTaskList } ->
          case homeMsg of
            Logout ->
              ( { model | page = Models.AuthPage }, setOAuthAccessToken Nothing )
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
                    , queryTasks accessToken zl
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


module Views exposing (..)

import Date
import Dom
import Html as H
import Html.Attributes as HA
import Html.Events as HE

import Marshallers
import Models exposing (Model, ZTaskList)
import Messages exposing (Msg)
import OAuthHelpers exposing (makeAuthorizeUrl)

view : Model -> H.Html Msg
view model =
  case model.accessToken of
    Nothing ->
      viewLoginPage model
    Just token ->
      viewMainPage model

viewLoginPage : Model -> H.Html Msg
viewLoginPage model =
  let
    viewOAuthKeyInput =
      H.div [] [ H.input
        [ HE.onInput Messages.UpdateOAuthKey
        , HA.value <|
            case model.oauthKey of
              Nothing ->
                ""
              Just oauthKey ->
                oauthKey
        ]
        [ H.text "OAuth client key" ] ]
    viewAuthoriseButton =
      case model.oauthKey of
        Nothing ->
          H.p [] [ H.text
          "We need an OAuth client key to initiate authorisation." ]
        Just oauthKey ->
          H.p [] [ H.a
            [ HA.href <|
              makeAuthorizeUrl
              oauthKey
              "https://localhost/src/Main.elm"
              "state-here" ]
            [ H.text "Authorise via Google OAuth" ] ]
  in
    H.div []
      [ viewOAuthKeyInput
      , viewAuthoriseButton
      ]

viewMainPage : Model -> H.Html Msg
viewMainPage model =
  H.div []
    [ case model.accessToken of
        Nothing ->
          H.p [] [ H.text "No access token found" ]
        Just accessToken ->
          H.div []
            [ H.span [] [ H.text "Access token: " ]
            , H.code [] [ H.text accessToken ]
            ]
    , viewTaskLists model.taskLists model.currentTaskList
    , viewOneTaskList model.currentTaskList
    ]

viewTaskLists : Maybe (List ZTaskList) -> Maybe ZTaskList -> H.Html Msg
viewTaskLists taskLists currentTaskList =
  case taskLists of
    Nothing ->
      H.div []
        [ H.p [] [ H.text
            "Tasklists not loaded." ]
        , H.input
          [ HA.type_ "button"
          , HA.value "Load Tasklists"
          , HE.onClick Messages.QueryTasklists
          ] []
        ]
    Just [] ->
      H.p [] [ H.text
        "You have no tasklists." ]
    Just ls ->
      let
        viewTitle : Maybe ZTaskList -> ZTaskList -> H.Html Msg
        viewTitle focus taskList =
          let
            prefix =
              case focus of
                Nothing ->
                  "- "
                Just ztl ->
                  if ztl.meta.id == taskList.meta.id then
                    "* "
                  else
                    "- "
          in
            H.li
              [ HE.onClick (Messages.SelectTaskList taskList) ]
              [ H.text (prefix ++ taskList.meta.title) ]
      in
        H.div []
          [ H.p [] [ H.text "Tasklists: " ]
          , H.ul [] <|
            List.map (viewTitle currentTaskList) ls
          ]

viewOneTaskList : Maybe ZTaskList -> H.Html Msg
viewOneTaskList taskListMaybe =
  case taskListMaybe of
    Nothing ->
      H.p [] [ H.text "No tasklist selected" ]
    Just taskList ->
      H.div []
        [ H.p [] [ H.text <|
            "Tasks in " ++ taskList.meta.title ++ ":" ]
        , case taskList.tasks of
            [] ->
              H.p [] [ H.text "This list has no tasks" ]
            tasks ->
              H.ul [] <|
                let
                  viewTaskTitle : Marshallers.GTask -> H.Html Msg
                  viewTaskTitle t =
                    H.li [] [ H.text t.title ]
                in
                  List.map viewTaskTitle tasks
        ]

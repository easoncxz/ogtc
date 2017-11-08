
module Views exposing (..)

import Date
import Dom
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Material
import Material.Scheme as Scheme
import Material.Layout as Layout
import Material.Grid as Grid
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Textfield as Textfield
import Material.Button as Button

import Marshallers
import Models exposing (Model, ZTaskList)
import Messages
import Messages exposing (Msg)
import OAuthHelpers exposing (makeAuthorizeUrl)

view : Model -> H.Html Msg
view model =
  Layout.render
    Messages.Mdl
    model.mdl
    [ Layout.fixedHeader
    , Layout.fixedDrawer
    ]
    { header = viewHeader model
    , drawer = viewDrawer model
    , tabs = ([], [])
    , main = viewMain model
    }
  |> Scheme.top   -- can be replaced with proper <link> elements

viewHeader : Model -> List (H.Html Msg)
viewHeader model =
  [ Layout.row []
    [ Layout.title []
        [ H.text "ogtc"
        ]
    ]
  ]

viewDrawer : Model -> List (H.Html Msg)
viewDrawer model =
  [ Layout.title [] [ H.text "Drawer title" ]
  , Layout.navigation []
      [ Layout.link [] [ H.text "one" ]
      , Layout.link [] [ H.text "two" ]
      , Layout.link [] [ H.text "three" ]
      ]
  ]

boxed : List (Options.Style m)
boxed =
  [ Options.css "padding-left" "8%"
  , Options.css "padding-right" "8%"
  , Options.css "margin" "auto"
  ]

viewMain : Model -> List (H.Html Msg)
viewMain model =
  [ case model.accessToken of
      Nothing ->
        viewLoginPage model
      Just token ->
        viewMainPage model token
  ]

viewLoginPage : Model -> H.Html Msg
viewLoginPage model =
  Options.div
    [ Options.many boxed ]
    [ H.h1 []
        [ H.text "Welcome to ogtc!" ]
    , H.p []
        [ H.text <|
            "Ogtc is a Google Tasks client that runs in your browser. " ++
            "To use ogtc, please supply a Google OAuth client id." ]
    , H.div []
        [ H.span []
            [ Textfield.render Messages.Mdl [921238] model.mdl
                [ Textfield.label "OAuth client id"
                , Textfield.floatingLabel
                , Textfield.text_
                , Textfield.value <| Maybe.withDefault "" model.oauthKey
                , Options.onInput Messages.UpdateOAuthKey
                ]
                []
            ]
        , Options.span [ Options.css "margin-left" "1em" ]
            [ Button.render Messages.Mdl [776256] model.mdl
              [ Button.colored
              , Button.ripple
              , Options.onClick <|
                  Messages.SetOAuthClientId model.oauthKey
              ]
              [ H.text "save" ]
            ]
        ]
    , case model.oauthKey of
        Nothing ->
          Button.render Messages.Mdl [888173] model.mdl
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Button.disabled
            ]
            [ H.text "Authorise via Google OAuth" ]
        Just oauthKey ->
          Button.render Messages.Mdl [879123] model.mdl
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Button.link <|
                makeAuthorizeUrl oauthKey "https://localhost/index.html" "state-here"
            ]
            [ H.text "Authorise via Google OAuth" ]
    ]

viewMainPage : Model -> String -> H.Html Msg
viewMainPage model accessToken =
  H.div []
    [ H.h1 [] [ H.text "ogtc" ]
    , H.p [] [ H.text "Access token: " ]
    , H.div [] [ H.text accessToken ]
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
      case taskList.tasks of
        Nothing ->
          H.p [] [ H.text "Loading tasks..." ]
        Just [] ->
          H.p [] [ H.text "This list has no tasks" ]
        Just tasks ->
          H.div []
            [ H.p [] [ H.text <|
                "Tasks in " ++ taskList.meta.title ++ ":" ]
            , H.ul [] <|
                let
                  viewTaskTitle : Marshallers.GTask -> H.Html Msg
                  viewTaskTitle t =
                    H.li [] [ H.text t.title ]
                in
                  List.map viewTaskTitle tasks
            ]

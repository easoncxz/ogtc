
module Views exposing (..)

import Dom
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as JE
import Time.DateTime as DT exposing (DateTime)
import Material
import Material.Scheme as Scheme
import Material.Layout as Layout
import Material.Grid as Grid
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Textfield as Textfield
import Material.Button as Button

import Models exposing (Model, ZTaskList)
import Messages
import Messages exposing (Msg)
import OAuth.Authorization exposing (makeAuthorizeUrl)
import GoogleTasks.Encoders as GEncoders

view : Model -> H.Html Msg
view model =
  Layout.render
    Messages.Mdl
    model.mdl
    [ Layout.fixedHeader
    , fixedDrawerByPage model.page
    ]
    { header = viewHeader model
    , drawer = viewDrawer model
    , tabs = ([], [])
    , main = viewMain model
    }

fixedDrawerByPage : Models.AppPage -> Layout.Property Msg
fixedDrawerByPage page =
  case page of
    Models.AuthPage ->
      Options.nop
    Models.HomePage _ ->
      Layout.fixedDrawer

viewHeader : Model -> List (H.Html Msg)
viewHeader model =
  [ Layout.row []
    [ Layout.title [] [ H.text "ogtc" ]
    , Layout.spacer
    , Layout.navigation [] <|
        case model.page of
          Models.AuthPage ->
            []
          Models.HomePage _ ->
            [ Layout.link
                [ Options.onClick (Messages.HomePageMsg Messages.Logout) ]
                [ H.text "logout" ] ]
    ]
  ]

viewTasklistTitle : ZTaskList -> H.Html Msg
viewTasklistTitle l =
  Layout.link
    [ Options.onClick (Messages.HomePageMsg <| Messages.SelectTaskList l) ]
    [ H.text l.meta.title ]

viewDrawer : Model -> List (H.Html Msg)
viewDrawer model =
  [ Layout.title [] [ H.text "Task lists" ]
  , Layout.navigation [] <|
      case model.page of
        Models.AuthPage ->
          []
        Models.HomePage { taskLists } ->
          case taskLists of
            Nothing ->
              [ Layout.link [] [ H.text "Loading tasklists..." ] ]
            Just lists ->
              List.map viewTasklistTitle lists
  , Layout.spacer
  ] ++
  (case model.page of
    Models.HomePage homePage ->
      [ viewNewTaskListField model.mdl homePage ]
    Models.AuthPage ->
      []
  )

viewNewTaskListField : Material.Model -> Models.HomePageModel -> H.Html Msg
viewNewTaskListField mdl homePage =
  Options.div boxed
    [ Textfield.render Messages.Mdl [626525] mdl
      [ Textfield.label "New Tasklist"
      , Textfield.floatingLabel
      , Textfield.text_
      , Textfield.value homePage.newTaskListTitle
      , Options.onInput
        (Messages.HomePageMsg << Messages.UpdateNewTaskListTitle)
      , Options.css "width" "100%"
      ]
      []
    , Button.render Messages.Mdl [7715536] mdl
      [ Button.flat
      , Button.type_ "submit"
      , Options.onClick
        (Messages.HomePageMsg Messages.CreateNewTaskList)
      ]
      [ H.text "Go" ]
    ]

boxed : List (Options.Style m)
boxed =
  [ Options.css "padding-left" "8%"
  , Options.css "padding-right" "8%"
  , Options.css "margin" "auto"
  ]

viewMain : Model -> List (H.Html Msg)
viewMain model =
  [ case model.page of
      Models.AuthPage ->
        viewLoginPage model
      Models.HomePage m ->
        viewHomePage model m
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
                , Textfield.value model.oauthClientId
                , Options.onInput Messages.UpdateOAuthClientId
                ]
                []
            ]
        ]
    , case model.oauthClientId of
        "" ->
          Button.render Messages.Mdl [888173] model.mdl
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Button.disabled
            ]
            [ H.text "Authorise via Google OAuth" ]
        cid ->
          Button.render Messages.Mdl [879123] model.mdl
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Button.link <|
                makeAuthorizeUrl
                  cid
                  (model.location.origin ++ "/index.html")
                  "state-here"
            ]
            [ H.text "Authorise via Google OAuth" ]
    ]

viewHomePage : Model -> Models.HomePageModel -> H.Html Msg
viewHomePage model ({ taskLists, currentTaskList } as page) =
  case Models.findZTaskListById currentTaskList taskLists of
    Nothing ->
      Options.div [ Options.many boxed ]
        [ H.h1 [] [ H.text "No tasklist selected" ]
        , H.code [] [ H.text page.newTaskListTitle ]
        ]
    Just tl ->
      Options.div [ Options.many boxed ]
        [ H.h1 [] [ H.text tl.meta.title ]
        , H.pre [] [ H.text
            (JE.encode 2
              (Maybe.withDefault JE.null
                (Maybe.map GEncoders.gTask
                  (List.head
                    (Maybe.withDefault [] tl.tasks)))))
          ]
        , viewOneTaskList (Just tl)
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
                  viewTaskTitle t =
                    H.li [] [ H.text t.title ]
                in
                  List.map viewTaskTitle tasks
            ]

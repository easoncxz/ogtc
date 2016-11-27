
import Dom
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Task

import Messages exposing (Msg(..))
import Models exposing
  ( TaskStatus(..)
  , ZTask
  , ZTaskId(..)
  , ZTaskList
  , ZTaskListId(..)
  , NextIds
  , Model
  )

-- Main

main : Program Never Model Msg
main = H.program
  { init          = init
  , update        = update
  , subscriptions = subscriptions
  , view          = view
  }

-- Model

model : Model
model =
  { taskLists = []
  , currentTaskList = Nothing
  , taskListPrompt = "(task list name here...)"
  , taskPrompt = "(task name here...)"
  , nextIds =
      { task = 0
      , taskList = 0
      }
  , oauthKey = "key-ish"
  , oauthSecret = "not-so-secret"
  }

init : (Model, Cmd Msg)
init = (model, Cmd.none)

-- Draw the next task ID from the ID keeper
drawTaskId : NextIds -> (ZTaskId, NextIds)
drawTaskId n =
    ( ZTaskId n.task
    , { n | task = n.task + 1 }
    )

-- Draw the next tasklist ID from the ID keeper
drawTaskListId : NextIds -> (ZTaskListId, NextIds)
drawTaskListId n =
    ( ZTaskListId n.taskList
    , { n | taskList = n.taskList + 1 }
    )


-- Update

mapWhere : (a -> Bool) -> (a -> a) -> List a -> List a
mapWhere predicate f xs =
    let
        pf x =
            if predicate x then
                f x
            else
                x
    in
        List.map pf xs

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        CreateTaskList name ->
            let
                (id, nextIds) = drawTaskListId model.nextIds
                taskList =
                    { name = name
                    , tasks = []
                    , id = id
                    }
                m_ =
                    { model
                    | taskLists = taskList :: model.taskLists
                    , currentTaskList = Just id
                    , taskListPrompt = ""
                    , nextIds = nextIds
                    }
                focus =
                    Task.attempt
                        (always NoOp)
                        (Dom.focus elemIds.taskListInputElem)
            in (m_, focus)
        DeleteTaskList taskListId ->
            let
                remainders =
                    List.filter
                        (\tl -> tl.id /= taskListId)
                        model.taskLists
                removingCurrent = Just taskListId == model.currentTaskList
                m_ =
                    { model
                    | taskLists = remainders
                    , currentTaskList =
                        if removingCurrent then
                            Maybe.map
                                (\tl -> tl.id)
                                (List.head remainders)
                        else
                            model.currentTaskList
                    }
            in (m_, Cmd.none)
        SwitchToTaskList selection ->
            let
                m_ =
                    { model
                    | currentTaskList =
                        let
                            matched = List.head <|
                                List.filter
                                    (\tl -> tl.id == selection)
                                    model.taskLists
                        in
                            Maybe.map (\tl -> tl.id) matched
                    }
            in (m_, Cmd.none)
        CreateTask name ->
            let
                (id, nextIds) = drawTaskId model.nextIds
                task =
                    { title = name
                    , status = NeedsAction
                    , id = id
                    }
                m_ =
                    { model
                    | taskLists =
                        mapWhere
                            (\tl -> Just tl.id == model.currentTaskList)
                            (\tl -> { tl | tasks = task :: tl.tasks })
                            model.taskLists
                    , nextIds = nextIds
                    , taskPrompt = ""
                    }
                focus = Task.attempt
                    (always NoOp)
                    (Dom.focus elemIds.taskInputElem)
            in (m_, focus)
        DeleteTask selection ->
            let
                m_ =
                    { model
                    | taskLists =
                        mapWhere
                            (\tl -> Just tl.id == model.currentTaskList)
                            (\tl -> { tl | tasks =
                                List.filter
                                    (\t -> t.id /= selection)
                                    tl.tasks })
                                    model.taskLists
                    }
            in
                (m_, Cmd.none)
        UpdateTaskListPrompt p ->
            ({ model | taskListPrompt = p }, Cmd.none)
        UpdateTaskPrompt p ->
            ({ model | taskPrompt = p }, Cmd.none)
        UpdateOAuthKey k ->
            ({ model | oauthKey = k }, Cmd.none)
        UpdateOAuthSecret s ->
            ({ model | oauthSecret = s }, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- View

elemIds =
    { taskListInputElem = "task-list-input"
    , taskInputElem = "task-input"
    }

view : Model -> H.Html Msg
view model =
  H.div []
    [ H.h1 [] [ H.text "OGTC client" ]
    , H.div []
        [ H.h4 [] [ H.text "OAuth info" ]
        , H.p []
            [ H.b [] [ H.text "Key: " ]
            , H.text model.oauthKey
            , H.input
                [ HE.onInput UpdateOAuthKey
                , HA.value model.oauthKey
                ] []
            ]
        , H.p []
            [ H.b [] [ H.text "Secret: " ]
            , H.text model.oauthSecret
            , H.input
                [ HE.onInput UpdateOAuthSecret
                , HA.value model.oauthSecret
                ] []
            ]
        ]
    , H.h2 [] [ H.text "Task lists:" ]
    , H.ul []
        (List.map
            (\tl ->
                H.li []
                    [ H.a
                        [ HE.onClick (SwitchToTaskList tl.id)
                        , HA.href "#"
                        ]
                        [ H.text tl.name ]
                    , H.button [ HE.onClick (DeleteTaskList tl.id) ]
                        [ H.text "delete" ]
                    ])
            model.taskLists)
    , H.span []
        [ H.text "New task list:" ]
    , H.form [ HE.onSubmit (CreateTaskList (model.taskListPrompt)) ]
        [ H.input
            [ HE.onInput UpdateTaskListPrompt
            , HA.value model.taskListPrompt
            , HA.id elemIds.taskListInputElem
            ]
            []
        , H.input
            [ HA.type_ "submit"
            , HA.value "go!"
            ]
            []
        ]
    , H.h2 [] [ H.text "Tasks in this list:" ]
    , let
        currentTaskList = List.head <|
            List.filter
                (\tl -> Just tl.id == model.currentTaskList)
                model.taskLists
      in
          case currentTaskList of
              Just tl ->
                  H.div []
                    [ H.h3 [] [ H.text (tl.name ++ ":") ]
                    , H.ul []
                        (List.map
                            (\t -> H.li []
                                [ H.span [] [ H.text t.title ]
                                , H.button
                                    [ HE.onClick (DeleteTask t.id) ]
                                    [ H.text "delete" ]
                                ])
                            tl.tasks)
                    , H.span []
                        [ H.text "New task:" ]
                    , H.form [ HE.onSubmit (CreateTask (model.taskPrompt)) ]
                        [ H.input
                            [ HE.onInput UpdateTaskPrompt
                            , HA.value model.taskPrompt
                            , HA.id elemIds.taskInputElem
                            ]
                            []
                        , H.input
                            [ HA.type_ "submit"
                            , HA.value "new task"
                            ]
                            []
                        ]
                    ]
              Nothing ->
                  H.p [] [ H.text "No task-list selected! :)" ]
    ]


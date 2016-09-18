
module Views exposing (..)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import String

import Models exposing
    ( ZTask
    , Msg
        ( UpdateNewTaskTitleField
        , SubmitNewTask
        , DeleteTaskAt
        )
    )

showTask : ZTask -> H.Html Msg
showTask t = H.p [] [ H.text t.title ]

showTaskWithCancel : Int -> ZTask -> H.Html Msg
showTaskWithCancel n t =
    H.div []
        [ H.span [] [ H.text t.title ]
        , H.button
            [ HE.onClick (DeleteTaskAt n) ]
            [ H.text "rm" ]
        ]

showTaskList : List ZTask -> H.Html Msg
showTaskList ts =
    H.div [] (List.indexedMap showTaskWithCancel ts)

newTaskPrompt : String -> H.Html Msg
newTaskPrompt newTaskTitleField =
    H.div []
        [ H.input
            [ HA.value newTaskTitleField
            , HE.onInput UpdateNewTaskTitleField
            ] []
        , H.input
            [ HA.type' "submit"
            , HE.onClick SubmitNewTask
            ] []
        ]


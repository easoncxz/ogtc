
import Html as H
import Html.App as App

import Views as V
import Utils exposing (splitAt)
import Models exposing
    ( Id(..)
    , TaskStatus(..)
    , ZTask
    , Model
    , Msg(..)
    )

-- Main

main : Program Never
main = App.program
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    }

-- Model

model : Model
model =
    { tasks             = []
    , newTaskTitleField = "New task"
    }

init : (Model, Cmd Msg)
init = (model , Cmd.none)

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let model' =
        case msg of
            NoOp ->
                model
            UpdateNewTaskTitleField t ->
                { model | newTaskTitleField = t }
            SubmitNewTask ->
                { model
                | newTaskTitleField = ""
                , tasks = model.tasks ++ [
                    { title = model.newTaskTitleField
                    , status = NeedsAction
                    }]
                }
            DeleteTaskAt n ->
                if n < 0 then
                    model
                else
                    { model
                    | tasks =
                        let (front, back) = splitAt n model.tasks
                        in case List.tail back of
                            Just bs ->
                                front ++ bs
                            Nothing ->
                                front
                    }
    in (model', Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- View

view : Model -> H.Html Msg
view model =
    H.div []
        [ H.h1 [] [ H.text "OGTC client" ]
        , V.showTaskList model.tasks
        , V.newTaskPrompt model.newTaskTitleField
        ]


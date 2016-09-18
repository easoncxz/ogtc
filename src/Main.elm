
import Html as H
import Html.App as App

import Views as V
import Utils exposing (splitAt)
import Messages exposing (Msg(..))
import Models exposing
  ( Id(..)
  , TaskStatus(..)
  , ZTask
  , ZTaskList
  , Model
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
  { taskList =
    { name = "my task list"
    , tasks = []
    }
  , taskPrompt = "New task"
  }

init : (Model, Cmd Msg)
init = (model , Cmd.none)

-- Update

addDefaultTask : ZTaskList -> String -> ZTaskList
addDefaultTask list title =
  let newTask = { title = title , status = NeedsAction }
  in { list | tasks = list.tasks ++ [newTask] }

deleteTaskAt : ZTaskList -> Int -> ZTaskList
deleteTaskAt list n =
  if n < 0 then
    list
  else
    { list
    | tasks =
      let
          (front, back) = splitAt n list.tasks
      in
          case List.tail back of
            Just bs ->
              front ++ bs
            Nothing ->
              front
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let model' =
    case msg of
      NoOp ->
        model
      UpdateNewTaskTitleField t ->
        { model | taskPrompt = t }
      SubmitNewTask ->
        { model
        | taskPrompt = ""
        , taskList = addDefaultTask model.taskList model.taskPrompt
        }
      DeleteTaskAt n ->
        { model | taskList = deleteTaskAt model.taskList n }
  in (model', Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- View

view : Model -> H.Html Msg
view model =
  H.div []
    [ H.h1 [] [ H.text "OGTC client" ]
    , V.showTaskList model.taskList.tasks
    , V.newTaskPrompt model.taskPrompt
    ]


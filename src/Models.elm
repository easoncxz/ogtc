
module Models exposing (..)

type alias Model =
    { tasks             : List ZTask
    , newTaskTitleField : String
    }

type alias ZTask =
    { title  : String
    , status : TaskStatus
    }

type Id a = Id String

type TaskStatus
    = NeedsAction
    | Completed

type Msg
    = NoOp
    | UpdateNewTaskTitleField String
    | SubmitNewTask
    | DeleteTaskAt Int



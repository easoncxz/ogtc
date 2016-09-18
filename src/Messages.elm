
module Messages exposing (..)

type Msg
    = NoOp
    | UpdateNewTaskTitleField String
    | SubmitNewTask
    | DeleteTaskAt Int



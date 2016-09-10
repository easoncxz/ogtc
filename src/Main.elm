
import Html as H
import Html.App as HA
import Html.Attributes as HT
import Html.Events as HE
import Date exposing (Date(..))

-- Model

type Id a = Id String

type Tree a
    = Leaf a
    | Branches (List (Tree a))

type TaskStatus
    = NeedsAction
    | Completed

type TaskPosition = TaskPosition String

type TaskListKindValue = TaskListKindValue String
taskListKindValue = TaskListKindValue "tasks#taskList"

type LinkEntities = LinkEntities
    { type'       : String
    , description : String
    , link        : String
    }

type GTask = GTask
    { kind      : TaskListKindValue
    , id        : Id GTask
    -- , etag      : String
    , title     : String
    -- , updated   : Date
    -- , selfLink  : Id GTask
    , parent    : Maybe (Id GTask)
    , position  : TaskPosition
    -- , notes     : String
    , status    : TaskStatus
    -- , due       : Date
    -- , completed : Date
    , deleted   : Bool
    , hidden    : Bool
    -- , links     : List LinkEntities
    }

type GTaskList = GTaskList
    { kind     : TaskListKindValue
    , id       : Id GTaskList
    , etag     : String
    , title    : String
    , updated  : Date
    , selfLink : String
    }

type Model = Model
    { accessToken       : String
    , tasks             : Tree GTask
    , inputValue        : String
    , newTaskTitleField : String
    }

model : Model
model = Model
    { accessToken = "(no token)"
    , tasks = Branches []
    , inputValue = "initial value"
    , newTaskTitleField = "New task"
    }

-- Update

type Msg
    = NoOp
    | UpdateInput String
    | UpdateNewTaskTitleField String
    | SubmitNewTask

makeDefaultTask : String -> GTask
makeDefaultTask title = GTask
    { kind     = taskListKindValue
    , id       = Id "new id"  -- TODO
    , title    = title
    , parent   = Nothing
    , position = TaskPosition ""  -- TODO
    , status   = NeedsAction
    , deleted  = False
    , hidden   = False
    }

update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        NoOp ->
            Model model
        UpdateInput t ->
            Model { model | inputValue = t }
        UpdateNewTaskTitleField t ->
            Model { model | newTaskTitleField = t }
        SubmitNewTask ->
            Model { model
                | newTaskTitleField = ""
                , tasks =
                    let newTask = makeDefaultTask model.newTaskTitleField
                    in case model.tasks of
                        Leaf task ->
                            Branches [Leaf task, Leaf newTask]
                        Branches taskList ->
                            Branches (taskList ++ [Leaf newTask])
                }

-- View

showTaskTree : Tree GTask -> H.Html Msg
showTaskTree tree =
    case tree of
        Leaf (GTask task) ->
            H.p [] [ H.text task.title ]
        Branches trees ->
            H.div [] <| List.map showTaskTree trees

newTaskSubmitter : Model -> H.Html Msg
newTaskSubmitter (Model m) =
    H.div []
        [ H.input
            [ HE.onInput UpdateNewTaskTitleField
            , HT.value m.newTaskTitleField
            ]
            []
        , H.input
            [ HT.type' "submit"
            , HE.onSubmit SubmitNewTask ]
            []
        ]

view : Model -> H.Html Msg
view (Model model) =
    H.div []
        [ H.h1 [] [ H.text "OGTC client" ]
        , showTaskTree model.tasks
        , newTaskSubmitter (Model model)
        ]

main =
    HA.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


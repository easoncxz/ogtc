
import Html as H
import Html.App as HA
import Html.Attributes as HT
import Html.Events as HE
import Date exposing (Date(..))

type Id a = Id String

type TaskStatus
    = NeedsAction
    | Completed

type alias ZTask =
    { title  : String
    , status : TaskStatus
    }

type alias Model =
    { tasks             : List ZTask
    , newTaskTitleField : String
    }

type Msg
    = NoOp
    | UpdateNewTaskTitleField String
    | SubmitNewTask
    | DeleteTaskAt Int

-- Model

model : Model
model =
    { tasks             = []
    , newTaskTitleField = "New task"
    }

-- Update

splitAt : Int -> List a -> (List a, List a)
splitAt n xs = (List.take n xs, List.drop n xs)

update : Msg -> Model -> Model
update msg model =
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


-- View

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

newTaskPrompt : Model -> H.Html Msg
newTaskPrompt model =
    H.div []
        [ H.input
            [ HT.value model.newTaskTitleField
            , HE.onInput UpdateNewTaskTitleField
            ] []
        , H.input
            [ HT.type' "submit"
            , HE.onClick SubmitNewTask
            ] []
        ]

view : Model -> H.Html Msg
view model =
    H.div []
        [ H.h1 [] [ H.text "OGTC client" ]
        , showTaskList model.tasks
        , newTaskPrompt model
        ]

main =
    HA.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



import Html as H
import Html.App as HA
import Html.Attributes as HT
import Html.Events as HE

main =
    HA.beginnerProgram
        { model = model
        , view = view
        , update = update
        }

type Tree a
    = Leaf a
    | Branches (List (Tree a))

type Task = DummyTask

type alias Model =
    { accessToken : String
    , tasks : Tree Task
    , inputValue : String
    }

type Msg
    = NoOp
    | UpdateInput String

model : Model
model =
    { accessToken = "(no token)"
    , tasks = Branches []
    , inputValue = "initial value"
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model
        UpdateInput text ->
            { model | inputValue = text }

view : Model -> H.Html Msg
view model =
    H.div []
        [ H.text "some stuff here"
        , H.input
            [ HE.onInput UpdateInput
            , HT.value model.inputValue
            ]
            []
        , H.p [] [ H.text model.inputValue ]
        ]

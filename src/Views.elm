
module Views exposing (view)

import Dom
import Html as H
import Html.Attributes as HA
import Html.Events as HE

import Models exposing (..)
import Messages exposing (..)

view : Model -> H.Html Msg
view model = H.text "hello"

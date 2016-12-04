
module Views exposing (view)

import Date
import Dom
import Html as H
import Html.Attributes as HA
import Html.Events as HE

import Models exposing (..)
import Messages exposing (..)

view : Model -> H.Html Msg
view model =
  H.div []
    [ H.button
      [ HE.onClick MakeRoll ]
      [ H.text "roll a dice" ]
    , H.div []
      [ H.text "dice roll: "
      , H.code []
        [ H.text (toString model.dice) ]
      ]
    , H.hr [] []
    , H.p [] [ H.text
      ("Clock running? --> " ++ toString model.clockEnabled) ]
    , H.p []
      [ H.text "Current time: "
      , H.code []
        [ H.text (case model.time of
            Nothing ->
              "unknown"
            Just t ->
              t |> Date.fromTime |> toString) ]
      ]
    , H.button
      [ HE.onClick RefreshClock ]
      [ H.text "refresh clock" ]
    , H.button
      [ HE.onClick ToggleClockEnabled ]
      [ H.text "toggle clock" ]
    ]


module Views exposing (..)

import Date
import Dom
import Html as H
import Html.Attributes as HA
import Html.Events as HE

import Models exposing (Model)
import Messages exposing (Msg)
import OAuthHelpers exposing (makeAuthorizeUrl)

view : Model -> H.Html Msg
view model =
  case model.accessToken of
    Nothing ->
      viewLoginPage model
    Just token ->
      viewMainPage model

viewLoginPage : Model -> H.Html Msg
viewLoginPage model =
  let
    viewOAuthKeyInput =
      H.div [] [ H.input
        [ HE.onInput Messages.UpdateOAuthKey
        , HA.value <|
            case model.oauthKey of
              Nothing ->
                ""
              Just oauthKey ->
                oauthKey
        ]
        [ H.text "OAuth client key" ] ]
    viewAuthoriseButton =
      case model.oauthKey of
        Nothing ->
          H.p [] [ H.text
          "We need an OAuth client key to initiate authorisation." ]
        Just oauthKey ->
          H.p [] [ H.a
            [ HA.href <|
              makeAuthorizeUrl
              oauthKey
              "https://localhost/src/Main.elm"
              "state-here" ]
            [ H.text "Authorise via Google OAuth" ] ]
  in
    H.div []
      [ viewOAuthKeyInput
      , viewAuthoriseButton
      ]

viewMainPage : Model -> H.Html Msg
viewMainPage model =
  H.div []
    [ case model.accessToken of
        Nothing ->
          H.p [] [ H.text "No access token found" ]
        Just accessToken ->
          H.div []
            [ H.span [] [ H.text "Access token: " ]
            , H.code [] [ H.text accessToken ]
            ]
    ]

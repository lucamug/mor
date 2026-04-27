module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Border as Border
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


init : () -> ( Model, Cmd msg )
init _ =
    ( { count = 0 }, Cmd.none )


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ Html.text "Zoom IN" ]
        , div [] [ Html.text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ Html.text "Zoom OUT" ]
        , layout [] <|
            column [ width fill, padding 10 ]
                [ image [ width fill, Border.width 1 ] { src = "map.svg", description = "A world map" }
                ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

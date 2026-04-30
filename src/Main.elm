module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg
import Svg.Attributes
import Task


type alias Model =
    { zoom : Float
    , panX : Float
    , panY : Float
    , dragging : Bool
    , svgWidth : Float
    , velX : Float
    , velY : Float
    , lastTouchX : Float
    , lastTouchY : Float
    , lastPinchDist : Maybe Float
    , mouseX : Float
    , mouseY : Float
    , svgHeight : Float
    , showLabelDots : Bool
    , calibX : Float
    , calibY : Float
    , calibScaleX : Float
    , calibScaleY : Float
    , containerW : Float
    , containerH : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { zoom = 0
      , panX = 0
      , panY = 0
      , dragging = False
      , svgWidth = 800
      , velX = 0
      , velY = 0
      , lastTouchX = 0
      , lastTouchY = 0
      , lastPinchDist = Nothing
      , mouseX = 0
      , mouseY = 0
      , svgHeight = 600
      , showLabelDots = False
      , calibX = -35
      , calibY = -78.5
      , calibScaleX = 0.9619
      , calibScaleY = 0.9664
      , containerW = 800
      , containerH = 600
      }
        |> reset
    , Task.perform
        (\vp -> Resize (round vp.viewport.width) (round vp.viewport.height))
        Browser.Dom.getViewport
    )


type Msg
    = Increment
    | Decrement
    | PanLeft
    | PanRight
    | PanUp
    | PanDown
    | GoUK
    | GoUS
    | GoJA
    | DragStart Float
    | DragEnd
    | DragMove Float Float
    | AnimationFrame
    | TouchStart Float Float Float (Maybe ( Float, Float ))
    | TouchMove Float Float (Maybe ( Float, Float ))
    | TouchEnd
    | ZoomSlider Float
    | GoReset
    | MouseMove Float Float Float Float
    | ToggleLabelDots
    | SetCalibX Float
    | SetCalibY Float
    | SetCalibScaleX Float
    | SetCalibScaleY Float
    | Resize Int Int


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Increment ->
            let
                factor =
                    2 ^ 0.1
            in
            ( { model | zoom = model.zoom + 0.1, panX = model.panX * factor, panY = model.panY * factor }, Cmd.none )

        Decrement ->
            let
                factor =
                    2 ^ -0.1
            in
            ( { model | zoom = model.zoom - 0.1, panX = model.panX * factor, panY = model.panY * factor }, Cmd.none )

        PanLeft ->
            ( { model | panX = model.panX - 50 }, Cmd.none )

        PanRight ->
            ( { model | panX = model.panX + 50 }, Cmd.none )

        PanUp ->
            ( { model | panY = model.panY - 50 }, Cmd.none )

        PanDown ->
            ( { model | panY = model.panY + 50 }, Cmd.none )

        GoUK ->
            ( uk model, Cmd.none )

        GoUS ->
            ( us model, Cmd.none )

        GoJA ->
            ( ja model, Cmd.none )

        DragStart w ->
            ( { model | dragging = True, svgWidth = w, velX = 0, velY = 0 }, Cmd.none )

        DragEnd ->
            ( { model | dragging = False }, Cmd.none )

        DragMove movX movY ->
            if model.dragging then
                let
                    containerAspect =
                        if model.containerH > 0 then
                            model.containerW / model.containerH

                        else
                            2000 / 1280

                    svgUnitsPerPixel =
                        if containerAspect < 2000 / 1280 then
                            1280 / model.containerH

                        else
                            2000 / model.containerW

                    dx =
                        movX * svgUnitsPerPixel

                    dy =
                        movY * svgUnitsPerPixel
                in
                ( { model
                    | panX = model.panX - dx
                    , panY = model.panY - dy
                    , velX = model.velX * 0.4 + dx * 0.6
                    , velY = model.velY * 0.4 + dy * 0.6
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        AnimationFrame ->
            if model.dragging then
                ( { model | velX = model.velX * 0.8, velY = model.velY * 0.8 }, Cmd.none )

            else if abs model.velX > 0.01 || abs model.velY > 0.01 then
                ( { model
                    | panX = model.panX - model.velX
                    , panY = model.panY - model.velY
                    , velX = model.velX * 0.92
                    , velY = model.velY * 0.92
                  }
                , Cmd.none
                )

            else
                ( { model | velX = 0, velY = 0 }, Cmd.none )

        GoReset ->
            ( reset model, Cmd.none )

        ToggleLabelDots ->
            ( { model | showLabelDots = not model.showLabelDots }, Cmd.none )

        MouseMove clientX clientY w h ->
            let
                scale =
                    2 ^ model.zoom

                mapAspect =
                    2000 / 1280

                containerAspect =
                    if h > 0 then
                        w / h

                    else
                        mapAspect

                ( zoomedWidth, zoomedHeight ) =
                    if containerAspect < mapAspect then
                        let
                            zh =
                                1280 / scale
                        in
                        ( zh * containerAspect, zh )

                    else
                        let
                            zw =
                                2000 / scale
                        in
                        ( zw, zw / containerAspect )

                offsetX =
                    (2000 - zoomedWidth) / 2 + model.panX / scale

                offsetY =
                    (1280 - zoomedHeight) / 2 + model.panY / scale

                svgX =
                    offsetX + (clientX / w) * zoomedWidth

                svgY =
                    offsetY + (clientY / h) * zoomedHeight
            in
            ( { model | mouseX = svgX, mouseY = svgY, svgWidth = w }, Cmd.none )

        ZoomSlider newZoom ->
            let
                factor =
                    2 ^ (newZoom - model.zoom)
            in
            ( { model | zoom = newZoom, panX = model.panX * factor, panY = model.panY * factor }, Cmd.none )

        TouchStart w x y maybeSecond ->
            let
                pinchDist =
                    maybeSecond
                        |> Maybe.map (\( x2, y2 ) -> sqrt ((x2 - x) ^ 2 + (y2 - y) ^ 2))
            in
            ( { model | dragging = True, svgWidth = w, lastTouchX = x, lastTouchY = y, velX = 0, velY = 0, lastPinchDist = pinchDist }, Cmd.none )

        TouchMove x y maybeSecond ->
            if model.dragging then
                case maybeSecond of
                    Just ( x2, y2 ) ->
                        let
                            newDist =
                                sqrt ((x2 - x) ^ 2 + (y2 - y) ^ 2)

                            zoomDelta =
                                case model.lastPinchDist of
                                    Just oldDist ->
                                        logBase 2 (newDist / oldDist)

                                    Nothing ->
                                        0

                            factor =
                                2 ^ zoomDelta

                            newZoom =
                                model.zoom + zoomDelta
                        in
                        ( { model
                            | zoom = newZoom
                            , panX = model.panX * factor
                            , panY = model.panY * factor
                            , lastPinchDist = Just newDist
                            , lastTouchX = x
                            , lastTouchY = y
                            , velX = 0
                            , velY = 0
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        let
                            containerAspect =
                                if model.containerH > 0 then
                                    model.containerW / model.containerH

                                else
                                    2000 / 1280

                            svgUnitsPerPixel =
                                if containerAspect < 2000 / 1280 then
                                    1280 / model.containerH

                                else
                                    2000 / model.containerW

                            dx =
                                (x - model.lastTouchX) * svgUnitsPerPixel

                            dy =
                                (y - model.lastTouchY) * svgUnitsPerPixel
                        in
                        ( { model
                            | panX = model.panX - dx
                            , panY = model.panY - dy
                            , velX = model.velX * 0.4 + dx * 0.6
                            , velY = model.velY * 0.4 + dy * 0.6
                            , lastTouchX = x
                            , lastTouchY = y
                            , lastPinchDist = Nothing
                          }
                        , Cmd.none
                        )

            else
                ( model, Cmd.none )

        TouchEnd ->
            ( { model | dragging = False, lastPinchDist = Nothing }, Cmd.none )

        SetCalibX v ->
            ( { model | calibX = v }, Cmd.none )

        SetCalibY v ->
            ( { model | calibY = v }, Cmd.none )

        SetCalibScaleX v ->
            ( { model | calibScaleX = v }, Cmd.none )

        SetCalibScaleY v ->
            ( { model | calibScaleY = v }, Cmd.none )

        Resize w h ->
            ( { model | containerW = toFloat w, containerH = toFloat h }, Cmd.none )


attrsButton : List (Attribute msg)
attrsButton =
    [ paddingXY 15 10, Border.width 1, rounded 50, Background.color <| rgb 1 1 1, htmlAttribute <| Html.Attributes.style "pointer-events" "auto" ]


view : Model -> Html Msg
view model =
    layout [ height fill ] <|
        column
            [ width fill
            , height fill
            , Background.color <| rgb 0.7 0.95 1
            , clip
            , inFront <| header model
            , inFront <| footer
            ]
            [ el [ width fill, height fill ] <|
                html <|
                    Html.div
                        [ Html.Attributes.style "cursor"
                            (if model.dragging then
                                "grabbing"

                             else
                                "grab"
                            )
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "position" "relative"
                        , Html.Attributes.style "touch-action" "none"
                        , Html.Events.on "mousedown" dragStartDecoder
                        , Html.Events.on "mousemove" mouseMoveDecoder
                        , Html.Events.on "touchstart" touchStartDecoder
                        , Html.Events.on "touchmove" touchMoveDecoder
                        , Html.Events.on "touchend" (Decode.succeed TouchEnd)
                        ]
                        (svg model.zoom model.panX model.panY model.containerW model.containerH
                            :: viewCursorLabel model
                            :: List.map (viewLabel model.zoom model.panX model.panY model.showLabelDots model.calibX model.calibY model.calibScaleX model.calibScaleY model.containerW model.containerH) labels
                            ++ List.map (viewCard model.zoom model.panX model.panY model.containerW model.containerH model.calibX model.calibY model.calibScaleX model.calibScaleY) cards
                        )
            ]


type alias Label =
    { lat : Float
    , lng : Float
    , content : String
    }


labels : List Label
labels =
    [ { lat = 51.5, lng = -0.1, content = "London" }
    , { lat = 48.9, lng = 2.3, content = "Paris" }
    , { lat = 40.7, lng = -74.0, content = "New York" }
    , { lat = 35.7, lng = 139.7, content = "Tokyo" }
    , { lat = -33.9, lng = 151.2, content = "Sydney" }
    , { lat = 55.8, lng = 37.6, content = "Moscow" }
    , { lat = 28.6, lng = 77.2, content = "Delhi" }
    , { lat = -23.5, lng = -46.6, content = "São Paulo" }
    , { lat = 30.1, lng = 31.2, content = "Cairo" }
    ]


type alias Card =
    { lat : Float
    , lng : Float
    , title : String
    , description : String
    , minZoom : Float
    }


cards : List Card
cards =
    [ { lat = 51.5
      , lng = -0.1
      , title = "Card 1"
      , description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In et dolor porta, tristique orci at, tempus nibh. Ut ac gravida turpis."
      , minZoom = 2
      }
    ]


viewCard : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Card -> Html.Html msg
viewCard zoom panX panY containerW containerH calibX calibY calibScaleX calibScaleY card =
    if zoom < card.minZoom then
        Html.text ""

    else
        let
            baseWidth =
                2000

            baseHeight =
                1280

            scale =
                2 ^ zoom

            mapAspect =
                baseWidth / baseHeight

            containerAspect =
                if containerH > 0 then
                    containerW / containerH

                else
                    mapAspect

            ( zoomedWidth, zoomedHeight ) =
                if containerAspect < mapAspect then
                    let
                        h =
                            baseHeight / scale
                    in
                    ( h * containerAspect, h )

                else
                    let
                        w =
                            baseWidth / scale
                    in
                    ( w, w / containerAspect )

            offsetX =
                (baseWidth - zoomedWidth) / 2 + panX / scale

            offsetY =
                (baseHeight - zoomedHeight) / 2 + panY / scale

            svgX =
                ((card.lng + 180) * (baseWidth / 360) - baseWidth / 2) * calibScaleX + baseWidth / 2 + calibX

            latRad =
                card.lat * pi / 180

            mercY =
                logBase e (tan (pi / 4 + latRad / 2))

            svgY =
                (baseWidth / 2 * (1 - mercY / pi) - baseHeight / 2) * calibScaleY + baseHeight / 2 + calibY

            pctX =
                (svgX - offsetX) / zoomedWidth * 100

            pctY =
                (svgY - offsetY) / zoomedHeight * 100
        in
        Html.div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" (String.fromFloat pctX ++ "%")
            , Html.Attributes.style "top" (String.fromFloat pctY ++ "%")
            , Html.Attributes.style "transform" "translate(-50%, calc(-100% - 18px))"

            -- , Html.Attributes.style "pointer-events" "none"
            , Html.Attributes.style "width" "180px"
            ]
            [ layoutWith { options = [ noStaticStyleSheet ] }
                -- [ htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                []
                (column
                    [ width fill
                    , Border.width 2
                    , Border.color (rgb255 51 51 51)
                    , Border.rounded 10
                    , Border.shadow { offset = ( 3, 3 ), size = 0, blur = 0, color = rgb255 51 51 51 }
                    , Background.color (rgb 1 1 1)
                    , padding 10
                    , spacing 4
                    ]
                    [ el [ Font.bold, Font.size 14 ] (text card.title)
                    , paragraph [ Font.size 12, Font.color (rgb255 68 68 68) ] [ text card.description ]
                    ]
                )
            , Html.div
                [ Html.Attributes.style "width" "0"
                , Html.Attributes.style "height" "0"
                , Html.Attributes.style "border-left" "10px solid transparent"
                , Html.Attributes.style "border-right" "10px solid transparent"
                , Html.Attributes.style "border-top" "12px solid #333"
                , Html.Attributes.style "margin-left" "calc(50% - 10px)"
                ]
                []
            , Html.div
                [ Html.Attributes.style "width" "0"
                , Html.Attributes.style "height" "0"
                , Html.Attributes.style "border-left" "8px solid transparent"
                , Html.Attributes.style "border-right" "8px solid transparent"
                , Html.Attributes.style "border-top" "10px solid white"
                , Html.Attributes.style "margin-left" "calc(50% - 8px)"
                , Html.Attributes.style "margin-top" "-13px"
                ]
                []
            ]


viewCursorLabel : Model -> Html.Html msg
viewCursorLabel model =
    let
        scale =
            2 ^ model.zoom

        mapAspect =
            2000 / 1280

        containerAspect =
            if model.containerH > 0 then
                model.containerW / model.containerH

            else
                mapAspect

        ( zoomedWidth, zoomedHeight ) =
            if containerAspect < mapAspect then
                let
                    h =
                        1280 / scale
                in
                ( h * containerAspect, h )

            else
                let
                    w =
                        2000 / scale
                in
                ( w, w / containerAspect )

        offsetX =
            (2000 - zoomedWidth) / 2 + model.panX / scale

        offsetY =
            (1280 - zoomedHeight) / 2 + model.panY / scale

        pctX =
            (model.mouseX - offsetX) / zoomedWidth * 100

        pctY =
            (model.mouseY - offsetY) / zoomedHeight * 100

        labelText =
            "(" ++ String.fromInt (round model.mouseX) ++ ", " ++ String.fromInt (round model.mouseY) ++ ")"
    in
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" (String.fromFloat pctX ++ "%")
        , Html.Attributes.style "top" (String.fromFloat pctY ++ "%")
        , Html.Attributes.style "transform" "translate(12px, -50%)"

        -- , Html.Attributes.style "pointer-events" "none"
        , Html.Attributes.style "color" "black"
        , Html.Attributes.style "font-size" "11px"
        , Html.Attributes.style "font-family" "monospace"
        , Html.Attributes.style "white-space" "nowrap"
        , Html.Attributes.style "background" "rgba(255,255,255,0.75)"
        , Html.Attributes.style "padding" "1px 4px"
        , Html.Attributes.style "border-radius" "3px"
        ]
        [ Html.text labelText ]


viewLabel : Float -> Float -> Float -> Bool -> Float -> Float -> Float -> Float -> Float -> Float -> Label -> Html.Html msg
viewLabel zoom panX panY showDots calibX calibY calibScaleX calibScaleY containerWidth containerHeight label =
    let
        baseWidth =
            2000

        baseHeight =
            1280

        scale =
            2 ^ zoom

        mapAspect =
            baseWidth / baseHeight

        containerAspect =
            if containerHeight > 0 then
                containerWidth / containerHeight

            else
                mapAspect

        ( zoomedWidth, zoomedHeight ) =
            if containerAspect < mapAspect then
                let
                    h =
                        baseHeight / scale
                in
                ( h * containerAspect, h )

            else
                let
                    w =
                        baseWidth / scale
                in
                ( w, w / containerAspect )

        offsetX =
            (baseWidth - zoomedWidth) / 2 + panX / scale

        offsetY =
            (baseHeight - zoomedHeight) / 2 + panY / scale

        svgX =
            ((label.lng + 180) * (baseWidth / 360) - baseWidth / 2) * calibScaleX + baseWidth / 2 + calibX

        latRad =
            label.lat * pi / 180

        mercY =
            logBase e (tan (pi / 4 + latRad / 2))

        svgY =
            (baseWidth / 2 * (1 - mercY / pi) - baseHeight / 2) * calibScaleY + baseHeight / 2 + calibY

        pctX =
            (svgX - offsetX) / zoomedWidth * 100

        pctY =
            (svgY - offsetY) / zoomedHeight * 100
    in
    if showDots then
        Html.div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" (String.fromFloat pctX ++ "%")
            , Html.Attributes.style "top" (String.fromFloat pctY ++ "%")
            , Html.Attributes.style "transform" "translate(-50%, -50%)"

            -- , Html.Attributes.style "pointer-events" "none"
            , Html.Attributes.style "width" "6px"
            , Html.Attributes.style "height" "6px"
            , Html.Attributes.style "border-radius" "50%"
            , Html.Attributes.style "background" "red"
            ]
            []

    else
        Html.div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" (String.fromFloat pctX ++ "%")
            , Html.Attributes.style "top" (String.fromFloat pctY ++ "%")
            , Html.Attributes.style "transform" "translate(-50%, -50%)"

            -- , Html.Attributes.style "pointer-events" "none"
            , Html.Attributes.style "color" "black"
            , Html.Attributes.style "font-size" "12px"
            , Html.Attributes.style "font-weight" "bold"
            , Html.Attributes.style "white-space" "nowrap"
            , Html.Attributes.style "text-shadow" "0 0 3px white, 0 0 3px white, 0 0 3px white"
            ]
            [ Html.text label.content ]


footer =
    paragraph
        [ Font.size 60
        , Font.center
        , Font.family [ Font.serif ]
        , centerX
        , alignBottom
        , padding 20
        , Font.bold

        -- , htmlAttribute <| Html.Attributes.style "letter-spacing" "0.2rem"
        , htmlAttribute <| Html.Attributes.style "color" "orange"
        , htmlAttribute <| Html.Attributes.style "text-shadow" "1px 1px 0 brown, -1px -1px 0 brown, 1px -1px 0 brown, -1px 1px 0 brown"

        -- , htmlAttribute <| Html.Attributes.style "text-shadow" "0px 0px 0 brown"
        , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
        ]
        [ text "Elm Training Material"
        ]


calibSlider : String -> Float -> Float -> Float -> (Float -> Msg) -> Element Msg
calibSlider label_ min_ max_ value_ msg =
    column
        [ spacing 3
        , htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
        ]
        [ el [ Font.size 10, centerX ] <| text (label_ ++ ": " ++ String.fromFloat value_)
        , Input.slider
            [ width (px 120)
            , Element.behindContent
                (el [ width fill, height (px 2), centerY, Background.color (rgb 0.5 0.5 0.5), rounded 2 ] none)
            ]
            { onChange = msg
            , label = Input.labelHidden label_
            , min = min_
            , max = max_
            , step = Nothing
            , value = value_
            , thumb = Input.defaultThumb
            }
        ]


header model =
    wrappedRow
        [ spacing 10
        , paddingXY 15 10

        -- , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
        ]
        [ Input.button attrsButton { onPress = Just Increment, label = text "Zoom IN" }
        , Input.button attrsButton { onPress = Just Decrement, label = text "Zoom OUT" }
        , Input.slider
            [ width (px 200)
            , htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
            , Element.behindContent
                (el
                    [ width fill
                    , height (px 2)
                    , centerY
                    , Background.color (rgb 0.5 0.5 0.5)
                    , rounded 2
                    ]
                    none
                )
            ]
            { onChange = ZoomSlider
            , label = Input.labelHidden "Zoom"
            , min = -1
            , max = 6
            , step = Nothing
            , value = model.zoom
            , thumb = Input.defaultThumb
            }
        , Input.button attrsButton { onPress = Just GoUK, label = text "UK" }
        , Input.button attrsButton { onPress = Just GoUS, label = text "US" }
        , Input.button attrsButton { onPress = Just GoJA, label = text "JA" }
        , Input.button attrsButton { onPress = Just GoReset, label = text "Reset" }
        , Input.button attrsButton
            { onPress = Just ToggleLabelDots
            , label =
                text
                    (if model.showLabelDots then
                        "Dots ON"

                     else
                        "Dots OFF"
                    )
            }
        , calibSlider "X offset" -200 200 model.calibX SetCalibX
        , calibSlider "Y offset" -200 200 model.calibY SetCalibY
        , calibSlider "X scale" 0.5 2.0 model.calibScaleX SetCalibScaleX
        , calibSlider "Y scale" 0.5 2.0 model.calibScaleY SetCalibScaleY
        , row [ spacing 10, padding 5 ]
            [ Input.button attrsButton { onPress = Just PanLeft, label = text "←" }
            , column [ spacing 5 ]
                [ Input.button attrsButton { onPress = Just PanUp, label = text "↑" }
                , Input.button attrsButton { onPress = Just PanDown, label = text "↓" }
                ]
            , Input.button attrsButton { onPress = Just PanRight, label = text "→" }
            ]
        , column
            [ spacing 10
            , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
            ]
            [ text <| String.fromFloat model.zoom
            , text <| String.fromFloat model.panX
            , text <| String.fromFloat model.panY
            ]
        ]


mouseMoveDecoder : Decode.Decoder Msg
mouseMoveDecoder =
    Decode.map4 MouseMove
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
        (Decode.at [ "currentTarget", "clientWidth" ] Decode.float)
        (Decode.at [ "currentTarget", "clientHeight" ] Decode.float)


dragStartDecoder : Decode.Decoder Msg
dragStartDecoder =
    Decode.map DragStart
        (Decode.at [ "currentTarget", "clientWidth" ] Decode.float)


dragMoveDecoder : Decode.Decoder Msg
dragMoveDecoder =
    Decode.map2 DragMove
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


secondTouchDecoder : Decode.Decoder (Maybe ( Float, Float ))
secondTouchDecoder =
    Decode.maybe
        (Decode.map2 Tuple.pair
            (Decode.at [ "touches", "1", "clientX" ] Decode.float)
            (Decode.at [ "touches", "1", "clientY" ] Decode.float)
        )


touchStartDecoder : Decode.Decoder Msg
touchStartDecoder =
    Decode.map4 TouchStart
        (Decode.at [ "currentTarget", "clientWidth" ] Decode.float)
        (Decode.at [ "touches", "0", "clientX" ] Decode.float)
        (Decode.at [ "touches", "0", "clientY" ] Decode.float)
        secondTouchDecoder


touchMoveDecoder : Decode.Decoder Msg
touchMoveDecoder =
    Decode.map3 TouchMove
        (Decode.at [ "touches", "0", "clientX" ] Decode.float)
        (Decode.at [ "touches", "0", "clientY" ] Decode.float)
        secondTouchDecoder


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        dragSubs =
            if model.dragging then
                [ Browser.Events.onMouseMove dragMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed DragEnd)
                ]

            else
                []

        inertiaSub =
            if model.dragging || abs model.velX > 0.01 || abs model.velY > 0.01 then
                [ Browser.Events.onAnimationFrame (\_ -> AnimationFrame) ]

            else
                []
    in
    Sub.batch (dragSubs ++ inertiaSub ++ [ Browser.Events.onResize Resize ])


uk : Model -> Model
uk model =
    { model | zoom = 3.3, panX = -519, panY = -973 }


us : Model -> Model
us model =
    { model | zoom = 1, panX = -1095, panY = 158 }


ja : Model -> Model
ja model =
    { model | zoom = 2.9, panX = 5239, panY = 226 }


reset : Model -> Model
reset model =
    { model | zoom = 0, panX = 28, panY = 165 }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


svg : Float -> Float -> Float -> Float -> Float -> Html msg
svg zoom panX panY containerWidth containerHeight =
    -- Converted with https://html-to-elm.com/
    let
        baseWidth =
            2000

        baseHeight =
            1280

        scale =
            2 ^ zoom

        mapAspect =
            baseWidth / baseHeight

        containerAspect =
            if containerHeight > 0 then
                containerWidth / containerHeight

            else
                mapAspect

        -- Fill height when portrait, fill width when landscape
        ( zoomedWidth, zoomedHeight ) =
            if containerAspect < mapAspect then
                -- Portrait: constrain by height, crop left/right
                let
                    h =
                        baseHeight / scale
                in
                ( h * containerAspect, h )

            else
                -- Landscape: constrain by width, crop top/bottom (original behavior)
                let
                    w =
                        baseWidth / scale
                in
                ( w, w / containerAspect )

        offsetX =
            (baseWidth - zoomedWidth) / 2 + panX / scale

        offsetY =
            (baseHeight - zoomedHeight) / 2 + panY / scale

        viewBoxValue =
            String.join " "
                [ String.fromFloat offsetX
                , String.fromFloat offsetY
                , String.fromFloat zoomedWidth
                , String.fromFloat zoomedHeight
                ]

        style1 =
            String.join ";"
                [ "fill:orange"
                , "fill-opacity:1"
                , "fill-rule:evenodd"
                , "stroke:none"
                , "stroke-width:0"
                ]

        style2 =
            String.join ";"
                [ "fill:none"
                , "stroke:brown"
                , "stroke-width:" ++ String.fromFloat (2.5 / (scale ^ 0.547)) ++ "px"
                , "stroke-linecap:round"
                , "stroke-linejoin:round"
                , "stroke-dasharray:none"
                , "stroke-opacity:1"
                , "stroke-miterlimit:4"
                ]
    in
    -- Source: https://www.fla-shop.com/svg/
    Svg.svg
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.viewBox viewBoxValue
        , Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        ]
        -- [ Svg.rect
        --     [ Svg.Attributes.x "0"
        --     , Svg.Attributes.y "0"
        --     , Svg.Attributes.width "2000"
        --     , Svg.Attributes.height "1280"
        --     , Svg.Attributes.stroke "red"
        --     , Svg.Attributes.strokeWidth "2"
        --     , Svg.Attributes.fill "none"
        --     ]
        --     []
        [ Svg.path
            [ Svg.Attributes.style style1
            , Svg.Attributes.d "m 602.4,1259 2,4 h -7.3 l -2,-4 z m 5.7,0 0.8,4 h -4.5 l -0.8,-4 z m 378.3,-4 z m -214.6,-4 4.5,4 h -5.7 z m -189.3,-4 h -5.7 -0.8 3.6 z m 779.5,-4 h -2 l 1,-5 z m -764.9,-9 1.6,4 h 2.9 1.6 l 2.9,9 2,4 6.1,4 h 2 l -2.8,4 h -2.5 l -9.3,-4 -2.9,4 h -2.8 -3.3 l -7.3,-4 4.5,-4 h 2.4 l 1.3,-4 h 3.6 v -4 h -4.9 l 1.3,-5 z m 53.6,-8 -3.2,4 h -2 l 2,-4 z m 2.1,8 h -1.6 -2.1 l 3.7,-8 h 5.7 l 0.8,4 z m 684.2,-28 v 0 h 2 l 1,4 3,-4 h 2 l -1,4 h -3 v 5 l -4,-5 h -1 l -1,-4 z m -767.5,0 0.8,9 h -3.7 v -5 z m 5.7,-44 h -0.8 -3.7 0.8 v -8 h 2.9 l 1.6,4 h -2 z M 1744,1145 h 4 7 5 v 9 l -3,8 h -3 l -1,4 h -6 l -3,-8 -1,-4 -3,-9 z m 145,0 3,5 4,-5 3,9 -6,8 h -2 v 4 l -8,4 -1,4 -3,8 -5,4 -4,4 -3,-4 h -9 l -2,-4 5,-8 6,-4 7,-4 8,-8 1,-4 2,-4 2,-5 z m 12,-28 h 3 3 v 8 h 7 l 4,-4 2,4 v 4 l -2,4 h -4 l -2,8 -4,4 -4,5 h -3 l 3,-9 -1,-4 h -7 v -4 h 4 l 2,-8 -2,-8 -3,-4 -5,-4 2,-4 5,4 v 0 l 2,4 z m -36,-49 h -1 z M 282.8,1041 v -1 z m -168.2,-19 h 0.7 z m -81.92,0 v 0 H 31.95 Z M 1862,1021 v 1 h -2 l 1,-2 z m -596,1 -1,1 -2,-1 v -1 l 1,-1 1,1 z m 593,5 v 2 l -3,-2 -3,-2 -2,-1 -2,-2 h -1 l -2,-2 -1,-2 v -1 h 1 l 3,2 1,1 2,2 2,1 4,3 z m -583,-9 h -1 -1 l 1,-3 h 1 v 1 z M 61.17,1010 h -0.73 v 0 l 0.73,-1 z m 1809.83,-2 1,1 h -2 z m -1700.9,-7 1.5,2 -2.2,-1 z m 1697.9,1 h -1 -1 l 1,-1 z m 51,3 h -1 -3 l -1,-2 2,-1 v -1 l 1,-1 h 2 l 3,2 v 2 1 z m -723,-6 v -1 z m 731,-4 -2,2 v 0 h -2 -1 l 2,-2 h 2 l 1,-1 h 2 z m -63,-1 -2,2 v 0 -3 z m -926,-2 h -0.8 z m 921,-5.5 v 1.3 h 1 l 1,2.2 -2,1 -1,-1.6 v -0.8 l -1,-2.1 z M 56.78,983.3 h -0.73 l 0.73,-0.4 h 0.73 -0.73 z m -40.2,-0.4 0.73,0.4 h -0.73 z m 33.62,-2.4 2.19,1.6 -2.19,-0.8 z m -1.47,-1.3 v 1.3 h -2.19 v -1.3 L 48,978.4 Z M 1209,974.8 v 1.2 z m 28,12.2 1,0.8 -1,2.2 -1,2 -1,-1 -1,-1.6 -1,0.6 1,4 v 2 l -1,2 -1,2 v 2 l -1,3 -2,8 -1,2 v 2 l -2,5 -1,3 -1,4 -1,3 -1,5 -1,3 -1,4 -3,1 h -2 -2 l -2,2 h -2 l -2,-2 -4,-1 v -2 l -2,-2 -1,-7 v -2 l -1,-3 v -3 l 1,-1 v -2 h 2 l 1,-2 v 0 l 3,-5 -1,-2 1,-1 -2,-2 v -1 l -1,-3 v -4 -2 l 2,-3 1,-2 v -1 h 2 l 1,-2 h 2 l 2,-1 h 2 l 1,-1 4,-2.2 v -1.3 l 2,0.5 1,-2.1 v -2 h 1 v -2.4 -1.3 h 2 l 2,-0.8 1,-3.6 -1,-1.7 3,-1.2 2,0.4 v 1.6 l 1,0.9 1,4.8 1,3.7 v 1.6 z m 248,-15.1 v -0.8 z m -285,-3.2 v 1.6 l -1,-0.8 v -2.1 h 1 z m 534,11.8 2,2.8 4,-0.4 4,3.6 1,8.5 3,7 -1,3 2,3 7,6 6,2 v 4 l 3,2 2,7 6,2 1,4 3,3 5,5 3,6 1,10 2,4 -2,17 -3,9 -5,4 -4,8 -2,4 -3,8 v 4 4 l -3,4 h -6 -5 l -3,4 -5,4 -4,-4 -1,-4 -8,8 -5,-4 h -5 l -6,-4 -4,-4 v -4 l -3,-8 h -3 l -1,-4 -1,-4 -4,4 1,-8 h 1 l -1,-4 -3,4 -4,4 -3,4 h -3 v -4 l -5,-8 -1,-4 -8,-4 -2,4 -6,-4 h -11 l -9,4 h -7 l -11,4 -3,8 -8,-4 -11,4 h -4 -1 l -5,4 h -8 l -5,-4 h -3 v -4 h 4 v -12 l -4,-8 v -6 l -4,-8 -1,-4 -2,-5 2,-1 v -5 l -4,-8 2,-5 v -5 l 4,-1 1,-3 4,-2 7,-5 4,1 19,-7 3,-3 1,-3 3,-2 -1,-5 3,-3 h 5 l 4,-6 6,-7.7 6,-0.4 2,-3.3 5,5.3 9,2.1 -1,-3.7 2,-4.1 2,-1.6 1,-3.7 4,-1.6 h 5 l 3,-1.2 1,-2.8 11,2 9,1.6 v 4.9 l -4,2.4 v 2.9 l -2,4.4 8,5.5 4,2 3,3 h 4 l 2,3 3,2 5,-1 3,-8 1,-6 -1,-6.5 2,-2.9 -1,-4.5 1,-2 2,-6.1 h 3 l 1,4.5 2,0.8 1,5.3 z m -202,-18.3 v 0 -0.8 z m 302,1.6 h -2 l -2,-1.6 v -1.3 l 2,1.3 h 2 z m -222,-2.9 h -3 l -3,-2.4 h -2 l 1,-2 3,-0.8 5,3.6 z m 208,-5.2 2,0.8 h 2 l 2,0.8 1,1.2 -1,0.8 -2,-0.8 h -3 l -1,-1.2 z m -1769.8,0 v -0.8 z m 1581.8,5.2 -3,0.5 h -2 l 1,-3.7 2,-1.2 3,-0.8 2,-1.7 1,-2 4,-0.8 h 4 l -1,2.8 -6,1.7 -1,0.8 z m 293,-9.7 v 0.8 l -1,-0.8 z m -96,3.7 v 1.6 l -2,-1.6 -1,-3.7 v -0.8 l 1,1.6 v 1.6 z m -212,-2.9 -3,2 -6,-1.2 2,-2.4 h 4 z m -27,0.8 -3,0.8 1,-2.4 1,-1.2 2,1.2 z m 14,-0.8 -11,2.9 -1,-2.9 2,-1.6 3,2.4 2,-2.8 5,0.4 z m 32,-1.6 -4,0.8 1,-1.2 z m -51,0.8 -2,1.6 -3,-1.6 3,-2 z m 226,0 -1,0.8 -1,-1.6 -2,-0.4 1,-0.8 h 2 z m 10,-0.8 v 1.6 l -4,-2.8 -2,-1.7 1,-0.8 2,1.7 1,0.8 z m -114,0 -4,0.8 2,-4.5 5,-1.2 v 1.2 2.5 z m -352,-4.9 v 0.8 l -1,-0.8 z m 452,-1.6 2,2.4 -3,-1.6 -1,-1.6 -2,-1.2 1,-0.9 3,2.1 z m -273,-6.5 4,1.6 3,-0.8 2,1.2 h 3 l 2,3.3 9,0.4 3,-2.9 2,1.7 h 1 l 4,1.2 h 2 l 2,3.6 2,0.8 4,-0.8 2,1.7 v 3.6 h -4 l -2,-1.6 -4,0.8 -4,-0.8 -6,-0.4 -3,-1.6 -4,-1.7 -5,0.8 -5,-1.2 -5,-0.8 v -1.6 l -3,-1.2 -1,-2.5 z m 266,2.8 -1,2.5 -2,-0.8 v -2.5 l -3,-1.2 2,-2.4 2,2.4 z m -21,-6.5 v 2.1 l -7,3.6 h -5 l -7,-2.8 v -1.2 l 5,0.4 6,-0.4 h 3 l 1,-2.5 3,-0.8 -1,-3.7 4,0.9 1,2.4 z m -513,-2 h -2 l -1,-2.4 1,-0.9 z m 93,-4.5 v 0.8 0 z m 427,1.2 -3,1.7 -1,-3.7 z m -139,-3.6 -2,1.6 -4,-2 v -1.6 l 4,-0.9 2,1.7 z m 13,-4.1 6,2.1 1,2 -1,1.6 -4,-2 h -5 -3 l -2,-3.3 6,0.8 z m -303,0 h -1 z m 189,0.4 -1,2.5 h -2 v -3.7 z m -8,-0.4 -1,1.2 -4,-1.2 -1,-3.2 h -2 v -2.9 h 4 l 1,3.7 z m -182,-6.1 h -1 l 1,-0.8 z m 140,-3.6 2,4.4 -2,-0.8 -1,-2 z m -135,-1.2 -1,0.4 v -0.4 z m -874.8,-0.9 -0.8,1.3 -0.8,-0.4 0.8,-0.9 z m -372.4,-0.8 h -0.7 z m 1565.2,0.8 1,0.9 h 3 l 2,3.6 -1,1.2 1,3.7 2,1.6 1,2 1,2.5 h 2 l 5,-4.5 v -2.4 l 3,0.8 1,-2.9 3,-1.6 5,1.6 6,2.9 h 2 l 5,1.6 11,4.5 h 2 l 3,2 h 3 l 2,2.9 4,2.4 v 3.7 l 4,0.4 3,2.4 h 2 l 3,3.7 h -5 l 2,4.4 3,2.9 1,2.8 10,8.1 v 2.1 l -5,-1.3 h -6 l -2,-1.6 -1,-2 -6,-6.1 -1,-2 -8,-2.5 -2,0.8 -5,4.5 1,2 -5,2.5 -2,-1.6 -4,0.8 -2,-0.8 -6,-4.9 h -5 v -1.6 -2.1 l -1,-1.6 1,-2 -3,-3.3 -2,-4 -4,-2.5 -2,-0.8 -6,-2 h -3 l -5,-2.9 -4,-2.4 -2,3.2 h -2 l 1,-3.2 -2,-2 1,-1.7 2,-2 -6,-2 -1,-1.7 -1,-1.6 -4,-0.4 2,-3.2 3,-0.4 3,-1.7 z m -321,-1.6 h -1 v -0.8 z m -652.4,-0.8 4.1,0.8 -0.4,2.9 -2.5,3.6 -8.5,1.6 -1.6,-1.6 v -4 l 1.6,-3.3 4,0.8 z m -223.1,4.5 -3.2,0.8 v -1.6 l 1.6,-1.3 -0.8,-1.6 -0.8,-1.2 h -0.4 l 1.2,-0.8 0.8,0.8 v 1.2 l 1.6,0.8 0.4,1.7 z M 26.1,904.5 v 0.8 h -0.73 z m 977.9,0 -1,1.6 -1,-0.8 1,-1.6 z m 359,-2.1 -1,0.4 -1,-0.4 1,-0.8 z m 160,-3.6 -1,0.4 h -1 v -0.4 h 1 z m -162,0 v 0 -0.8 z m 274,2 -3,2.9 -4,0.8 -3,-0.8 -7,-0.9 -4,0.9 -2,-0.9 -3,2.5 -1,2 2,3.7 2,2.4 2,0.4 3,-2.8 h 3 v -0.8 h 4 l -2,3.6 -3,0.8 -2,2.9 2,0.8 3,4.5 -1,2.8 2,0.8 v 2.9 l -3,0.8 -1,2 -2,-1.2 V 928 l -3,-3.2 v -4.1 l -2,-0.8 -2,2 1,1.7 v 7.3 l -1,4.5 -2,1.2 -3,-1.2 1,-5.3 v -2.9 l -1,-2.8 -2,0.4 v -4.1 l 2,-4.4 v -2.9 l 2,-3.2 1,-7.8 4,-0.8 1,-2.8 10,2 6,0.8 3,-0.8 3,-3.6 1,0.8 z m 17,-2.8 -3,2.8 3,-1.6 v 3.2 l 3,2.1 -6,-1.7 -1,-3.6 1,-2.9 z M 128.4,896.3 v 0.9 l -1.4,-0.9 h 0.7 z m 1228.6,-1.2 -1,0.4 v -0.4 0 z m 1,-4.4 v 0.8 h -1 v -0.8 z m 3,-4.5 v 0.8 h -1 v -0.8 z m -345,0.8 -2,2 -1,-0.8 1,-2 h 2 z m 343,-5.3 v 0.8 z m 121,-4.4 4,0.8 h 5 l 2,1.6 2,4.5 5,2.8 5,4.5 1,2 h 2 l 3,2.8 4,2.5 v 2 l 7,2 1,1.7 -1,5.3 1,1.2 4,0.8 1,4.5 2,2 4,0.8 2,4.5 v 2.8 l -2,11 -2,-2 -4,2 -5,-5.3 h -1 l -6,-4.9 v -1.6 l -4,-2 -4,-6.1 v -1.2 l -2,-6.1 -3,-2.9 -1,-1.2 -3,-1.6 -2,-6.5 -1,-2.5 -5,-2 v -2.8 l -4,-4.5 h -1 l -6,-5.3 -2,-4.1 2,-0.8 z m 124,1.6 -5,1.6 2,2 -5,1.7 1,2.8 h -3 l 2,4.5 2,2 -1,2.8 4,2.5 2,2.8 h -4 l -2,0.8 -2,2.9 v 3.6 l -2,3.7 -3,1.2 v 6.1 l -3,4.5 -1,1.2 -6,2.4 -1,-3.2 -4,-0.4 -3,-2.5 -3,2.5 h -4 v -2.5 h -8 l -1,-3.6 v -4.9 l -4,-3.2 -2,-2.9 -1,-5.7 1,-3.6 4,-2.9 1,2.1 6,0.8 1,-5.3 2,-2 7,-1.7 5,-5.7 1,-1.6 h 1 1 v -0.8 l 1,-0.4 2,-0.8 v -0.8 0.8 h 1 l 2,-1.6 3,-4.9 2,-1.6 4,-0.9 2,0.9 1,2.8 4,0.8 z m 209,-10.2 -1,0.8 v -0.8 z m -453,0.8 -1,1.2 v -0.8 -1.2 z m 509,-3.2 v 0.8 z m -182,-0.4 -1,0.4 v -1.2 z m -285,7.3 -4,1.2 -2,-0.8 -2,-4.1 v -7.3 l 2,-6.9 1,-1.2 3,2 2,4.5 3,5.7 -1,3.6 z m 240,-15.5 1,5.7 1,3.7 -3,2.4 -3,-2.4 v 2.4 l 1,4.1 -1,2.4 -2,-1.6 -4,-1.2 -2,-1.6 v -2.9 l 1,-2.4 -1,-1.2 -3,-0.9 v 2.1 l -2,-2.1 -2,1.7 v -1.7 l -3,4.5 -1,-0.8 2,-5.3 4,-0.4 2,-3.2 4,3.2 3,-4.5 2,0.9 2,-0.9 v -3.6 z m -9,-6 -1,3.2 -3,-0.8 2,-2.4 z m -3,-1.3 -3,1.3 2,-3.7 z m -23,-0.8 -3,2.1 h -1 l 3,-3.7 z m -964.4,-1.6 -0.8,0.8 v 0.8 0.8 h 0.8 v 1.3 h -0.8 -0.8 -0.8 -0.4 -1.6 -0.9 0.9 l 0.8,-0.5 h 0.8 v -2.4 H 639 V 848 h 0.8 0.4 0.8 0.8 v -0.4 h 0.8 z m 983.4,8.1 -1,1.6 -2,-2.4 -1,-2.8 2,-0.8 1,-4.1 2,0.4 -2,4.5 z M 619.9,847.6 h -0.8 -0.9 0.9 v -0.8 z m 7.3,-0.8 -0.8,0.8 h -0.8 -0.4 -0.9 v -0.8 h 0.9 0.4 0.8 V 846 h 0.8 z m 16.2,-0.8 v -0.8 h 0.4 0.9 l -0.9,0.8 z m 992.6,2 v 1.6 l -2,-0.8 -2,-2.8 3,-1.7 z m -13,0 -4,1.6 v -6.5 l 6,2.9 z m -984.8,-6.5 v -0.8 l 0.8,-0.4 v 0.4 0.8 z m -34.6,-1.2 v 0.4 0.8 l -0.4,-0.8 v -0.4 h -0.8 0.8 z m -3.6,0.4 0.8,0.8 -0.8,-0.8 -0.4,-0.4 h -0.9 l 0.9,-0.8 v 0.8 z m 1038,3.2 1,2.9 -2,-0.8 -1,-2.9 -4,-2.8 -1,-1.6 h 5 l 2,1.6 z M 595.1,838.7 v 0.8 l -1.6,-0.8 0.8,-0.9 z m 659.9,-0.9 v 0 h 2 v 0 l 1,0.9 -1,0.8 -2,0.8 -2,-1.6 v -0.9 z m -604.3,-2 h -0.8 l -0.8,-0.8 v -0.8 h 0.8 v 0.8 h 0.8 z M 641.8,835 H 641 v -0.8 -0.8 l 0.8,0.8 z M 1741,833.4 v 1.6 l -1,-1.6 z m -124,4.4 -3,1.7 -3,-6.5 h 3 l 3,2 z m -974.4,-5.6 h -0.8 v -0.9 -0.8 l 0.8,-0.8 v 0.8 0.8 z m 0.8,-4.5 v 0.8 l -0.8,-0.8 h -0.8 l -0.8,-0.8 v -0.8 -0.5 h 0.8 v 0.5 h 0.8 v 0.8 z M 1746,824 v 0.8 -0.8 -0.8 h 1 z m -903.1,0.8 -0.8,0.8 -1.2,-0.8 0.4,-1.6 z M 641,824 h -0.8 v -0.8 l -0.4,-0.8 V 822 h 0.4 0.8 v 0.4 0.8 z m 0,-3.6 h -0.8 v -0.8 h 0.8 z m 0,-2.9 v 0.8 h -1.2 -0.8 l 0.8,0.4 H 639 v 0.9 l -0.8,-0.9 v -0.4 -0.8 h 0.8 v 0.8 h 0.8 v -0.8 -0.8 h 0.4 v 0.8 z m -4.9,-2.4 v 0.8 z m 2.9,-1.2 h -0.8 -0.9 v -0.8 h 0.9 0.8 z m 195,0 -1.2,0.8 v -1.6 h 1.2 z m -200.3,-1.7 v 0.9 l -0.8,-0.9 h -0.4 v -0.8 h 0.4 v 0.8 z M 630.8,811 H 630 Z m 7.4,-0.8 v 0.8 h -0.9 v -0.8 z m -9.4,0 v -0.8 z m -6.9,-0.8 h 0.8 v 0.8 h -0.8 -0.4 v -0.8 z m 10.6,-0.8 h 0.4 z m -14.3,-0.8 h -0.4 l 0.4,-0.4 h 0.9 v 0.4 z m 14.3,0 h -0.8 -0.9 0.9 v -0.4 h 0.8 z m -1.7,-0.4 0.9,-0.8 v 0.8 z m -8.1,-1.7 v 0.9 -0.9 h 0.8 z m 1.6,0 h -0.8 z m -13.8,0 h 0.4 0.8 0.9 0.8 1.2 0.8 0.8 0.8 l 0.8,0.9 v 0.8 H 617 l -0.8,0.4 v 0.8 h -1.6 -0.4 -0.8 -0.8 -0.9 -0.8 -0.4 -0.8 -0.8 v -0.8 l 0.8,-0.4 -0.8,-0.8 0.8,-0.9 v -0.8 z m -50,1.7 -0.8,2 -1.2,-0.8 h -0.8 -1.6 l -0.4,1.6 -0.8,-0.8 h -1.7 l -2,-2 h -1.6 l 0.8,-1.7 2,-0.8 5.3,1.7 z m 1057.5,22.3 2,0.8 2,-2 2,0.8 1,2.9 2,-1.7 2,4.5 2,0.8 -1,2.9 -3,-2.9 -1,-2.4 -3,-2.9 -3,1.7 -2,-1.7 -2,2.5 -4,-2.5 v -1.2 l 2,-1.6 -5,-1.6 -1,-5.7 3,-2.1 v -2.4 -2 -3.7 l 1,-4.5 3,-0.8 4,1.7 2,4.8 2,1.7 -2,2.8 -5,7.3 z m 240,-29.2 v 0.8 z m -1323,0 -0.9,0.8 -0.4,-0.8 z m 44.6,-3.7 5.3,1.6 4.1,-0.8 3.6,1.6 h 2.5 l 0.8,2.1 2.8,-0.8 4.5,3.6 -0.8,2.5 -7.3,-0.9 -3.7,1.7 h -2.8 l -2.5,2.8 -1.2,-2.4 -1.6,-0.4 h -4.5 l -2.8,-0.8 -2.9,0.8 -2.4,-0.8 0.8,-2.5 2.9,1.6 h 5.2 l 0.9,-2 -1.7,-0.8 0.8,-2.8 -3.6,-0.9 z m 978.4,6.9 -4,3.7 -5,-1.7 -1,-4.4 4,-3.7 7,-1.2 2,2.8 -2,1.3 z m -1419.3,-2.4 -2.2,1.6 -0.7,-0.8 v -1.6 l -0.8,-2.1 0.8,-1.6 v -1.2 l 3.6,1.2 0.8,2.4 1.4,0.9 -1.4,0.4 z m -4.4,-8.2 h -1.5 l -0.7,-1.2 h 1.5 l 1.4,0.8 z m 443.7,-2.8 -0.4,0.8 -0.8,0.8 h -0.8 -0.8 -0.8 v -0.8 -0.8 h 0.8 0.8 0.8 0.8 v -0.8 h 0.4 z m -453.2,-2.8 0.7,2 h -1.4 l -1.5,-1.2 z m 460.5,-0.9 h -0.4 -0.8 0.8 z m -1.2,-0.8 v 0.8 l -0.8,-0.8 z m -466.6,-1.2 -0.8,1.2 h -1.4 v -1.2 l 1.4,-0.8 z m 461.3,-1.6 0.8,0.8 h -0.8 L 578,783 v 0.8 h -0.4 V 783 h 0.4 z m -4.8,-1.2 v 0.4 0.8 l -0.9,0.8 h -0.8 l -0.8,0.8 v -0.8 l 0.8,-0.8 h 0.8 l 0.9,-0.8 h -0.9 l -0.8,-0.4 h 0.8 z m -1.7,0 h -0.8 l -0.8,-0.8 h 0.8 z m -31.3,-1.7 3.7,2.9 h 2.8 l 6.5,3.6 1.7,1.7 7.7,2 1.6,2.4 2.1,0.4 3.2,1.7 0.4,1.6 -3.6,1.2 -6.6,-0.8 -3.6,0.8 -3.7,-0.8 2.9,-2.8 -1.2,-1.3 -3.7,0.4 -2.4,-2 -1.3,-3.6 -2.4,0.8 -4.1,-0.8 -3.2,-2.1 -6.5,-0.8 -0.8,-2.8 h -3.7 l -4,2.8 h -3.3 l -2.8,2 0.8,-2.8 1.6,-2 4.1,-2.5 5.3,-0.8 8.9,0.8 z m 27.7,0.9 h -0.8 v -0.9 -0.8 h -0.8 v -0.8 -0.4 h -0.5 v -0.8 h -0.8 v -1.6 l 1.3,2.4 v 0.4 0.8 h 0.8 z m -5.3,-4.5 -0.4,0.8 v -0.8 h -0.8 v -0.8 h 0.8 z m 6.9,-2.9 v 0.9 -0.9 -0.8 l 0.4,0.8 z m -17.9,-0.8 v 0.8 h 0.8 v -0.8 h 0.8 v 0.8 0.9 0.4 l -0.8,0.8 h -0.8 v -0.8 h 0.8 -0.8 v -0.4 l -0.4,-0.9 0.4,-0.8 0.8,-0.8 v 0.8 z m 13.4,0 H 565 v 0.8 -0.8 -0.8 l -0.8,-0.8 v -0.4 h -0.8 V 770 h 0.8 v 0.8 l 0.8,0.4 v 0.8 z m -10.9,-5.3 -0.9,0.9 v -0.9 h 0.9 0.8 z m -2.5,2.5 v 0.8 l 0.8,0.4 h -0.8 -0.4 l -0.8,0.8 v -0.8 h -0.8 l -0.8,-0.4 h -0.8 l 0.8,-0.8 v 0.8 h 0.8 V 770 h -0.8 v -0.8 h 0.8 v -0.8 -0.9 -0.4 h 0.8 0.8 v 1.3 0.8 h 0.4 z m 1064.6,8.1 -3,5.7 -3,-1.6 -2,-3.7 v -2 l 5,-9 3,-1.2 2,2.9 z m -1057.7,-12.6 0.4,0.8 h 0.8 l 0.9,0.8 v 0.4 1.7 0.8 h -0.9 v -0.8 h 0.9 v -1.7 -0.4 h -0.9 v -0.8 h -0.8 -0.4 v -0.8 h -0.8 v -0.8 0.8 z m 679.7,-2.8 h -2 V 761 h 1 z m -687,-5.3 v 0.8 h -0.8 -0.8 -0.8 l -1.2,0.8 h -0.9 -0.8 l -0.8,-0.8 h 0.8 l 0.8,0.8 0.9,-0.8 0.4,-0.8 0.8,0.8 h 0.8 0.8 z m 0.4,-0.8 h 0.8 0.8 l 0.9,0.8 0.8,0.8 0.4,0.8 0.8,0.8 -0.8,0.4 -0.4,0.8 v 0.9 0.8 l -0.8,-0.8 h 0.8 v -0.9 -1.2 l 0.4,-0.8 h -0.4 l -0.8,-0.8 -0.9,-0.8 v -0.8 h -0.8 -0.8 -0.4 z m 319.8,-4.9 v 0.8 l -1.6,-0.8 h 0.8 0.8 v -0.8 z m 13,0 h -0.4 l -0.8,-0.8 h -0.8 l 0.8,-0.8 v -0.8 h 1.2 l 0.8,0.8 v 0.8 z m -8.5,-1.6 h -0.9 l -0.8,-0.8 h 0.8 v -0.4 l 0.9,0.4 z m 4.4,-2.9 v 0.8 l -0.8,1.3 -0.8,0.8 h -0.8 l -0.8,-1.2 V 748 h 0.8 1.6 v -0.8 l 0.8,-0.8 h 0.4 v 0.8 z m 11,2.1 -0.8,0.8 v -0.8 l 0.8,-0.4 V 748 l 0.4,-0.8 v -0.8 l 0.8,-0.8 h 0.8 v 0.8 1.6 l -0.8,0.9 z m -19.1,-2.1 -0.8,-1.6 v -0.4 h 0.8 l 0.8,0.4 -0.8,0.8 z m -503.9,-3.6 -0.8,2 -1.4,-0.4 0.7,-2.4 z m 525.8,1.6 h -0.8 v -0.8 l 0.8,-0.8 h 0.8 l 0.5,-0.8 v 1.6 h -0.5 z m -273,-21.1 v -0.8 h 0.8 z m 253.9,-3.7 h 0.9 1.2 l 0.8,0.4 -0.8,0.8 h -0.4 l -0.8,-0.8 h -0.9 z m 797.2,-0.8 -2,2.8 -2,7.4 -2,1.6 -1,-3.3 -3,-2.8 2,-2 v -2.9 l -5,1.2 V 718 l 6,-4.9 2,2 3,0.8 z m 11,-9.7 4,3.6 -9,3.7 -3,3.6 v -3.6 l 1,-4.9 4,0.8 z m -538,-1.3 h -3 l -3,-2.4 4,-2 5,-0.8 -2,4.4 z m -41,-2.8 -5,0.4 -2,-1.2 h -4 v -2.5 h 3 v 1.7 h 8 z m -60,-5.3 v 0.8 -0.8 -0.8 z m -211,-7.3 v 0.8 l -0.4,-0.8 z m -3.7,-5.7 h 1.7 0.8 0.8 v 0.4 h -1.6 -1.7 l -0.4,-0.4 z m 220.7,-3.2 -2,6.9 1,2 -1,2 -4,-1.2 -11,-6.1 2,-2.8 h 2 l 3,1.6 8,-0.8 z m -232.9,-0.4 h -0.8 -0.8 -0.8 V 683 h 0.8 0.8 z m -2.8,-1.7 v 0.8 h -0.8 l -0.8,-0.8 h 0.8 z m 160.9,-0.8 v 0.8 -0.8 l -0.8,0.8 v -0.8 z m -156.8,1.6 -2.1,-0.8 -0.8,-0.8 z m 3.6,-0.8 -1.2,-0.8 0.8,-0.8 0.4,0.8 h 0.8 z m 152.4,-1.6 -0.8,-0.4 v -0.8 h 0.8 l 0.8,-0.8 0.4,0.8 z m -173.5,-3.7 h -0.8 v -0.4 -0.8 l 0.8,0.8 z m 182.8,-2.8 h 0.8 0.9 v 0.8 0.8 h -0.9 v 1.2 l -1.6,0.9 v -0.9 h -1.2 v -1.2 h -0.8 -1.6 l 2.4,-2.4 h 1.2 z m 6.1,-1.2 v 0.4 0.8 l -1.6,-0.8 h -0.8 v -0.4 -0.8 h 1.6 z m 729.2,28.4 -2,3.7 -4,1.2 -1,-2 -2,1.6 h -3 l -2,3.6 -5,-0.8 -5,3.7 -3,4.5 -4,-3.3 v -2 l 2,-2.1 -4,-1.6 -4,2.1 -4,0.8 -5,0.8 -5,1.6 -2,-2.4 4,-2.1 6,-5.2 h 3 l 8,-1.3 3,1.3 7,-8.2 9,-3.6 3,-3.7 2,-2.8 4,-9.4 -2,-7.3 5,-3.6 4,2.8 2,2.8 1,5.3 -2,3.7 -1,4.5 -2,0.4 v 8.9 l -2,2.8 v 2.9 z m -701,-22.7 -5,1.6 -2,-2.4 1,-7.4 -2,-2.8 4,-1.6 2,-2.1 2,0.9 1,4.4 v 3.7 z m 0,-21.2 -2,5.7 -2,-1.2 -1,-3.2 v -2.9 l 3,-2.8 1,0.8 z m 725,-8.1 -2,2 -6,0.9 -2,4.4 -1,2.1 -8,-4.1 h -2 l -2,2 -3,-2 -1,2 4,2.1 -6,3.2 1,-3.2 -2,-2.1 v -2.8 l 6,-3.7 h 2 l 1,-3.6 1,-2.9 1,-5.3 -1,-3.6 2,-2 6,7.3 4,3.6 3,0.4 h 2 z M 644.7,631.8 h -4.5 l -0.4,-2.8 4.9,-3.2 z m 22.7,-10.9 v 0.8 -0.8 0.8 0.4 0.8 h -0.8 v -0.8 h 0.8 v -0.4 l -0.8,-0.8 v -0.8 h 0.8 z m 288.9,-17.5 0.8,0.4 z m -1.6,-1.6 h -0.4 V 601 Z m -317.4,0.8 -1.2,2 -7.3,-2.8 v -3.7 z m -331.1,0.8 2.2,7.3 -10.3,-5.3 -5.8,-3.6 -8.8,-9 0.7,-3.6 13.9,5.3 3.7,6.5 z m 362,-7.3 2.1,4.9 h 6 l 5.7,1.6 -1.6,4.9 v 7.3 l 6.1,3.6 -2.4,5.3 -9.4,-5.7 -1.6,3.7 -3.7,-6.1 h -11.3 l -6.1,0.8 v -2 l 2.4,-6.1 7.3,-16.7 2.9,-5.3 3.6,-2 2.1,3.7 z M 1892,570.9 v 2.4 l -2,-2.4 z m -160,52 -7,1.6 1,-3.6 v -4.5 l 1,-2.8 -2,-6.5 2,-6.1 -1,-8.6 2,-5.2 -4,-6.5 1,-9.8 v -2.8 l 6,-4.5 2,6.1 -1,11 2,3.6 2,10.2 2,6.5 -4,0.8 -2,2 -3,11 3,4.4 z M 80.18,565.2 l -5.12,1.6 0.73,-4.4 z m 183.52,2.9 -2.9,1.6 -2.9,-3.7 0.7,-5.3 6.6,1.7 z m 680.8,-8.2 -2,1.7 1.2,-2.9 z m -849,-4 -4.35,0.4 -1.46,-2.9 5.11,-0.8 z m 833.6,-6.1 v 2.4 l 3.6,-1.6 h 2.5 l 2.8,5.3 v 3.6 l -3.7,2.1 1.7,10.1 -1.7,4.5 -6.5,3.6 -6.5,3.7 -3.2,0.8 -5.7,-2.8 4.5,-6.9 1.2,-4.9 -4.1,-2.5 1.2,-1.2 -0.8,-6.5 7.4,0.4 v -4.8 l 1.6,-2.9 z m 95.9,-1.2 v 3.6 h -3 l -1,-4.5 2,-0.8 z m -756.1,0.4 -2.2,1.6 -2.2,-0.8 1.4,-4.9 z m 764.1,-0.4 -2,4 -2,-1.2 h -1 l -1,-4.5 3,-1.6 4,-2.8 1,0.8 v 2.8 z m -770.7,0.4 -2.2,3.2 -3.7,-1.6 0.8,-3.7 v -6.5 z m 675.7,-10.2 -3.7,2.4 0.9,-3.6 0.8,0.4 z m -680.1,-1.2 -3.7,2 -0.7,-6.5 3.7,0.8 z m -8.8,-0.8 -2.2,1.2 -2.2,-6.5 h 3 z m 686.1,-6.1 1.2,2.4 -2.1,-0.8 -2.8,-2.8 1.2,-1.7 h 2.5 z m 78.8,3.6 -2,2.5 V 536 l 2,-3.7 h 4 l 3,-4.5 h 2 l 1,3.7 -3,1.6 z m 54,-2.8 -3,0.8 -1,-3.7 3,-3.6 h 3 l -2,2 z m -914.7,-6.5 1.5,4.5 -10.3,5.6 -2.9,-4.4 2.9,-3.7 3,0.8 z m 99.5,3.6 -2.9,1.7 -1.5,-7.3 2.9,-0.9 1.5,3.7 z m -6.6,-2 -4.4,1.2 -2.2,-6.5 h 2.9 l 5.2,2.1 z m 687.3,-2.4 -2.8,2.4 v -5.3 l 4.5,-1.6 z m 153.5,-2.1 -2,2.9 1,-2 -1,-3.7 5,-1.6 -1,3.6 z m -135.2,-4.4 -0.8,2.8 -4.1,3.7 0.4,2.4 7.4,0.4 h 3.2 l 0.4,2.5 -2,5.6 -3.7,4.5 1.7,3.7 2.8,1.6 2,2.8 0.8,4.5 1.7,4.1 3.6,1.6 3.7,7.3 0.4,5.3 6.1,1.2 1.2,1.6 0.8,2.9 -0.8,2.8 -3.7,4.5 v 2 l 1.7,2.5 -1.7,1.2 -3.6,1.6 -6.5,-0.8 -4.9,1.6 -4.5,-0.8 -4.4,1.6 -0.8,2.9 -2.1,-1.6 -3.6,0.8 -2.5,2 -1.2,-1.2 4.9,-5.3 1.6,-2.8 3.7,-2.1 -1.7,-1.6 -2,0.8 -0.8,-2.4 -3.7,0.8 -0.8,-2 5.3,-2.9 0.8,-2.4 -0.8,-5.7 6.5,-2.5 v -2 l -3.6,-8.1 1.6,-3.7 -5.3,2.5 -4.5,-3.3 3.3,-4 -2.5,-3.7 h -1.2 l -1.6,-3.7 1.6,-3.2 h -3.2 l -1.2,-2 1.2,-0.9 1.6,-4.4 -1.6,-1.2 0.8,-3.7 2.8,-1.6 v -2.9 l 0.8,-3.6 2.5,0.8 z m 139.2,-2.1 -2,1.3 v -3.7 z m -15,-15.4 -1,2 -2,-2.8 z m -992.16,3.7 -5.12,0.8 -4.39,-3.7 6.58,-2.4 z M 932.3,481.1 v 0.8 l -1.6,-2.4 v -0.4 z m -387.6,-2.8 -1.6,4.8 h -3.7 l 1.6,-6.5 z m -18.3,-0.8 -5.3,2 0.9,-7.7 7.3,-1.7 1.2,2.9 z m -460.11,-9.4 -3.66,0.4 -7.31,-4.1 2.19,-3.2 z m 465.81,-11.8 7.3,3.7 -4.4,4.4 -8.2,-3.6 -4.8,-0.8 -7.4,7.3 -7.3,-11 -0.8,-6.5 2,-14.2 3.3,-2.8 7.3,9.7 9.4,5.7 z m 356.3,-26.4 1.3,1.6 -0.9,2.9 6.9,4.5 -0.8,2 0.8,3.6 -2.4,3.7 -4.9,6.5 -2.4,-0.8 -3.7,2.4 -2,2.1 -8.1,2.8 v 2.4 l -4.5,1.3 -8.1,-2.1 -2.9,-2.8 -8.1,-0.8 2.5,-2.1 -0.9,-4.4 -1.6,-2.5 -0.4,-2.8 h -6.1 l 2.5,-2 4,-0.9 -0.4,-2 1.2,-3.2 -3.6,-1.3 -2.1,0.9 h -6.5 l 5.7,-9 2.1,-3.6 h 3.2 l 2.9,2 3.6,6.1 -1.6,2 6.1,2.5 0.4,-3.3 -0.4,-4.1 4.9,4.1 v -4.1 l 2.8,-1.6 5.3,0.8 2.8,2.1 0.8,-2.9 2.9,0.8 1.6,-2.4 -0.8,-2 2.8,-0.9 1.7,3.7 z M 565,413.7 l -6.5,2.4 -3.6,-6.5 4.4,-8.9 7.3,0.8 0.5,8.5 z m 487,-13.9 -4,0.9 3,-5.3 v -2.1 l 5,-0.8 v 4.5 z m -2,-7.3 -1,4.5 -4,-0.8 4,-4.9 z m 183,0 -5,2.5 -3,-3.3 3,-7.7 3,-0.8 4,2.8 z m -169,-8.5 -1,4.1 -5,-1.3 4,-5.2 z m -606.7,8.5 -3.6,6.1 -6.5,-1.6 -9.4,-4.5 3.7,-10.1 3.7,-5.3 10.1,9.7 z m 1414.7,-13.8 -2,2.9 -6,-3.7 3,-2.8 z m -1184.7,5.7 -5.3,2.4 -2.8,-4.4 -5.3,-2.1 0.8,-7.3 2.9,-3.2 7.3,3.2 5.3,4.9 0.8,3.7 z m 603.7,-6.5 -4,2.4 -4,-3.2 -4,-5.7 4,-3.7 z m -462.7,-11.8 -9.3,2.5 4,-7.4 4.5,-0.8 2.8,-2.8 2.5,7.3 z m 1111.7,-7.7 -8,2 -3,-1.6 -7,3.7 1,-7.3 6,-4.9 h 9 l 4,4.4 z m -1466.4,20.3 2,2.9 3.7,-1.3 3.6,4.1 2.5,7.3 -1.6,6.9 2.8,2.9 2.4,-10.2 2.1,-4.5 6.1,4.9 2,8.1 -2.8,4.5 0.8,5.3 4.8,6.5 2.5,-2.8 4.9,-9 0.8,-9.3 4.4,-4.5 -2.8,-5.3 -0.8,-8.1 h 6.5 l 8.9,4.5 7.3,7.3 0.4,6.5 -3.6,3.6 -2.9,-0.4 2.5,7.7 4,6.1 -1.2,6.5 -8.1,5.7 -4.5,5.3 -8.1,-1.6 -0.4,-3.7 -6.1,0.8 3.7,4.5 -2.9,5.7 -5.3,5.3 h -2.8 l -9.4,-8.1 v 2.8 l 3.7,5.3 7.3,0.8 3.7,1.2 -5.3,12.6 -4.5,2 h -7.7 l 0.4,4.5 -3.7,0.8 0.9,5.7 -8.2,6.1 -7.7,11 -3.6,8.1 -2.5,10.2 v 8.5 l 1.6,3.6 h 7.4 l 3.6,14.7 -0.8,3.2 7.3,-2.4 12.6,3.6 4.9,4.5 2.4,3.7 9.4,3.6 2,2.1 4.5,1.6 2.8,-0.8 9,2.8 -0.9,6.9 1.7,4.9 -0.8,7.3 5.2,7.3 3.7,2.9 1.2,3.6 3.7,0.8 2.4,-3.6 2,-1.2 2.1,-5.3 -2.1,-4.5 -0.8,-9.3 -2,-7.3 8.1,-4.5 3.7,-2.9 3.6,-7.3 v -7.3 l -3.2,-9.7 -7.3,-6.5 6.9,-11.8 -5.3,-11.4 2.8,-3.7 -1.2,-5.2 -0.8,-8.2 2.8,-2 9.4,2.9 6.9,0.8 4.1,-2.9 8.1,7.3 2.8,6.5 8.2,2.1 2.4,4.4 v 12.6 l 1.2,7.3 3.7,-0.8 6.5,7.3 7.3,-6.1 3.2,-3.6 0.4,-6.5 3.7,-6.5 3.7,3.6 v 4.5 l 3.6,3.7 4.5,8.1 3.6,8.5 v 9.8 l 2.9,6.5 5.3,1.2 v 3.7 l 2.8,3.2 3.7,-1.6 1.6,3.6 5.7,1.7 -4.5,5.7 -7.7,2.8 v 3.7 l 11,-8.2 h 2.8 l 2,4.5 5.3,1.6 2.1,3.7 -1.3,3.6 1.3,5.7 -6.6,6.1 -4.4,-0.8 -4.5,2 -1.2,3.7 -6.1,4.5 -7.3,0.8 -13,-0.8 h -13.8 l -3.7,3.6 -1.6,4.5 -4.5,0.8 -4,3.7 -5.3,7.7 9.3,-6.5 8.1,-3.7 6.5,-1.2 4.5,0.4 2.5,4.5 -3.3,2.8 -5.7,2.5 1.6,2.8 4.1,-1.6 -0.4,3.6 2,8.2 5.7,4.4 5.3,0.8 2.8,-1.6 2.5,0.8 0.4,4.5 -8.5,2.8 -7.4,3.7 -3.6,4.5 -3.7,1.2 -2.4,-2 v -4.5 l 3.2,-2.9 3.7,-2.8 -2.4,-2.4 -7.4,3.2 -2.8,-1.6 v 4.5 l -7.3,2 -4.5,3.6 -3.6,0.9 -3.7,5.6 v 3.3 l 2.4,6.5 -6.1,2 -8.1,0.8 -6.5,5.3 1.6,1.2 -0.8,4.5 -3.6,5.3 -1.6,2.8 -2.9,4.5 h -3.7 l -0.8,3.7 2.5,2.8 1.2,7.3 -2.9,5.7 -7.3,3.7 -2,2.8 h -2.8 l -3.3,4.5 -9.3,7.3 -2.9,7.3 1.2,6.5 3.3,7.3 -0.8,1.6 3.6,8.2 v 6.5 l -2.4,4.4 -2.8,0.4 -1.3,-4 -2.4,-0.8 -1.2,-6.1 h -2.5 l -2,-5.7 1.2,-5.7 -5.7,-6.9 -2.8,-1.2 -6.1,2.8 -4.9,-5.3 h -4.4 l -3.7,1.7 -5.7,-0.8 -2.4,2.8 -7.3,5.3 -2.9,-0.8 -3.6,-3.7 -2.1,1.6 -7.3,-1.6 -4.5,1.6 -6.1,4.9 -7.3,4.5 -2.8,6.1 2.4,6.5 -0.8,2 -1.6,2.8 -0.8,4.5 -0.4,6.1 v 2 l -0.8,1.7 1.2,5.7 0.8,1.6 1.6,4 2.9,3.3 1.6,1.2 0.4,2.9 0.8,0.8 1.6,2.8 h 3.7 l 2,1.6 1.6,1.7 3.7,-0.8 1.2,-0.9 h 3.7 l 2.4,-1.6 h 2.9 l 0.8,1.6 h 1.2 l 1.6,-1.6 -0.8,-1.2 2.4,-1.6 0.4,-0.8 0.9,-2.1 1.6,-1.6 -0.8,-0.4 0.8,-3.7 v -1.6 l 1.2,-0.8 2.4,-0.8 2.9,-0.4 2.8,-0.8 2.9,-0.8 4.4,1.6 1.7,-0.8 0.8,0.8 v 1.2 l -0.8,2.4 -1.7,1.3 -0.8,1.6 -0.4,2 -1.6,1.6 h 2 l -2,6.5 h -2.8 l 1.2,0.9 v 1.2 l -1.2,2.4 0.8,1.2 -0.8,4.5 -0.9,1.6 -1.6,0.8 -0.4,2.1 1.2,0.8 0.8,-0.8 1.7,0.8 2.8,-1.6 0.8,0.8 h 0.4 l 2.5,0.8 2.8,-0.8 3.7,-0.8 1.6,0.8 2,-0.8 2.1,0.8 h 2.4 l 1.2,1.6 -0.4,0.4 0.4,0.8 3.3,0.8 1.2,0.8 -1.2,0.8 1.2,3.7 -1.2,1.2 -0.8,2.9 v 6.1 l -0.9,0.8 v 2 0.8 l -0.8,2.1 v 1.6 l 0.8,0.8 1.7,2 1.2,2.9 2.4,1.6 0.4,0.8 1.7,0.4 v 2.4 l 1.6,0.9 1.2,-0.9 1.6,1.3 h 1.2 l 2.5,-0.4 0.8,-0.9 2.8,-1.6 2.9,-1.2 h 2.8 v 0.4 h 2.5 l 2.8,0.8 2,2.5 1.7,1.2 3.6,2.8 v -2.8 l 4.5,-4.5 h 0.4 l 1.6,-5.7 2.9,-3.6 2.4,0.8 2,-1.6 h 4.5 l 9.3,-5.7 0.9,2.8 -2.9,1.6 1.2,4.1 -2.8,5.3 3.6,4.4 2.5,-1.6 v -2.8 l -2.5,-4.5 0.9,-2 7.3,-2.5 -1.7,-2 1.7,-2 2,4 h 3.7 l 2.4,1.7 1.2,3.6 10.6,-0.8 2,2.4 5.7,0.5 3.3,-2.1 7.7,-1.6 -0.4,2.4 2.8,3.7 2,-1.6 2.5,0.8 3.6,2 -3.2,5.3 3.6,-0.8 3.7,0.8 4.5,2 3.6,3.7 v 2.4 l 2.9,1.2 4.4,4.5 -0.8,2.4 0.8,-2.4 h 10.2 l 0.8,-0.8 6.1,1.6 -1.6,2.1 1.6,-2.1 4.9,1.6 5.3,4.1 2,3.7 2.8,0.8 v 3.6 l 3.7,7.3 2.4,0.9 v 2.8 l -4.4,5.3 -2.9,2 -3.6,7.3 6.5,-3.6 v 2.4 0.4 l 0.8,1.6 4.4,0.9 h 4.1 l 3.7,-2.9 1.6,-3.6 4.5,-0.9 3.6,2.9 9.4,2.8 2.4,3.7 2,1.6 2.9,-1.6 5.3,2.4 6.5,1.2 6.5,-0.8 8.1,4.5 7.3,6.5 2,0.8 h 3.7 l 3.2,0.8 1.3,2.1 2.4,8.9 v 3.7 l -2.4,5.6 -6.5,8.2 -2.9,2.4 -3.6,6.5 -3.7,4.5 -2.4,0.4 -1.3,3.2 0.8,2.9 -0.8,2.8 0.8,7.1 -1.6,7 -0.8,4 -1.2,2 -0.8,6 -3.7,8 -2.4,1 -1.2,4 0.8,3 -5.3,3 -1.6,2 h -7.3 -1.3 -4.4 l -0.8,2 -5.7,3 -6.1,3 -5.7,4 -2.9,5 v 11 l -1.6,5 -2.8,2 -3.7,6 -0.8,3 -2,1 -2.5,-2 -2.8,5 H 691 l -1.7,8 -2.8,9 h -3.7 l -3.6,8 h -3.7 -5.2 -2.1 l -5.3,-4 h -3.6 -2.9 l 0.8,-13 -2.4,9 0.8,4 6.1,8 v 0 l 1.2,4 2.5,4 v 0 l -4.5,8 v 0 l -8.1,4 -9.4,4 h -6.9 v 4 l -2,4 0.8,4 h -4.5 -3.6 -6.5 -0.8 l 1.6,8 h 2 4.5 v 9 h -5.3 l -3.6,4 0.8,4 -2.9,4 -6.5,4 -3.6,8 1.2,4 h 3.6 l 5.3,4 v 0 l -1.6,4 -5.7,4 -3.6,8 v 4 l -6.1,4 -1.3,4 0.9,8 2.8,4 h -3.7 -3.6 l -5.3,4 -0.8,9 h -2 -4.5 l -1.2,-4 h 5.7 v -5 l -5.7,5 H 578 v -5 l -3.6,-4 h 0.8 l -3.7,-12 h 0.8 l -1.6,-8 v -12 h 1.6 l 0.8,-4 h -4.4 l 3.6,-8 h -1.6 -3.6 l -2.1,-4 3.7,-4 h 0.8 2.8 l 1.7,4 1.2,-4 0.8,-8 h 3.6 l 0.9,-4 h -2.9 l 0.4,-4 -0.4,-4 h 2 l -0.8,-9 h 1.7 l -1.7,-4 -2,4 h -2.4 l -1.3,-8 1.3,-8 h 1.6 l 0.8,-4 -1.6,-4 -0.8,-8 h 2.4 l 1.2,-4 2.5,-8 2,-4 0.8,-4 2,-4 1.7,-9 -1.7,-8 v -4 l 1.7,-3 -0.9,-6 1.7,-3 1.2,-8 h 0.8 l 2.4,-11 -0.8,-3 v -5 -5 l 1.2,-1 0.9,-8 -0.9,-4 0.9,-4 -1.3,-10 -5.2,-3 -0.9,-3 -19,-9.6 -4.1,-4.5 -2.5,-2 0.9,-4.5 -5.7,-9.7 -2.5,-2.1 -5.7,-14.6 -2.8,-3.6 -2.4,-4.5 -2.1,-2 -6.1,-2.9 1.7,-3.2 -2.1,-4.9 0.4,-2.5 4.5,-4 0.8,-3.7 -3.6,-2.4 0.8,-3.7 1.2,-6.5 3.2,-2.8 -0.8,-3.7 6.5,-2.4 0.8,-0.4 -0.8,-2.5 1.7,-3.6 4,-1.2 3.7,-5.3 -1.2,-1.6 v -2.9 l -0.9,-8.5 0.9,-2.5 -2.9,-3.6 -1.6,-1.2 -1.6,-3.3 h 0.8 l 0.8,-1.2 -1.6,-0.8 v -0.8 l -2.1,-1.2 -1.6,-0.9 h -1.2 l -1.6,1.3 -0.8,0.8 -2.1,1.6 h -0.8 l -0.8,1.2 2.4,2.5 -1.6,2 h -2.8 v -1.2 l -3.7,-1.7 -0.8,-2 h -2 l -0.8,-0.8 h -2.9 v 1.2 l -0.8,-0.4 -2.8,-1.6 -0.9,-0.8 0.9,-1.2 -0.9,-0.9 -1.6,-1.6 -1.2,-0.8 -2.4,-0.4 -0.4,-2.4 -1.7,-0.8 -0.8,0.8 1.6,0.8 v 0.8 l -1.6,0.8 -0.8,-1.6 -1.2,-0.8 -0.8,-0.8 -0.8,-1.3 1.6,-1.6 -0.8,-2 -0.8,-1.6 -3.7,-2.1 -1.2,-2.8 -0.8,-0.8 -3.7,-2.9 v -0.8 h 2 l -2,-1.6 -0.8,-0.8 -0.8,1.6 h -4.5 l -2.8,-1.6 -2.9,-0.4 -1.6,-0.8 -2.8,-1.7 -2.1,0.8 -1.6,-0.8 -2,-1.2 -2.9,-1.6 -5.3,-5.3 -2.8,-2.8 -0.8,-0.9 -1.6,-0.4 h -2.1 l -1.6,-1.6 -2.8,2.9 -1.3,0.8 -2.4,0.8 h -1.2 -2.5 l -1.2,-0.8 -3.6,-0.8 -1.7,-1.3 -2.8,-0.8 -0.8,-0.8 -5.3,-1.6 -0.4,-0.4 -6.9,-2.5 -2.8,-2 -1.3,-1.6 h -1.6 l -2.8,-0.8 -4.5,-1.2 -0.8,-2.5 -3.7,-2 -1.2,-0.8 -2.4,-0.8 -2.9,-3.7 -0.8,-2 2.4,-0.8 v -1.3 h -1.6 l 2.1,-2.4 v -1.2 l -1.3,-1.6 -0.8,-1.7 v -2 l -1.6,-2 -2.8,-3.3 -2.1,-2.8 -1.6,-1.2 -2,-2.5 -1.7,-1.2 -1.2,-0.8 -1.6,-2.9 -2.2,-1.6 h -1.4 l -2.2,-2 v -1.6 l 1.4,-2.1 -1.4,-1.6 -2.2,-0.8 -0.7,-1.2 -2.2,-0.8 -0.8,-1.7 -0.7,-1.2 -2.2,-1.6 -2.2,-2.9 -1.4,-0.8 -0.8,-2 -1.4,-0.8 v -1.6 l -2.2,-2.9 -0.8,-1.6 v -1.2 l -2.2,-3.7 v -2.8 l -2.9,-0.8 v -1.7 h -2.9 l -2.2,-1.2 h -0.7 l -0.8,4.5 0.8,0.4 0.7,2.8 v 2.5 l 1.5,2 h 0.7 l 2.2,2.4 1.4,2.1 2.2,2 v 1.6 l 1.5,0.8 0.7,2.1 v 0.8 l 0.8,1.6 1.4,0.4 0.8,1.6 1.4,2.9 1.5,0.8 1.4,2 v 2.5 l 3.7,6.5 v 2 l 1.5,1.6 0.7,-0.8 2.2,1.6 0.7,2.1 1.5,1.6 v 1.2 l -2.2,1.6 H 380 v -2 l -0.7,-1.6 -2.2,-0.8 -2.2,-2.1 -2.2,-1.6 -2.2,-2 -0.7,-0.8 -0.7,-0.8 0.7,-2.1 -0.7,-2.4 -0.8,-2.9 -2.2,-0.8 -2.9,-2.8 h -1.5 l -1.4,-1.2 -3.7,-1.7 v -0.8 l -2.2,-1.2 3,-1.6 1.4,-0.8 v -2.9 l -4.4,-5.2 -1.4,-0.5 -3,-2.4 -0.7,-3.6 -0.7,-1.3 v -0.8 l -1.5,-1.6 v -1.2 l -2.2,-2.5 0.8,-1.2 -1.5,-1.6 -0.7,-1.2 -0.8,-1.7 -2.2,-5.2 -5.8,-4.9 -5.1,-2.4 h -4.4 l -1.5,-4.1 -6.6,-8.1 v -4.5 l -2.9,-1.6 -3.6,-8.6 -3,-2.8 -0.7,-6.9 -2.9,-2.9 1.4,-7.3 -2.2,-10.9 1.5,-6.5 1.5,-10.2 v -11 -2.8 l -3.7,-9 2.9,-1.6 9.5,2.5 -0.7,-6.5 -1.5,-1.7 -2.9,-1.6 -8,-6.5 0.7,-2 -3.6,-2.5 h -4.4 v -3.6 l -4.4,0.8 -4.4,-2.8 -0.7,-9 -5.9,-12.2 -2.2,1.2 -3.6,-4.8 -0.8,-6.5 1.5,-5.3 -2.2,2.8 -2.9,1.7 v -9 l -5.2,-4.9 -5.1,-7.3 -3.6,-1.6 -0.8,-6.5 -2.2,-2.8 -0.7,-0.9 -2.9,-2.8 h -3.7 l -6.5,2 -8.8,-8.9 -10.3,-6.5 h -3.6 l -9.5,-3.7 -6.6,0.8 -12.4,-6.5 -4.4,-4.4 -5.9,5.3 -1.4,6.5 h -6.6 l -3,4.4 -8,3.7 -0.7,-6.9 2.2,-4.1 v -5.3 l 5.8,-3.6 -1.4,-2.9 -4.4,2.9 -4.4,5.7 -1.5,4.4 -8,7.3 4.4,6.1 -4.4,3.7 -0.8,3.7 -10.9,6.5 -1.5,4.4 -5.1,1.2 -5.9,8.2 -7.3,2 -4.4,3.3 -5.1,0.4 -2.9,3.6 -3.7,-1.2 6.6,-6.5 3.7,-0.8 6.6,-6.1 4.3,-2 6.6,-9 1.5,-7.3 -5.9,-4.1 -2.2,4.1 -2.9,-4.9 -5.1,-1.6 -7.3,3.7 0.7,-8.6 -4.4,-5.3 -5.8,2.1 -7.34,-5.7 -1.46,-2.9 4.39,-2.4 -8.78,-7.7 v -3.3 l 7.32,-9.3 0.73,-5.7 2.19,-1.6 4.35,2.8 3.7,-2.8 2.2,-3.7 5.8,0.8 2.2,-2.4 -0.7,-6.5 -2.9,-2 2.9,-5.3 -6.6,1.6 -2.2,2 -6.55,-0.8 -6.59,1.7 -5.85,-1.7 -2.92,-4.8 v -4.5 l -4.39,-0.8 -2.93,-2.9 13.17,-9.3 5.85,-3.3 3.66,-0.4 0.75,7.3 9.5,0.4 1.5,-5.6 7.3,0.8 v -2.9 l -8.8,-1.6 -2.9,-3.7 -7.35,-1.6 -1.47,-7.3 -13.16,-10.2 1.46,-7.3 10.97,-1.2 5.85,-6.9 0.7,-5.7 5.9,-8.9 5.8,-0.4 5.9,-6.9 8.8,-0.4 6.5,-7.4 9.6,2.1 2.1,5.3 7.4,-1.7 4.3,0.8 -0.7,4.9 18.3,0.8 8,5.3 9.6,0.8 5.8,2.9 8,-2.5 5.2,3.7 6.5,3.7 10.3,2.4 2.9,3.6 10.3,5.7 3.6,-0.8 v -3.6 l 5.1,-3.7 h 3 l 0.7,-2.8 2.9,2.8 h 2.9 l 2.2,-4.1 5.1,-1.6 8.1,-5.7 4.4,2.1 3.6,3.6 3,-2.8 1.4,-10.2 5.1,5.7 2.2,7.3 3,4.5 2.9,1.2 1.5,-7.3 3.6,-2.8 2.2,10.1 h 3.7 l 2.2,-5.7 5.8,-0.8 5.9,2.9 3.6,3.6 8.8,3.3 3.6,2.8 7.4,2 2.2,-2 5.8,2 5.1,6.1 -5.8,6.5 4.4,3.7 10.2,0.8 7.3,-1.6 5.1,-2.9 2.2,3.7 5.9,1.2 3,4.5 1.2,5.3 2.9,-0.9 -3.7,-9.3 -2,-13.4 8.9,-2.9 2.1,-2 4.4,3.7 v 4 l 4.5,2.5 0.4,2.8 h 5.3 l 3.7,2.9 6.5,2.4 5.2,-2.4 12.2,0.8 4.5,2.8 -0.8,-5.7 -2.9,1.2 -2.8,-6.5 8.1,-1.2 6.5,4.9 5.7,0.8 6.9,-6.5 0.4,-5.3 -3.6,-2.8 V 384 l -7.4,-5.3 -4.8,-5.7 1.2,-6.9 -1.2,-3.6 v -7.8 l 6.5,-7.3 4.4,-2.4 6.5,6.5 2.1,5.3 -0.4,5.3 4,3.6 3.3,7.3 z m 1186.4,-41 -4,2.8 -7,-5.7 1,-3.6 z m -811.4,-4.5 -0.4,6.9 -10.6,-8.1 -0.4,-4.5 h 7.4 z m -489,-11 v 8.2 l 5.1,-5.7 9.5,4.9 0.7,6.1 5.9,-3.7 1.4,-5.3 5.1,5.3 3,11 2.9,3.6 -3.7,-24.8 8.1,-2.4 6.5,6.5 2.9,14.2 3.6,9.4 -1.2,8.9 3.7,4.9 15.8,9.7 v 7.3 l -4.9,-1.6 0.4,11.8 -9.3,2.8 -8.9,-1.6 -6.5,-4 -10.9,3.6 -2.2,2.9 -12.4,3.6 -11,0.8 -2.2,-10.9 -8.8,-0.9 -6.5,-2.4 -3.7,-5.7 0.7,-5.3 13.9,-3.6 9.5,2.4 3,-4.5 -9.5,-3.6 -6.6,1.6 h -12.5 l -3.6,-16.2 -5.1,-5.7 3.6,-10.6 5.9,-7.3 14.6,-8.5 z m 484.1,-2.8 -6.1,4.5 -7.3,-2.9 3.3,-3.6 4,-0.8 z m 419.3,23.6 4,11.7 6,6.5 v 4.9 l -5,-0.4 -4,-2.4 -1,2.8 -4,-3.6 -2,0.8 -5,-12.2 h -8 l -2,-4.5 2,-5.3 6,-4.8 -1,-9 3,-5.7 9,-5.2 8,3.6 -1,8.1 -4,6.5 z m 87,-22 -9,2.9 2,-8.2 4,-0.8 z m 302,7.3 -10,4.9 1,-4.9 v -2.4 l 2,-2.8 1,-8.6 5,1.2 6,9 z m -1115.4,-17.4 7.3,2 6.5,-0.8 5.3,2.8 4.8,7.3 -4.4,5.3 -6.5,-0.8 -4.5,2.5 -4.1,-2.5 -1.6,-6.5 -3.6,-2 z m -126.8,3.6 -3.6,11.4 -6.5,-8.5 -0.9,-6.1 7.4,-0.4 z m 1233.2,13.8 -2,8.6 -7,-6.5 -9,-3.7 2,-5.7 -1,-6.5 4,-1.6 2,-3.7 7,6.5 4,1.7 2,5.7 -3,2.8 z m -1138.1,-1.6 4.5,16.3 3.6,-6.5 -2.8,-10.6 3.6,-7.7 11.8,-7.3 6.5,0.4 2,8.1 3.7,3.7 2.4,13.4 5.7,1.2 8.1,-8.5 12.6,4 4.9,11.8 3.7,1.6 -0.8,8.1 6.1,-6.9 5.6,3.3 5.3,9.3 11.8,5.7 5.7,9.8 -4.5,2.8 2.5,18.3 3.6,1.6 0.4,4.9 3.7,1.6 6.9,-0.8 4.1,5.7 2.4,11.8 3.7,-2 h 6.5 l 2.8,4.4 -6.5,8.6 -0.8,5.2 -3.7,-0.8 -1.2,8.2 -7.3,-3.7 -6.9,-12.6 -5.7,-1.2 -0.8,8.5 2.4,7.3 9.4,7.3 1.6,16.3 -1.6,7.3 -4.5,-5.3 -8.5,-0.8 7.7,9.8 -0.4,4.4 -12.6,-4.4 -5.7,-6.1 h -3.3 l -7.7,-4.9 0.4,-6.1 -4.8,-2 -4.5,-9 -3.7,3.3 -5.7,-0.8 -8.1,2 -8.1,-4.5 4.5,-10.9 6.5,2.4 8.1,-0.8 4.9,-5.3 -4.1,-4.9 7.3,-8.1 4.1,-7.3 -3.7,-13 -7.3,-5.3 -2,-5.7 -4.5,0.4 v -4.9 l -11,-8.1 v -6.5 l -5.7,0.8 v 3.7 l -13.4,-3.3 -7.3,3.7 -13.8,-3.7 -2.8,1.7 -5.3,-5.3 h -7.7 l -6.9,-7.7 -2.1,-8.2 -1.6,-10.9 2.4,-11.8 3.7,-11.8 6.5,-8.1 5.7,-2.9 8.9,0.8 z m 1229.1,-11.8 -2,6.1 -8,-2.4 -4,-2 2,-9 4,-0.4 z m -1299.8,-7.7 3.6,3.7 11,-2.5 -3.6,15.5 7.3,5.3 1.2,5.6 -2.9,10.2 -5.6,2.8 -3.7,6.9 -3.7,-1.6 -2.4,-6.5 -4.9,-7.3 -6.1,-2.4 -4.8,-7.3 3.6,-6.5 3.7,6.5 4.9,-0.4 v -11.8 l -3.7,-7.3 z m 31.3,32.9 -5.7,2.9 v -9 l -2.5,-4.4 v -18.7 l 2.5,-5.3 8.5,-2.9 6.9,3.7 11,1.2 -5.3,10.6 -3.6,9.3 -7.4,0.8 z m 1255.5,-34.5 -3,2 -2,-5.7 5,-1.6 z m -1400.2,-10.2 7.3,5.7 h 12.4 l 3.7,3.7 8,10.9 -20.4,15.5 -0.8,5.3 -4.4,3.6 -2.9,11 -13.1,7.7 -1.5,-4.1 -3.7,-6.9 -6.5,-4.8 -3.7,0.8 2.9,-9 2.2,-2 0.7,-9.7 3.7,-7.4 v -7.3 l -2.9,-9.3 z m 1252.2,2.9 -2,5.7 -7,-3.7 2,-5.7 z m -701.6,-13 -4.5,0.4 v -7.3 l 4.5,-0.4 z m 888.6,-8.2 2,4.5 7,-0.8 3,7.3 -5,2.1 h -8 l -11,-9.4 3,-3.7 z M 467.9,293 h -4.5 l -10.9,-7.3 4.4,-11 7.4,-1.2 4,6.5 0.8,8.1 z m 1246.1,-26 4,2.8 3,-5.2 4,-2.1 4,4.9 7,0.8 7,6.5 -4,10.6 -7,-2.5 -5,2.9 -10,3.2 -3,-3.2 -2,6.9 -6,-2.5 -7,-10.1 2,-13.8 9,-6.5 z m -1269.7,8.5 0.9,9.8 h -13.4 l -0.5,-8.2 -11.7,-2.4 4.8,-15 8.2,-9.8 13.4,2.9 1.2,8.9 0.8,9.4 z m -55.5,-14.6 7.2,6.1 6.5,-2.4 2.4,7.3 -4.4,13.4 -5.3,2 -8.6,-2.8 -3.6,4.4 h -4.4 l -6.6,7.3 -7.3,2.1 -8.1,-5.7 8.8,-6.1 H 350 l -9.5,-4.9 4.4,-19.9 9.5,-8.9 4.4,6.9 5.9,-0.8 4.4,9.3 4.3,4.5 13.2,2 -3.6,-5.7 1.4,-6.5 -5.8,-4.4 5.8,-11.8 4.4,1.6 z m 944.2,-2.8 -8,5.2 -28,14.7 -7,7.7 v 4.5 l -5,2.4 -3,8.5 -6,3.7 1,4.4 -6,11 -10,-3.6 h -4 l -2,-6.5 7,-7.4 2,-9.7 5,-1.2 v -4.5 l 3,-9.3 7,-4.5 4,-6.1 5,-2.8 9,-7.4 4,2.9 9,-2.9 13,-12.1 5,-3.7 6,1.2 2,3.7 v 6.1 z m -865.1,-14.7 3.7,7.3 8.5,-2 2.4,13 -0.8,5.7 4.9,-2 10.6,9.3 4.8,-2 4.5,4.4 13,-8.9 h 9 l 10.9,4.5 4.5,5.3 -0.8,7.3 -3.7,9.3 -8.1,2 -8.5,-2.8 -4.5,2.4 -18.3,0.4 -8.1,-2.8 -4.5,1.6 -8.1,-2.4 -4.9,-11.4 2,-8.1 -4.4,-13.8 -10.2,-1.6 -5.7,-2.9 -5.3,-11.8 6.1,-3.6 z m -120.8,8.2 -7.3,6.5 -5.8,-3.7 -4.4,11.8 h -15.4 l -2.9,-3.7 2.9,-6.5 4.4,-1.6 8,-13 8.1,-8.1 9.5,0.8 5.1,14.6 z m 1194.9,-8.2 -6,9.4 8,-0.4 v -4.5 l 17,-0.8 9,10.1 3,1.7 3,8.5 -2,4.5 1,8.1 -4,5.7 -4,2.4 -2,4.5 -10,5.7 -1,3.6 -4,2.9 -4,7.3 -6,0.8 -5,9.3 8,-0.4 13,-6.9 -3,-5.7 4,-3.6 9,5.7 4,-0.4 4,4 9,-3.6 11,2.4 h 7 v 7.3 l 7,2.9 11,2.8 8,2.9 7,1.6 7,6.5 8,-1.2 9,13.8 3,7.3 6,5.7 3,-2.1 6,-17 3,5.3 7,3.6 7,-4.4 4,0.8 5,4.4 3,-4.4 4,-0.8 3,2.4 3,-0.8 -2,-6.5 2,-5.3 6,-8.9 3,-0.4 10,1.2 8,4.4 -1,3.7 13,-2 6,0.4 7,3.2 2,2.9 -2,5.7 10,6 6,7.4 5,-0.9 12,-3.6 11,0.8 9,6.5 3,6.5 -3,5.3 7,4.1 3,3.6 4,-3.6 10,-2.1 2,2.9 13,1.6 5,-4.5 3,3.7 -1,3.6 6,3.7 v 3.6 h 6 l 3,-3.6 -2,-8.1 -1,-8.2 12,2.5 2,1.2 16,-0.4 14,9.3 3,0.8 4,3.7 3,1.2 3,2.5 1,2.8 2,1.6 16,11 3,12.2 2,2.4 2,-2.4 -2,-3.7 3,-1.2 6,2.9 h 3 l 10,10.1 3,0.8 -5,3.7 v 2.8 l -2,0.8 -5,0.9 -2,2.8 v 2.8 l -2,2.1 -1,3.6 3,0.8 -4,2.9 -5,-1.2 -3,-3.7 -4,-1.6 -3,-2 -1,-6.1 -5,-2.1 -3,1.7 -4,0.4 -3,-4.9 2,-4.5 -6,0.8 -1,2.9 3,4.4 -2,4.5 -2,2 -3,2.9 -3,1.6 h -7 l 1,5.7 3,-1.2 2,4.9 v 6.5 l 4,3.2 1,4.9 -3,4.5 -9,-2.9 -1,-2.4 -5,5.3 -9,5.6 -3,-0.4 -11,11 -7,4.9 -1,5.3 -4,-5.3 -7,-2.1 -7,2.9 -4,5.3 v -6.9 l -6,4.4 -8,0.8 -4,11 -4,5.3 v 6.5 l 3,-1.6 3,2.4 -3,3.7 1,5.7 2,0.8 v 4.8 l -4,-0.4 -3,1.3 -2,4.4 v 10.2 h -4 l -5,3.6 -2,8.2 -5,4.4 -2,7.4 -8,8.1 -1,-4.5 -1,-6.1 -2,-5.7 -2,-22.7 v -6.5 l 2,-6.5 4,-3.7 5,-9.7 h 2 l 8,-7.8 2,-3.6 5,-4.5 5,-8.9 8,-4.5 4,-11 1,-10.1 -5,1.6 -2,8.5 -12,11.8 -5,-5.7 -2,-6.5 -10,0.4 -4,3.3 -5,8.5 -4,4.5 -5,5.3 1,7.7 -5,-1.2 -3,3.6 -3,-1.6 -5,-5.7 h -4 l -5,-1.6 -3,2.4 -8,2.9 -10,-2.5 -13,0.8 -4,1.7 -4,5.7 -6,4.4 -1,4.5 -2,0.8 -11,11.4 -4,6.1 -11,10.1 3,2.9 h 6 l -1,6.1 3,3.6 7,-0.8 1,-4.4 5,-1.7 9,9.8 -2,1.2 2,7.3 -4,7.3 -1,6.9 v 7.8 l -1,9.7 -5,4.9 -4,6.1 -1,3.6 -4,5.7 -10,11 -3,6.5 -7,4.5 -5,0.8 -3,-2.9 -8,3.7 -1,2.8 h -2 l -4,5.3 v 4.9 l -7,5.3 -4,2.8 v 3.7 l 4,3.6 2,3.7 3,5.7 1,3.6 v 7.3 l -1,3.3 -4,1.2 -2,-0.8 -5,2.4 -5,0.8 2,-10.5 -1,-4.1 2,-1.6 -1,-4.5 1,-1.2 h -7 l -4,-2.4 3,-6.5 v -2.1 l -5,-2.4 v -2 l -2,2 -5,0.8 -7,3.7 -1,-2.1 4,-7.3 -2,-1.6 -4,-1.6 -4,5.3 -5,2 -2,5.3 h -7 l -1,5.3 2,1.2 h 5 l 1,2.4 v 3.7 l 2,1.2 7,-4.9 4,2.9 h 6 l -2,4.4 -2,-0.8 -4,1.2 -3,3.7 -2,0.8 -3,2.8 -3,5.3 6,2.9 4,8.9 -1,2 4,3.7 -3,1.6 3,2.9 2,2 -5,2.4 5,6.5 v 2.1 l -7,8.9 v 2.8 l -3,2.9 -3,0.8 1,2 -3,8.2 -11,8.1 -3,3.6 -5,1.7 h -6 l -1,0.4 -1,1.6 -2,-1.6 -1,2.4 -10,3.7 h -4 l -4,2.8 2,3.7 -3,0.8 -1,-3.7 v -3.6 l -3,0.8 -3,-3.3 -4,2.5 -3,2.8 -2,0.8 -2,3.7 -3,2 -2,5.3 1,2.8 4,2.9 -1,0.8 5,5.3 2,2 6,6.5 3,8.1 v 2.1 3.2 1.2 l -1,5.3 -1,2.1 -9,4.4 h -3 l 1,2.1 -2,3.2 -5,2 -2,2.1 -2,0.8 v -4.9 l 1,-3.2 -3,-1.3 -3,-1.6 -6,-4.9 v -2.4 l -3,-1.2 -3,-2.5 h -5 l 1,-4.4 h -5 v 6.9 l -5,10.1 v 3.7 l 2,2.8 2,-0.8 1,3.7 1,0.8 v 3.6 l -1,1.7 6,3.6 h 2 l 3,2.9 6,4.9 1,3.2 1,2.8 -1,2.9 v 3.6 l 2,2.1 3,6.9 -5,-0.8 -7,-3.7 -4,-2.8 v -3.3 l -3,-2.8 v -4.9 l -2,-1.6 v -5.3 l -2,-2 -2,-2.1 -2,-4.4 -3,-2.5 v -1.2 l -3,-1.6 1,-2.9 2,-6.5 -1,-1.6 1,-1.2 v -4.5 -3.6 -4.5 l -2,-2.8 -3,-6.6 v -3.2 l -3,-9.3 -2,2 -6,3.6 -1,2.5 h -3 l -4,-1.6 2,-8.6 -4,-10.5 -2,-3.7 -2,-0.4 -1,-2.4 -2,-0.9 -1,-2 -3,-8.1 -2,-2 -2,1.2 -4,4.4 -2,-0.8 -4,1.7 -4,0.8 -3,-1.7 -4,1.7 -1,2 v 2.8 l -3,3.7 -4,1.6 -4,2.9 -4,5.3 -4,4.4 -5,2.1 -1,2.8 -3,1.6 h -2 l -2,2.9 -4,0.8 -1,3.6 1,4.5 1,4.9 -1,4.5 -2,2.8 v 9.7 l -2,-0.8 -3,4.5 1,1.2 -5,2.5 v 2.8 l -3,1.2 -3,-1.2 -2,-2.8 -2,-6.9 -3,-7.4 -5,-7.7 v -5.3 l -3,-4.4 -4,-8.2 -1,-5.6 -2,-9 -1,-6.5 2,-4.9 -2,-2.4 -1,-4.5 -3,4.5 -5,2.8 -2,-0.4 -8,-6.9 1,-2 4,-1.6 -5,-1.2 -3,-2.5 -2,-3.6 -4,-0.4 -2,-3.7 -2,-1.6 v -2.1 l -2,-1.6 -9,0.8 -5,-0.8 -1,1.6 -6,-0.8 -3,1.2 -1,-0.4 -1,0.4 -11,-2 -6,-0.8 -5,-1.2 -2,-6.9 -4,-1.2 -3,0.8 -5,2.8 -5,-0.8 -6,-4.5 -1,-1.2 -6,-2.4 -2,-6.5 -5,-5.3 h -8 l -3,-0.4 v 3.6 l 3,4.9 1,3.7 4,5.3 3,2 v 4.5 l 4,7.3 v -4.5 l 2,-2.8 3,0.8 v 4.8 l -2,3.3 2,1.2 2,2.5 4,-1.7 5,0.8 3,-1.6 9,-10.1 1,2 1,4.5 1,2.8 3,3.3 8,1.2 6,8.9 -2,4.5 -2,1.2 -3,4.5 -2,-0.8 -2,4.4 v 4.5 l -4,0.8 -1,1.2 -3,3.7 h -3 l -2,1.6 -1,3.7 h -5 l -5,2 -3,0.8 -3,2.9 v 2.4 l -4,2 -5,0.8 -7,2.9 -2,2.8 -4,-0.8 -3,3.3 -4,0.4 -6,0.8 -4,2.8 -4,0.8 -3,-0.8 -4,-15.8 v -3.7 l -2,-6.1 -4,-2.8 -2,-4.5 v -2 l -4,-5.3 -4,-2 -3,-4.5 v -3.7 -4.4 l -4,-7.3 -4,-2.1 -3,-3.6 1,-2.1 -3,-4.4 -3,-4.5 -4,-7.3 -4,-2 2,-6.9 v -1.3 0.8 l -5,10.2 -4,-5.3 -1,-2 h -3 l 5,6.5 2,7.3 8,14.6 v 4.5 l 2,3.7 3,1.2 3,3.2 1,4.9 2,11.8 4,5.3 2,0.4 2,3.6 3,10.6 2,0.4 3,2.4 4,2.1 2,3.6 4,2.5 v 2 l 4,2.8 2,3.7 -1,2.8 5,5.3 h 5 l 3,-2 5,0.4 4,-2 h 3 l 11,-2.1 4,-2.4 3,0.8 -1,3.7 1,4.4 h -2 l -1,4.9 -3,5.3 -2,2.8 -3,6.5 -1,3.3 -6,8.5 -7,7.3 -4,4.5 -7,4.5 -6,4.4 -8,9.4 -2,2.8 -3,2.5 -1,2 -4,1.6 v 2.9 l -3,7.3 h -2 l -2,7.3 1,2 3,3.7 -1,8.9 1,2.9 2,3.6 3,2.9 v 2.4 2 l 1,8.2 -1,4.4 2,6.5 -2,4.3 -4,6 -3,1 -7,3 -3,2 -5,7 -4,1 -3,4 -1,5 1,2 3,11 -1,4 v 3 l -1,2 -8,4 -4,2 -2,3 2,5 -4,10 -5,6 -5,8 -8,9 -5,5 -4,4 h -4 -4 l -5,4 h -6 -5 -5 -3 l -9,4 -5,-8 -4,-4 h 2 l 1,-5 -1,-4 -5,-8 -2,-8 -2,-3 -4,-3 -2,-4 -3,-9 v -6 l -2,-4 v -6 -3 l -5,-10 -5,-10 -4,-7 v -4 -7 l 1,-2 2,-4.3 v -7.3 l 5,-4.5 3,-4.4 V 965 l -3,-5.7 -1,-4.4 2,-3.7 -6,-12.2 4,-1.6 -4,0.8 -1,-1.6 -1,-3.7 -1,-2.8 -4,-2.9 -3,-3.6 -4,-3.7 -2,-3.6 -3,-6.1 3,-2.1 v -5.3 l 2,-2 -2,-0.8 2,-4.5 v -2 l 1,-3.7 -2,-4.4 -3,-1.2 -2,-3.7 -2,1.2 h -6 l -1,0.8 -4,0.8 -4,-2.8 v -3.2 l -5.8,-4.9 -4,-0.8 -4.5,0.8 -6.1,0.8 h -2 l -1.7,1.2 -3.6,0.8 -5.7,2.9 -4.5,0.8 -1.2,1.6 -5.3,-1.6 -8.9,-1.6 -7.3,1.6 -8.6,3.6 -8.9,-3.6 -6.5,-5.7 -5.3,-3.3 -0.4,-1.2 -5.3,-2.4 -2,-3.7 -2.4,-2 v -2.9 l -1.3,-4.4 -3.6,-1.6 -4.5,-4.1 -2.4,-1.6 v -2.9 h -2.9 l -3.6,-3.6 v -3.7 l 0.8,-2.8 -2.8,-7.4 2.8,-4.4 v -2.5 l 0.8,-2.8 2,-6.5 v -3.7 l -1.2,-3.6 0.4,-6.5 -1.2,-2.5 -3.6,-0.4 0.8,-6.1 2.8,-2.8 3.7,-9.3 4.4,-4.5 0.4,-4.5 1.7,-4.5 5.2,-3.6 2.1,-4.9 1.6,-2.4 7.3,-2.1 7.3,-6.5 2.1,-4.4 -0.9,-2.9 V 729 l 2.9,-4.5 v -2.1 l 4.5,-5.2 5.2,-2.1 3.7,-2.8 2.8,-4.5 2.1,-6.5 3.2,-0.8 0.4,2 3.3,2.5 h 4.9 l 8.1,0.8 h 2 l 3.7,-3.7 5.3,-0.8 1.6,-2.4 5.7,-2.9 h 6.5 l 2.4,-1.2 10.2,-0.8 3.3,1.2 5,-2.8 3,1.6 2,-1.6 7,0.8 6,-2 2,0.4 2,8.9 3,1.6 v 2.9 l -3,4.4 -3,2.1 1,2 6,4.5 h 1 l 5,2.4 5,-0.8 10,3.7 2,5.7 2,0.8 8,1.6 4,2 4,2.9 3,-0.8 3,-4.9 v -4.5 l 3,-4.5 5,-2 4,0.8 5,1.2 v 2.5 l 4,1.2 5,0.8 2,1.6 11,2.1 3,1.6 h 3 l 3,1.2 2,-0.8 5,-2.8 h 7 l 2,2.4 h 9 l 3,-1.6 1,-1.7 3,-9.3 5,-10.2 -1,-7.3 v -3.6 l 2,-1.7 -1,-2 -3,2 -5,-1.2 -3,3.7 -6,1.2 -4,-3.7 -4,-1.2 h -4 v 2.9 l -4,1.2 -3,-1.2 -1,-2.5 -3,-1.2 -5,-0.8 -1,-2 v -4.5 l -5,-1.6 2,-6.5 v -2.9 l -4,0.8 v -3.2 l 4,-3.7 5,-0.4 5,0.4 2,-1.2 1,-4.5 11,0.9 5,-4.5 6,-2 7,0.4 4,1.6 h 3 l 2,3.6 3,-0.8 3,2.5 5,0.4 6,-1.2 3,1.2 7,-2.9 1,-1.2 1,-3.2 -2,-5.7 -8,-4.5 -5,-5.7 -8,-3.6 -4,-4.5 4,-1.6 2,-4.1 v -3.7 l 3,-3.2 -1,-2 -4,0.8 -4,2 -5,0.8 -2,2.1 -4,1.6 v 2.8 l 4,3.7 4,-0.8 h 2 l -1,3.2 -5,-0.8 -1,1.2 -5,1.6 -2,2.9 -2,-0.8 v -4.9 l -5,-2.4 6,-3.7 -2,-1.2 h -6 l -2,-4.5 h -3 l -2,2.1 -4,3.6 -3,1.6 1,3.7 -1,2.8 -4,0.8 -1,7.4 -1,2.8 h -2 l -2,5.3 v 1.2 l 2,3.7 v 6.9 l -3,0.4 -2,2.4 -2,0.8 -3,2.9 -1,-4.5 -5,-2 -4,0.4 -3,3.6 -3,1.7 -3,-1.7 v 2.5 l 1,2 1,7.3 5,2.9 v 4.4 l -2,-2 -2,0.8 v 2.9 l -3,-0.9 3,5.3 -5,0.4 -1,-2 -3,1.6 v -4.4 l -2,-2.5 1,-2.8 3,-0.8 5,2.4 -1,-2.4 -4,-0.4 h -4 l -3,-3.3 v -1.2 l -3,-4.5 -1,-2 -2,-1.6 -1,-2.9 1,-8.9 h -1 l -2,-2.9 -2,-1.6 -5,-2.8 v -0.9 l -4,-2.8 -5,-2 -4,-3.7 v -1.6 l -1,-2 -1,-3.7 -2,-1.6 -2,2.4 -1,1.2 -2,-4.8 1,-0.9 -3,-1.6 -5,4.5 2,2 -2,2.5 1,2.8 5,4.5 1,0.8 2,6.5 5,4.5 2,0.4 h 4 v 3.6 l 6,3.3 5,2.8 3,3.7 -1,2 -2,-2.8 -5,-2.1 -1,1.2 -2,3.7 3,2.8 v 3.3 l -3,1.2 v 2.5 l -3,2.8 h -2 l 3,-7.3 -3,-6.5 h -1 l -2,-2.5 -1,-2.8 -4,-0.8 -2,-2.9 h -3 l -7,-5.6 -1,-1.7 -6,-5.3 -1,-6.5 -6,-3.6 -2,-0.8 -4,4.4 -3,0.9 v 0 l -5,4.4 -3,0.8 -7.1,-2.4 -2.9,-1.2 -5.3,2.8 -0.8,2.1 0.8,2.8 v 4.5 l -4.4,2.8 -1.3,1.6 -6.1,1.7 -4.8,7.3 -1.7,3.6 1.7,3.7 1.2,1.2 -4.1,2.9 -2.4,5.2 h -1.2 l -2.5,1.7 -2.8,3.6 h -11.8 l -1.2,1.2 -3.3,0.9 -0.4,2 -1.6,0.8 -2,-1.2 -3.3,-5.3 -4,-1.6 -2.5,1.6 -4.4,-0.8 -0.5,-2 v -6.5 l -3.6,-2.5 0.4,-4.5 1.6,-1.2 2.1,-10.1 -1.3,-6.1 0.8,-0.8 -0.8,-1.3 -1.6,-7.3 4.5,-1.6 2,-2.8 3.7,1.6 h 8.9 l 7.3,1.2 3.7,-0.8 7.3,1.6 2,-0.8 1.6,-2 1.7,-7.3 0.4,-10.2 -0.4,-2.4 -3.3,-1.3 -1.2,-3.2 v -1.2 l -2.8,-2.5 -0.9,-2 H 951 l -2,-1.6 h -3.7 l -0.8,-3.7 -2,-0.4 1.2,-2.4 7.3,-2.1 2.4,2.9 2.1,-1.6 3.6,0.8 v -4.9 l -0.8,-3.7 h 2.5 l 1.2,2.1 6.9,0.8 v -2.9 l 4.9,-2.4 2.4,-2 0.4,-6.1 5.3,-1.2 4.5,-2.9 4.4,0.4 v -0.4 h -2.4 l 0.8,-3.2 2.8,-5.7 5,1.6 2,-2.4 -2,-2.9 -2,2.1 -1,-2.9 5,-4.5 5,-0.4 3,2.1 v -3.7 l 4,-0.8 1,1.6 4,-5.3 v -2.8 l -2,-4.5 V 549 l -3,-1.3 1,-2.8 -1,-2.4 V 538 h 2 l 5,-4.1 3,-0.8 2,1.2 v 3.3 l 4,2 -2,2 -2,1.7 -4,5.7 -1,5.2 3,0.9 1,2.8 4,1.6 3,3.7 1,-2.5 3,-0.8 1,-2 h 3 l 3,2.8 h 1 l 1,3.7 2,0.8 v -2 l 9,-3.3 1,-0.4 2,-2.4 4,-1.2 6,-1.7 1,3.7 2,0.8 5,-0.8 1,-4.5 h 6 v -2.8 -8.1 l -1,-2.1 1,-5.3 1,-2 2,-5.3 4,-1.2 4,3.7 v 2.8 l 2,0.8 4,-2.4 V 525 l 1,-3.7 -5,-1.6 v -2 l -1,-5.7 3,-1.6 2,-1.7 9,-2 5,2 4,0.8 2,-0.8 6,-5.7 -2,-5.2 -6,-0.4 -10,2 -3,1.6 -11,4.1 -3,-5.7 -2,-0.8 -5,-2.1 v -2.8 l 1,-4.5 v -6.1 l -2,-2 1,-2 -1,-3.3 v -3.6 l 1,-1.2 1,-3.7 4,-0.8 2,-5.3 3,-2 4,-6.5 2,-1.7 1,-3.6 5,-1.2 -1,-3.3 v -3.6 l -7,-4.1 h -2 l -3,1.2 -4,-2 -2,5.7 h -2 v 4.4 l -2,1.7 v 3.6 l 2,2.1 -3,3.6 -1,3.7 -3,2 -4,1.6 -2,2.9 -4,3.6 -2,5.3 -2,1.2 -2,11.8 -1,5.3 1,2.8 4,0.8 3,4.9 2,2.5 v 2 l -3,2.8 -7,0.8 7,2.9 -7,5.3 -2,2 v 9.8 l -1,3.6 -3,8.5 -3,0.9 -4,-0.9 -1,2.5 -1,4.9 h -6 v -2.9 l -4,-5.7 3,-2.4 -3,-3.7 -2,-4.8 -2,-2.5 1,-4.9 -3,-2.4 -1,-5.7 h 2 v -1.6 l -6,-1.2 v 2 h -3 l -8,9.8 -3,0.4 -4,-0.4 -7,-4.5 v -5.3 l -2,-4.9 1,-2.4 h 2 l -1,-4.5 -2,-2 v -2 l -1,-8.2 3,-10.1 3,-2.5 v -2.8 l 4,-0.8 v -2.9 h 5 l 4,-2.8 v -2.9 l 6,-2.4 2,-6.5 2,-2.8 5,-2.5 v -3.6 l 5,-3.7 1,-4 -1,-1.7 3,-6.5 3,-1.6 -2,-5.7 6,-4.5 3,-8.9 2,-2.8 7,-4.9 v -6.9 l 4,-1.2 2,-5.3 2,-0.8 1,-4.1 4,-1.6 2,-3.7 4,-0.8 1,2.4 5,-1.6 v -6.5 l 4,-2.4 7,1.6 5,-6.5 2,-4.5 5,-0.8 2,1.6 -4,5.7 2,2.9 6,-9.4 1,0.8 -1,7.4 2,-0.9 3,-8.9 3,-1.2 4,3.6 5,2.1 6,4.4 1,2.9 -8,5.3 1,2 h 6 l 4,2.4 2,-4.4 6,4.9 1,4.4 11,0.8 3,1.7 10,7.7 9,8.9 8,4.5 1,12.2 -4,6.1 -10,4.4 -8,-3.2 -9,-1.2 -3,-2.9 -8,-1.6 v 2 l 9,9 -2,8.9 2,4.9 -1,3.6 4,1.7 4,4.8 6,2.5 3,-2.9 -2,-4.4 h -3 l -4,-6.1 4,-4.1 5,4.1 8,3.6 5,-2.8 -4,-7.3 1,-2.9 4,-4.4 4,-1.3 4,-6.1 6,0.9 4,5.2 3,-8.1 v -3.6 l -5,-4.5 2,-5.7 1,-12.6 h 5 l 4,0.8 4,4.9 v 4.5 l -7,1.6 -2,4.9 3,3.2 2,4.1 h 6 l 3,-2.1 2,-8.9 5,-1.2 9,-7.3 17,-10.2 v 4.1 l 2,6.9 6,-4.5 8,-2.4 3,1.6 3,-4.5 6,-2 4,4.9 7,-3.7 -5,-10.9 5,-3.7 13,3.2 3,2.1 9,6.5 7,3.6 6,7.3 h 2 l 3,-7.3 -3,-2 -3,-6.1 -6,-5.7 3,-15.4 -4,-0.8 1,-7.8 5,-4.4 4,-5.3 3,-15.4 3,-2.9 10,-0.8 7,3.7 v 7.3 l -4,12.6 4,12.6 -1,6.5 -1,2 1,6.5 -1,10.6 1,4.8 5,5.3 -2,4.5 v 4.9 l -3,3.6 -1,3.7 -5,9.7 -5,1.2 -4,-4.4 -4,3.2 7,3.7 10,0.4 v -2.9 l 7,-4.4 2,-6.5 4,-4.9 v -6.9 l -2,-2.9 1,-4.8 9,-4.5 6,7.3 1,5.7 3,-2 -3,-9 -5,-4.4 -6,-1.3 -4,2.1 -6,-1.2 1,-4.5 -1,-3.7 V 373 l 4,-6.1 -4,-11.7 -3,-2.9 3,-6.5 7,-5.7 3,-10.9 1,8.5 -3,8.1 1,7.3 6,2.9 4,-2.1 -5,-5.2 -1,-4.5 5,-2.9 3,3.7 1,-6.5 5,-2.8 h 4 l 12,11.7 6,-1.6 1,6.5 -4,2.4 v 4.1 l 5,2.4 -1,8.6 4,-2.1 -3,-12.5 3,-6.5 -5,-4.5 -3,-6.5 -5,-1.2 -3,-3.7 1,-7.3 -2,-11.8 5,-0.8 10,-0.8 18,-4.5 -2,-11 1,-4.8 6,-5.3 7,-10.2 25,-9.3 -1,-3.3 10,-2.8 8,5.3 12,-9.8 2,-2.8 h 5 l 4,-2.9 -2,-6.5 2,-5.2 11,-13 6,-0.9 9,6.5 -2,7.4 9,2.4 z m -300,376.7 -11,5.7 -6,5.2 h -3 l -1,3.7 -3,4.5 4,7.3 v 4.8 l 6,8.2 6,10.9 -1,0.9 -4,10.1 v 2.5 3.6 l 3,2 3,0.9 3,2.8 3,1.6 4,1.2 9,-2 2,0.8 v -3.6 l -1,-8.2 1,-2.8 -1,-1.6 -3,-5.7 -2,-2.5 2,-2.8 3,0.8 5,-1.6 1,-2 -4,-2.5 -1,-4 h -4 l -2,2.8 -2,-0.8 v -2 l 2,-4.5 -6,-1.6 -2,-4.5 -3,-4.9 -3,-1.6 v -2 h 3 l 1,-2.5 2,-2.8 6,-0.8 4,-4.1 -1,-6.9 -3,-1.2 -2,1.2 z m -159,36.5 -4,1.7 6,-2.5 z m -612.2,-432.3 -2.5,6.5 -12.2,-0.4 1.2,-7.3 z m -90.8,3.7 -10.2,6.1 -6.6,-5.3 v -9.3 l 10.2,-4.5 h 8.8 z m 717,-2.9 -1,4.5 -7,5.7 -3,-5.7 -6,1.2 3,-11.3 -3,-6.1 10,-2.9 2,6.5 z m -11,-15.4 -5,2.4 -6,-7.3 10,-3.2 z m -713.3,0 -8,0.8 -0.8,-2.8 14.7,-11 5.8,7.3 -0.7,4.9 z m 86.3,8.1 -9,4.5 -4,-6.1 -3.3,-11 v -8.5 l 3.7,-1.6 14.6,10.9 z m 1059,-37.3 -1,8.1 6,-2.1 7,8.6 v 8.9 l -8,5.7 -14,2.8 -6,5.3 -2,-5.3 10,-32 z m -1081.8,21.1 -1.2,7.3 4.9,6.9 -5.7,6.5 -3.7,-2.4 -2.4,-7.8 -11,-5.2 -7.3,2.8 -1.2,-8.1 4.9,-4.1 -1.2,-6.9 -8.2,-2 0.8,-7.3 8.2,-1.7 18.7,15.5 z m 625.8,-37.8 7,13 v 8.9 l 14,8.6 -1,4.4 -9,1.7 -3,4.4 v 9.4 l -4,2 v 12.6 l -3,0.8 -3,19.1 -4,2.9 -11,-13.9 -3,-8.9 3,-5.7 v -5.3 l 7,-9.3 -3,-7.3 -4,7.7 -8,1.6 -6,-10.1 -2,-9.8 -4,-15 3,-8.2 10,2.9 2,5.7 5,-9.4 6,-0.8 5,-6.5 z m 406,1.2 -6,6.1 -8,-5.3 v -6.5 l 10,0.8 z m 21,-9.3 2,8.1 7,-4.5 4,8.1 -2,14.7 2,8.5 -7,5.3 -8,-1.6 -3,-4.1 -8,-1.6 -4,-5.7 v -6.1 l -4,-4.5 8,-14.6 z M 1273,150 v 7.7 l -8,-0.4 2,-6.9 z m -159,7.7 -1,8.1 -8,6.5 2,3.3 -13,9.3 -5,-6.5 -10,0.8 -11,-9.7 -3,-5.7 1,-8.1 18,-0.8 3,6 V 150 l 12,0.4 6,4.5 z m 113,-22.3 -14,11.3 -4,-6.5 z m 74,0.4 -7,12.6 -9,-5.3 2,-8.6 11,-2.4 z m -70,19.1 -6,1.6 v -6.1 l 8,-10.2 -2,-4.8 9,-3.7 4,5.7 -4,6.5 -7,1.6 z m 81,-17.5 -6,1.6 -4,-5.3 14,-10.1 1,7.3 z m -801,34.9 3.6,10.6 -7.3,4.9 -8.9,16.2 -13,8.5 -9.8,-1.2 -4,-6.1 -6.9,-15 4.4,-4.5 6.5,-1.6 -4,-4.9 -4.5,2.9 h -7.3 l -9,-24.4 6.5,-9.3 -2,-5.7 4.5,-14.7 10.1,-2.4 5.7,-4.9 7.3,22.4 17.1,4.4 0.8,7.4 4.9,16.2 z m 969,-18.3 -12,6.9 -2,-8.1 -10,-0.8 7,-7.3 -1,-8.9 3,-7.4 9,-4 5,-6.1 5,11.8 3,9.3 v 14.2 z M 727.5,84.2 v 5.6 l -10.5,-13 -4.1,-9.7 7.7,-1.2 10.6,10.9 z m -123.9,-30.9 24.4,5.3 3.7,10.9 8.1,4.1 2,8.1 -6.5,9.8 -11.8,11.3 -0.8,11 -15.4,17.1 -9.4,16.6 -8.1,9 -1.2,10.9 -11,7.4 -7.3,8.5 -1.6,17.9 -4.5,12.2 -12.2,8.9 -1.6,10.2 -5.7,3.6 2,7.3 5.3,-0.8 0.4,6.5 -15,11.8 -3.3,-8.5 -7.7,3.6 -8.1,-1.6 -5.3,3.7 -11.8,-2.9 -9.3,-0.8 -2,-9.3 10.1,-11.8 -2.4,-11.8 4.4,-2 5.3,2.8 2.9,8.1 6.5,4.1 -3.7,-12.2 -4.9,-6.5 -7.3,-2.4 V 206 l 4.9,-10.9 9.8,-1.2 2,-3.3 -0.8,-12.2 -4.9,-8.9 -6.1,-2.1 v -16.2 l 14.6,1.6 17.1,-6.5 -1.6,-6.1 -7.3,2.9 -7.4,-6.5 -3.6,8.1 -9.4,-0.8 -10.9,-4.9 -6.1,-5.3 -5.7,-10.9 2.8,-5.3 -10.9,-11.4 8.1,-9.8 9.3,-6.5 h 9.8 l 11.4,-15.4 8.9,8.9 4.5,-17.4 14.6,-13.4 6.5,5.2 11.8,-8.9 12.2,5.3 8.1,-7.3 z m 224.7,-19.1 8.1,21.1 17.5,11.8 -5.7,11.8 -14.2,7.3 -13.8,-1.6 -7.3,7.3 20.7,0.8 -1.6,8.9 6.5,1.2 0.8,-10.9 9.3,-2.9 2.5,11.8 5.6,7.3 11,1.6 11,-14.6 h 10.1 l 14.7,13 -13.8,22 -11,8.1 2.8,5.7 -10.1,22.7 -5.7,0.8 -3.7,9.4 1.2,9.7 -7.3,13.9 -5.7,23.9 5.3,-6.5 5.7,6.5 -2,5.3 4,3.7 h 7 l 1.2,9.3 -1.2,3.7 -8.2,-3.7 -6.5,2.5 -3.2,4.8 v 4.5 l 10.5,7.3 1.3,18.3 -5.7,1.6 -1.7,7.3 3.7,3.7 5.3,3.6 -2.5,4.1 h -2.8 l -1.2,-3.6 h -6.9 l -2.9,6.9 7.3,4 0.9,8.1 h -5.3 l -2.9,4.5 -6.5,-2.8 -2.8,-3.7 -7.3,2 -3.7,5.3 5.7,2.9 2.8,11.8 10.6,8.5 0.4,7.3 3.3,3.2 1.2,9.4 -4.5,4.5 h -4.9 l -5.2,-6.1 v -4.9 l -2.9,-3.7 -3.6,-0.8 -0.9,2.9 -4.4,3.6 -7.3,0.8 -2.1,9 7.3,-1.7 2.9,4.5 3.6,-2.8 h 3.7 l 7.3,4 2.5,3.7 -3.7,5.3 -14.2,13 -14.7,4.4 -1.2,2.5 -8.9,2 -4.9,-0.8 -6.9,8.1 -0.4,3.7 -3.2,3.6 -0.5,3.7 -3.6,5.7 -4.5,0.8 -8.9,6.1 -4.9,-2.4 v 4.4 l -8.9,0.8 0.8,4.5 -4.5,7.3 -1.6,12.2 -4.9,9.8 h -3.6 l 1.2,9.3 -3.7,8.9 v 7.4 l -1.2,4.8 -5.3,-1.2 -2.8,2 -2.9,-2 -4.4,-7.3 -7.4,-0.8 -6.5,-2.9 -6.1,-8.9 -2.8,-8.1 -4.5,-5.7 -2,-6.1 0.8,-5.7 -3.6,-2.8 v -10.2 l -6.1,-7.3 -1.3,-6.9 1.3,-4.9 -2.9,-8.1 2.5,-5.7 0.4,-7.3 h 3.6 l 1.6,-5.3 5.7,-0.8 v -8.1 l 1.7,-8.6 -2.1,-5.2 -3.6,0.8 -6.9,-5.3 -4.1,-0.4 -3.2,-4.5 3.2,-2.8 7.7,2 3.7,3.6 6.9,2.1 -3.7,-7.7 -3.6,-3.3 -4.5,-8.5 -6.1,-2.9 -0.4,4.1 -4.5,1.6 -6.1,-4.4 1.7,-7.4 3.6,-2.8 -0.8,-4.5 2.4,-6.5 -4.4,-4.4 0.8,-5.7 -3.7,-5.3 -1.6,-7.3 v -8.6 l -3.7,-5.2 v -3.7 l -4.4,0.8 -0.8,-5.3 -2.1,-1.2 0.8,-6.9 -4.4,-4.9 -3.7,-4.4 -5.7,-4.5 h -16.2 l -3.7,-1.2 -2,3.6 -3.7,0.4 -0.8,-3.6 -2.4,-2 -2.9,7.3 -8.5,-2.9 -6.9,-6.1 3.7,-4.8 -9.4,-5.3 -3.6,-4.9 2,-4.5 8.9,-0.8 4.5,-2.8 h 8.5 l -1.2,-8.2 -2.4,3.7 -8.6,2 -6.1,-2.8 -2,-5.7 -6.5,-2.4 -5.3,-7.4 1.7,-9.3 16.6,-8.1 2.5,-4.5 7.7,-4.1 h 6.1 l 8.1,-10.9 v -17.1 h -11 l -3.2,-8.1 4.4,-7.3 8.2,-7.3 2,-7.4 9.8,-7.7 7.3,4.9 3.6,-2.8 0.8,-11 -2.4,-12.6 13.8,-11 22,-10.1 8.9,19.1 18.3,-2.1 -6.5,-9.7 -2.5,-13 h 8.2 l 10.9,13 13.8,7.3 1.7,-10.2 4.4,-3.6 -13.8,-30.1 16.7,-9.3 6.5,2 7.3,10.1 8.1,0.9 2.1,-21.2 32.9,-8.9 14.6,2.4 z"
            ]
            []
        , Svg.path
            [ Svg.Attributes.style style2
            , Svg.Attributes.d "m 844.9,377.1 -4.4,5.1 -13.9,13.1 -13.8,4.4 -1.6,2.2 -8.5,2.2 -5.3,-0.7 -6.5,8 -0.8,3.7 -3.7,3.6 v 3.7 l -4.5,5.9 -3.6,0.8 -8.5,5.7 -6.1,-2 v 4.4 l -8.6,0.4 0.4,4.5 -4,6.5 -1.7,12.6 -5.2,10.2 h -2.9 l 1.6,9.3 -3.6,8.9 -0.8,7.4 -1.6,5.2 -4.9,-1.6 -2.5,2 -3.6,-2 -4.1,-7.3 -6.9,-0.8 -7.3,-2.9 -5.7,-8.9 -2,-7.7 -5.3,-6.1 -2,-5.7 1.2,-6.1 -4.5,-2.8 v -10.2 l -5.7,-7.3 -1.6,-6.5 1.6,-5.2 -2.8,-8 2,-5.9 0.8,-7.3 h 3.7 l 1.6,-5.1 6.5,-0.7 -0.8,-8.1 1.6,-8.7 -2.4,-5.2 -3.7,0.8 -6.5,-5.2 -3.6,-0.7 -3.7,-4.4 2.9,-2.9 8.1,2.2 3.6,3.7 6.5,2.1 -3.6,-8 -3.7,-2.9 -4.4,-8.8 -4.9,-2.9 -0.8,4.4 -5.3,1.4 -5.7,-4.4 2,-7.3 3.3,-3.6 v -3.7 l 2,-6.6 -5.3,-4.4 1.7,-5.8 -4.5,-5.1 -1.2,-7.3 0.4,-8.8 -4.1,-5.1 v -3.7 l -3.6,0.7 -0.9,-5.1 -2.8,-1.4 1.2,-6.6 -4.9,-5.1 -3.6,-4.4 -6.1,-4.4 h -15.9 l -3.2,-1.5 -2.8,3.7 -3.7,0.7 V 260 l -2.8,-2.1 -2.9,7.3 -8.9,-3 -5.7,-5.8 3.6,-5.1 -10.1,-5.2 -3.7,-5.1 2.9,-4.4 8.1,-0.7 4.1,-2.9 h 8.9 l -1.6,-8 -2.1,3.6 -8.9,1.5 -5.7,-2.2 -2,-5.8 -6.9,-2.2 -4.9,-7.4 2,-9.5 16.3,-8 2,-4.4 8.1,-4.4 6.5,-0.7 8.2,-10.2 -0.8,-16.9 h -11 l -2,-8 3.6,-7.3 8.5,-7.3 1.7,-7.4 9.3,-8 7.3,5.1 4.5,-2.9 v -11 l -2,-12.4 13.8,-10.98 21.9,-10.24 9,19.02 18.2,-2.2 -6.9,-9.5 -1.2,-13.17 h 7.3 l 11.8,13.17 13,7.3 1.6,-10.23 4.1,-3.66 -13.8,-29.99 17,-9.5 7.4,2.19 6.5,10.24 8.1,0.73 2,-21.2 32.9,-8.78 14.7,2.19 20.3,9.51 7.3,21.21 17.9,11.7 -6.1,11.7 -13.8,7.32 -13.9,-1.47 -7.3,7.34 20.3,0.7 -0.8,8.8 6.1,1.4 1.2,-10.9 9,-2.95 2,11.75 6.9,7.3 11,1.4 10.1,-14.6 h 10.2 l 14.6,13.2 -13.8,21.9 -10.1,8.1 2,5.1 -9.4,23.4 -6.9,0.7 -2.8,9.5 0.8,9.5 -6.5,13.9 -6.5,24.1 5.7,-6.5 5.3,6.5 -2.5,5.2 5.3,3.6 h 5.7 l 1.6,9.5 -0.8,3.6 -8.9,-3.6 -6.5,2.2 -2.9,5.1 0.8,4.4 10.2,7.3 1.6,18.3 -6.9,1.5 -0.4,7.3 2.9,3.6 4.8,3.7 -2,4.4 h -2.8 l -0.8,-3.7 h -7.4 l -2.8,6.6 8.1,4.4 v 8 h -5.3 l -2.8,4.4 -6.1,-3.6 -3.7,-3 -6.5,2.2 -4.4,5.1 6.1,3 2.8,11.7 10.2,8.7 1.6,7.4 2,2.9 1.6,8.8 -4.4,5.1 h -5.3 l -4.1,-5.9 -0.8,-5.1 -2.4,-3.6 -3.7,-0.8 -1.2,3 -4.5,3.6 h -6.5 l -2.8,9.5 7.3,-1.4 2.9,4.3 3.6,-2.9 h 3.7 l 7.3,4.4 2.8,3.7 M 603.6,53.11 l 24,5.12 4.4,10.98 7.3,3.65 2.5,8.78 -6.9,9.46 -11,11 -1.2,11.7 -15.5,16.8 -9.3,16.9 -7.3,8.7 -2.5,11 -10.9,7.3 -7.3,8.8 -1.3,17.5 -4.4,12.5 -12.6,8.8 -1.2,10.2 -5.3,3.6 1.6,7.3 5.7,-0.7 v 6.6 l -15.5,11.7 -2.8,-8.8 -8.1,3.7 -8.1,-1.5 -4.9,3.7 -11.8,-3 -9.4,-0.7 -1.6,-9.5 9.4,-11.7 -2.1,-11.7 4.5,-2.2 5.7,3 2.4,8 6.5,4.3 -3.6,-12.3 -5.3,-6.6 -7.3,-2.2 V 206 l 5.3,-11 9.3,-1.5 2.9,-2.9 -1.3,-12.4 -5.2,-8.8 -4.9,-2.2 v -16.1 l 14.6,1.5 15.8,-6.6 -1.2,-5.9 -7.3,3 -7.3,-6.6 -3.2,8 -10.2,-0.7 -11,-5.1 -5.7,-5.1 -5.2,-11 2.4,-5.1 -11,-11.7 7.7,-9.5 9.8,-6.62 h 10.2 l 10.9,-15.35 9,8.77 4,-17.55 14.6,-13.17 7,5.12 11.3,-8.78 13.4,5.12 7.4,-7.31 21.1,6.58 m 123.5,36.57 -10.2,-13.16 -3.6,-9.51 8.1,-1.46 9.4,10.97 -3.7,7.31 v 5.85 M 1480,154 l -12,6.6 -2,-8 -10,-0.8 7,-7.3 -2,-8.7 4,-7.4 9,-4.3 5,-5.9 5,11.7 2,9.5 v 13.9 l -6,0.7 m -969.5,18.3 3.7,10.3 -7.3,5.1 -8.6,16.1 -12.5,8.7 -9.4,-1.4 -4.5,-5.9 -7.3,-15.3 5.3,-4.4 5.7,-1.5 -4.5,-5.1 -3.6,2.9 h -8.2 l -8.5,-24.1 6.5,-9.5 -2,-5.9 4,-14.6 10.6,-2.2 6.5,-5.1 7.3,22.7 15.9,4.4 0.8,7.3 5.3,16.1 4.8,1.4 m 800.5,-35.1 -5,1.5 -4,-5.1 14,-10.3 1,7.3 -6,6.6 m -81,17.6 -5,1.4 v -5.8 l 8,-10.3 -3,-5.1 10,-3.6 4,5.8 -4,6.6 -7,1.5 -3,9.5 m 70,-19 -7,12.4 -8,-5.1 2,-8.8 11,-2.2 2,3.7 m -74,-0.8 -12,11.7 -5,-6.6 17,-5.1 m -113,22.7 v 8 l -7,6.6 1,3 -12,9.5 -6,-6.6 -10,0.7 -11,-9.5 -3,-5.9 1,-8 18,-0.7 3,5.8 v -11.7 l 12,1.5 6,4.4 8,2.9 m 160,-8 v 8 l -7,-1.5 1,-5.8 6,-0.7 m 216,5.8 2,8 7,-4.3 4,8 -3,14.6 3,8.8 -8,5.1 -6,-1.4 -4,-4.4 -9,-1.5 -4,-5.8 v -5.9 l -3,-4.4 7,-14.6 14,-2.2 m -21,9.5 -6,5.9 -8,-5.9 v -5.8 l 10,0.7 4,5.1 m -407,-1.5 8,13.2 v 8.1 l 14,9.5 -1,4.4 -9,1.4 -3,4.4 v 9.5 l -4,2.2 v 12.4 l -3,0.8 -3,18.9 -4,3 -11,-13.9 -3,-8.8 3,-5.8 -1,-5.1 8,-9.5 -3,-7.4 -5,8.1 -7,1.5 -6,-10.3 -2,-9.5 -4,-15.3 3,-8.1 9,2.9 3,5.9 5,-9.5 6,-0.8 5,-6.5 5,4.3 m -624,38.1 -1.6,7.3 4.4,6.6 -5.2,6.6 -3.7,-2.2 -2,-8.1 -11.8,-5.1 -7.3,2.9 -1.4,-8 5.1,-4.4 -1.5,-6.6 -8,-2.2 0.7,-7.3 8,-1.5 19.8,14.7 4.5,7.3 m 1081,-21.2 -2,8 7,-2.2 7,8.8 v 8.8 l -8,5.8 -14,2.2 -6,5.9 -3,-5.2 11,-32.1 h 8 m -1059.1,37.3 -8.1,4.4 -5.3,-5.9 -2.8,-11 v -8.7 l 3.6,-1.5 14.7,11 -2.1,11.7 m -75.4,-8.8 -11,0.7 -8,0.8 v -3 l 13.9,-10.9 6.6,7.3 -1.5,5.1 m 702.5,0.7 -5,2.2 -6,-7.3 10,-2.9 1,8 m 11,15.4 -1,4.4 -8,5.8 -2,-5.8 -6,1.4 3,-11.7 -3,-5.8 10,-2.9 2,6.5 5,8.1 m -716.4,2.9 -11,5.8 -5.9,-5.1 v -9.5 l 10.3,-4.4 8.8,-0.7 -2.2,13.9 m 89.7,-3.7 -2,6.6 -12.6,-0.7 1.6,-7.3 13,1.4 m 661.7,153.6 5,2.2 2,-4.4 6,4.4 2,5.1 10,0.7 3,1.5 10,8 9,8.8 7,4.4 2,12.4 -5,5.9 -9,4.3 -9,-2.8 -8,-1.5 -3,-2.9 -7,-1.5 -1,1.5 8,9.7 -1,8.5 1,5.3 v 2.8 l 4,2.1 3,4.4 7,2.9 3,-2.9 -2,-4.4 h -3 l -4,-5.7 3,-4.5 6,4.5 8,3.6 4,-2.8 -3,-7.3 1,-2.9 4,-4.4 4,-1.7 4,-5.8 6,0.8 4,5 2,-8 v -3.6 l -3,-4.4 1,-5.9 1,-12.4 6,-0.7 3,1.4 3,5.2 1,4.3 -7,1.5 -2,5.1 3,3 2,4.3 h 6 l 3,-2.2 2,-9.5 5,-0.7 8,-7.3 18,-10.2 -1,4.3 3,6.6 6,-5.1 7,-1.5 4,1.5 3,-4.4 5,-2.2 5,5.1 6,-3.6 -4,-11 4,-3.6 13,2.9 4,2.2 9,6.6 7,3.6 6,7.3 2,-0.7 3,-6.6 -4,-2.2 -2,-5.8 -6,-5.9 3,-15.3 -4,-0.8 1,-8 5,-4.4 3,-5.1 3,-15.4 4,-2.9 10,-0.7 7,3.6 v 7.3 l -4,11.7 4,13.2 -1,6.6 -2,2.2 2,6.6 -2,10.2 2,5.1 5,5.1 -3,3.7 1,5.8 -4,3 -1,4.4 -4,8.7 -5,2.2 -4,-4.4 -4,3 6,3.6 11,0.8 v -3.7 l 7,-3.7 2,-6.5 4,-5.2 v -6.5 l -2,-3 v -5.1 l 10,-4.4 6,7.3 1,5.9 3,-2.2 -3,-9.5 -5,-3.7 -6,-1.4 -4,2.2 -5,-1.5 v -4.4 l -1,-4.4 v -5.8 l 4,-5.9 -4,-11.7 -3,-2.9 3,-7.3 7,-5.1 3,-11 1,8.8 -3,8 1,7.3 6,2.2 5,-1.4 -6,-5.2 -1,-4.4 5,-2.9 3,3.7 1,-6.6 4,-2.9 h 5 l 12,11.7 5,-1.5 1,6.6 -3,2.2 -1,4.4 6,2.2 -1,8.7 4,-2.1 -3,-12.5 3,-6.6 -6,-4.4 -3,-7.3 -4,-0.7 -4,-3.7 1,-8 -1,-11 5,-1.4 h 10 l 18,-4.4 -2,-11 1,-5.1 6,-5.1 7,-10.3 25,-9.5 -1,-2.9 10,-2.9 8,5.1 12,-9.5 2,-2.9 h 5 l 4,-3 -2,-6.5 2,-5.2 11,-13.1 6,-0.7 9,6.5 -2,7.3 9,2.2 1,2.2 m -1195.1,8.1 -6.6,6.6 -5.8,-3.7 -5.1,11.7 H 314 l -2.9,-3.7 3.6,-7.3 4.4,-0.7 7.3,-13.2 8.1,-8.7 10.2,1.4 4.4,14.6 -2.2,3 m 121.4,-8.1 2.8,7.3 9,-2.2 2.8,12.5 -1.6,6.6 5.3,-2.2 10.1,9.5 5.3,-2.2 4.9,4.4 12.6,-8.8 h 9.3 l 10.6,4.4 4,5.1 -0.4,7.3 -3.6,9.5 -8.1,2.2 -9,-2.9 -4.4,2.2 -18.3,0.7 -7.3,-2.9 -4.1,1.4 -8.9,-2.1 -5.3,-11.7 2.4,-8.1 -3.6,-13.9 -11,-1.4 -6.1,-3 -4.9,-12.4 5.7,-2.9 11.8,3.6 m 863.7,14.7 -7,5.1 -27,14.6 -8,7.3 v 5.1 l -5,2.2 -4,8.8 -5,3.7 1,4.3 -7,11 -9,-3.6 h -3 l -3,-6.6 7,-7.3 3,-9.5 4,-1.5 v -4.4 l 3,-9.5 6,-4.4 4,-5.8 6,-3 9,-7.3 4,2.9 9,-2.9 12,-12.4 6,-3.7 5,1.5 3,3.6 v 5.9 l -4,5.9 m 210,-14.7 -6,9.5 8,-0.7 v -4.4 l 17,-0.7 9,10.2 3,1.5 3,8.8 -2,4.3 1,8.1 -5,5.8 -3,2.2 -3,4.4 -9,5.9 -1,3.6 -4,3 -5,7.3 -5,0.7 -6,8.8 h 9 l 13,-6.6 -3,-5.9 3,-4.3 10,6.5 4,-0.7 4,4.4 9,-3.7 11,2.2 h 7 l -1,7.3 8,3 11,2.9 8,2.9 7,1.5 7,6.6 8,-1.5 9,13.9 3,7.3 6,5.9 3,-2.2 6,-16.9 3,5.2 7,3.6 6,-4.4 5,0.8 5,4.4 3,-4.4 4,-1.5 3,2.9 3,-0.7 -2,-6.6 2,-5.8 6,-8.1 3,-0.7 10,0.7 8,5.1 -1,3.7 13,-2.2 h 6 l 7,3.7 1,2.9 -1,5.1 10,5.9 5,8 6,-0.7 12,-3.7 11,0.7 9,5.9 2,7.3 -2,5.1 7,4.4 3,3.7 4,-4.4 10,-1.5 2,2.2 13,2.2 5,-4.4 2,3.7 v 3.6 l 6,3.7 v 2.9 h 5 l 4,-2.9 -3,-8.1 v -8 l 11,2.2 3,1.5 16,-0.8 13,8.8 4,0.7 4,4.4 v 0 0 l 3,1.5 3,2.2 1,2.9 2,1.5 15,10.9 4,12.5 2,2.2 2,-2.2 -2,-3.7 3,-1.4 6,2.9 3,-0.8 10,11.2 2,0.4 -4,3.7 v 2.4 l -3,1.2 -4,0.9 -2,2.8 v 2.8 l -2,2.5 -1,2.8 2,1.6 -3,2.9 -5,-1.6 -3,-3.7 -5,-1.2 -3,-2.4 v -5.7 l -5,-2.1 -3,1.3 -5,0.8 -2,-5.3 1,-4.1 -5,0.4 -1,3.3 2,4 -1,4.5 -2,1.6 v 0.4 l -3,3.3 -3,1.2 h -7 l 1,5.3 3,-0.8 1,5.3 v 5.6 l 5,3.7 v 5.3 l -2,4 -9,-2.8 -2,-2 -4,4.8 -9,6.1 -3,-0.8 -11,11 -7,5.3 -2,4.9 -3,-4.9 -8,-2.5 -6,2.9 -4,5.3 -1,-6.5 -5,4.4 -8,0.4 -4,10.6 -5,5.7 v 5.7 l 4,-0.4 3,2 -3,3.7 v 5.7 h 3 v 6.1 l -4,-0.9 -3,1.7 -2,4 v 10.6 h -5 l -4,3.6 -2,7.8 -5,3.6 -2,8.1 -9,8.2 -1,-4.5 v -5.7 l -2,-6.1 -2,-22.3 v -6.9 l 2,-6.5 3,-3.7 5,-9.3 h 3 l 8,-9 2,-2.8 4,-4.5 6,-8.5 8,-4.5 4,-11 1,-10.1 -5,1.2 -2,8.9 -12,11.8 -5,-6.9 -2,-5.7 -10,0.8 -5,2.9 -4,8.9 -5,4.1 -4,5.3 1,7.3 -5,-0.8 -3,3.6 -3,-1.2 -5,-6.1 h -4 l -5,-2 -3,2.8 -8,2.9 -10,-2.1 -13,0.9 -4,1.2 -4,6.1 -6,4 -1,4.5 -2,0.8 -11,11.8 -4,5.7 -11,10.1 2,2.5 h 6 v 6.5 l 3,3.6 7,-0.8 1,-4.4 5,-2.1 9,10.2 -2,1.6 2,7.3 -4,7.3 -2,6.5 1,8.2 -2,8.5 -4,5.3 -4,5.7 -2,4.4 -3,6.1 -10,10.2 -3,6.5 -7,5.3 -5,0.4 -4,-3.7 -7,4.5 -2,2.8 M 389.3,260.8 l 6.6,5.8 7.3,-2.2 2.2,7.3 -4.4,13.2 -5.1,2.2 -8.8,-2.9 -4.3,4.4 h -3.7 l -7.3,7.3 -7.3,1.4 -7.3,-5.1 8,-6.6 -14.6,0.8 -9.5,-5.1 4.4,-19.8 8.7,-8.8 5.2,6.6 5.1,-1.4 4.4,10.2 4.3,4.4 13.2,2.2 -3.6,-5.9 1.4,-7.3 -5.8,-3.6 5.8,-11.8 4.4,1.5 0.7,13.2 m 55,14.6 0.4,9.5 h -13 l -0.8,-8 -10.8,-2.2 4.3,-15.4 8.9,-9.5 13,2.9 0.9,8.8 0.8,9.5 -3.7,4.4 m 1269.7,-8.8 4,3 2,-5.2 5,-2.2 4,5.2 h 7 l 7,7.3 -5,10.2 -7,-2.2 -4,2.9 -11,3 -2,-3 -2,6.6 -7,-2.2 -6,-10.2 2,-14.6 8,-5.9 5,7.3 M 467.5,293 H 463 l -11,-7.4 4.5,-10.9 8.1,-1.5 4.5,6.6 v 8 l -1.6,5.2 m 1292.5,-16.1 2,4.4 7,-0.8 3,7.3 -5,2.2 h -8 l -11,-9.5 3,-3.6 h 9 m -888.7,0.7 v 7.3 l -4.4,0.7 0.8,-7.3 3.6,-0.7 m 701.7,20.5 -3,5.8 -6,-3.6 2,-5.9 7,3.7 m -1252.4,-3 7.3,5.9 h 12.4 l 4.4,3.7 7.3,10.9 -20.4,15.4 -0.8,5.1 -4.4,3.7 -2.2,10.9 -13.8,8.1 -1.5,-4.4 -2.9,-7.3 -6.6,-4.4 -4.4,0.7 2.9,-8.8 2.2,-2.2 1.5,-9.5 3.6,-8 -0.7,-7.3 -2.9,-8.8 19,-3.7 m 1400.4,10.3 -4,2.2 -1,-5.9 4,-1.4 1,5.1 m -1254.7,34.4 -6.1,2.9 v -8.8 l -2.9,-4.4 v -19 l 2,-5.1 9,-3.7 7.3,3.7 11,2.2 -5.7,10.2 -3.3,9.5 -7.7,0.8 -3.6,11.7 m -32.6,-32.9 4.5,3.6 10.2,-2.2 -2.9,15.4 6.5,5.1 1.7,5.8 -2.1,10.3 -6.9,2.9 -2.8,6.6 -3.7,-1.5 -2,-6.6 -6.1,-7.3 -5.7,-2.2 -5.1,-7.3 4.3,-6.6 3.6,6.6 4.5,-0.7 v -11.7 l -2.8,-8.1 4.8,-2.1 m 1300.3,8 -2,5.8 -8,-2.1 -4,-2.2 2,-8.8 4,-0.7 8,8 m -1228.3,11.7 3.6,16.1 4.5,-6.6 -3.3,-10.2 3.7,-8.1 11.8,-7.3 h 6.5 l 1.6,8.8 3.7,2.9 2,13.9 6.1,1.5 8.1,-8.8 12.2,4.4 5.3,11.7 3.6,0.7 v 8.8 l 5.7,-6.6 5.3,2.9 4.9,9.5 11.8,5.9 6.1,9.5 -3.7,2.9 1.2,18.3 4.5,1.5 v 5.1 l 3.7,1.5 6.5,-0.8 4.4,5.9 2.9,11.7 2.8,-2.2 h 6.9 l 2.9,4.4 -6.1,8.9 -1.3,4.8 -3.6,-0.4 -1.6,7.8 -7.3,-3.7 -6.5,-12.2 -5.3,-1.6 -1.2,8.9 2,7.3 9.7,7.3 1.3,15.9 -1.3,7.3 -4.4,-4.9 -9,-1.6 8.2,10.2 -0.9,4.4 -12.2,-4.4 -6,-5.7 h -2.9 l -7.3,-5.3 0.8,-5.7 -6.1,-2.4 -4,-9.4 -3.7,3.7 -6.1,-0.8 -8.1,2.4 -7.7,-4.5 4,-10.9 6.9,2 8.6,-0.8 4.4,-5.7 -3.6,-4.5 6.5,-7.9 5.3,-7.3 -4.5,-13.2 -7.3,-5.1 -1.7,-5.8 -4,0.7 -0.8,-5.1 -11,-8.1 v -6.6 h -6.1 v 4.4 l -13,-2.9 -6.5,3.7 -14.6,-3.7 -2.9,1.5 -4.4,-5.2 h -9 l -5.7,-8 -2.8,-8 -0.8,-11 1.2,-11.7 3.7,-11.7 7.3,-8.1 5.3,-2.9 8.9,0.7 -8.1,17.6 m 1137.3,1.5 -2,8.7 -7,-6.5 -9,-3.7 2,-5.9 -1,-6.5 3,-1.5 3,-3.7 7,6.6 4,1.5 1,5.8 -2,3 1,2.2 m -1233.2,-13.9 -3.6,10.9 -6.6,-8 -0.8,-5.9 8.1,-0.7 2.9,3.7 m 126.3,-3.7 7.3,2.2 7,-0.7 5.6,2.9 4.5,7.3 -4.5,5.1 -5.6,-0.7 -5.3,2.2 -4.5,-2.2 -0.8,-6.6 -3.7,-2.2 v -7.3 m 1115.9,17.6 -10,5.1 1,-5.1 v -2.2 l 2,-3 1,-8.7 5,1.4 6,8.8 -5,3.7 m -302,-7.4 -9,3 2,-8.1 4,-0.7 3,5.8 m -87,22 4,11.7 6,6.6 v 5.1 l -6,-0.7 -3,-2.2 -1,2.9 -4,-3.7 -2,0.8 -5,-12.5 h -8 l -1,-4.4 1,-5.8 5,-4.4 -1,-9.5 4,-5.1 9,-5.1 8,3.6 -2,8.1 -3,6.5 -1,8.1 m -425.4,-26.3 5.7,2.9 -5.7,4.4 -7.3,-3 2.8,-3.6 4.5,-0.7 m -478.2,5.8 0.7,8.1 4.4,-5.9 9.5,5.1 0.7,5.1 6.6,-3.6 0.7,-4.4 5.9,5.1 2.2,11 2.9,3.6 -2.9,-24.8 8,-2.2 6.6,6.6 2.2,13.9 3.6,8.7 -0.7,9.5 2.9,5.2 16.2,9.5 v 7.3 l -5.3,-1.5 0.8,11.7 -8.7,2.9 -8.8,-1.4 -7.3,-4.4 -11,3.7 -2.2,2.9 -12.4,3.6 -11,0.8 -2.2,-11 -8,-0.7 -7.3,-3 -3.7,-5.1 0.7,-5.1 13.9,-3.7 10.3,2.2 2.9,-4.3 -10.2,-4.4 -5.9,2.2 h -13.2 l -3.6,-16.1 -4.4,-5.9 2.9,-10.2 5.9,-7.3 15.3,-8.8 3,5.1 m 488.4,17.6 -10.2,-8.1 -0.8,-4.4 h 7.3 l 5.3,5.9 -1.6,6.6 m 812.2,-2.2 -5,2.9 -6,-5.9 1,-3.6 10,6.6 m -1444.7,43.1 10.2,1.5 2.2,4.4 10.3,5.1 h 3.6 l 0.7,-3.7 4.4,-3.6 h 3 l 1.4,-3.7 2.2,3.7 h 3.7 l 2.2,-4.4 5.1,-1.5 7.3,-5.8 5.1,2.2 3.7,3.6 2.2,-2.9 2.2,-10.2 4.3,5.8 2.2,7.3 3.7,4.4 2.2,1.5 2.2,-7.3 2.9,-3.7 2.9,11 h 3.7 l 1.5,-5.9 5.8,-0.7 6.6,2.9 2.9,3.7 8.8,2.2 4.4,3.6 7.3,2.2 1.5,-2.2 6.5,2.2 4.4,5.9 -5.8,6.6 5.1,3.6 10.2,0.7 7.3,-1.4 5.2,-2.9 1.4,3.6 5.9,1.5 3.6,4.4 1.5,5.1 2.2,-0.8 -3.7,-9.5 -2.2,-13.1 9.5,-3 2.2,-2.1 3.7,2.9 0.7,5.1 4.4,2.2 v 2.9 h 5.1 l 3.7,2.9 6.6,2.2 4.9,-2.2 12.6,0.8 4.5,2.9 -0.9,-5.8 -2.8,1.4 -2.4,-6.6 7.3,-1.4 6.9,5.1 6.5,0.7 5.7,-7.3 0.8,-4.4 -3.7,-2.9 v -5.9 l -7.3,-5.1 -5.3,-5.8 2.5,-6.6 -2.5,-3.7 0.8,-8 6.1,-7.3 4.1,-2.2 7.3,6.6 1.6,5.1 -0.8,5.1 4.5,3.7 3.7,7.3 -5.3,5.8 m 1463,-28.5 5,4.4 -2,3.6 -8,2.2 -3,-1.4 v 0 l -7,3.6 1,-7.3 6,-5.1 v 0 h 8 m -1663.2,199.8 -2.2,3.6 -2.9,1.3 v -8.6 l -5.2,-5.3 -5.1,-7.3 -2.9,-1.6 -1.5,-6.5 -1.4,-2.8 -1.5,-1.7 -2.9,-2 h -3.7 l -6.5,2 -8.8,-8.5 -10.3,-6.5 H 216 l -10.2,-4.5 -6.6,1.2 -11.7,-6.5 -5.1,-4.4 -5.8,5.3 -0.8,6.5 h -7.3 l -2.9,4.4 -8.1,3.7 -0.7,-6.5 2.9,-4.5 v -5.3 l 5.9,-3.6 -1.5,-2.9 -5.1,2.9 -4.4,6.1 -1.4,4.4 -8.1,7.4 4.4,4.8 -4.4,4.5 -0.7,3.7 -10.3,6.5 -2.2,4.4 -4.3,1.7 -6.6,7.7 -7.3,2.4 -3.7,2.9 -5.1,0.8 -2.9,3.6 -3.7,-1.6 5.9,-6.5 3.6,-0.8 6.6,-5.7 5.1,-2.4 6.6,-8.6 0.7,-7.3 -5.1,-4.5 -2.9,3.7 -2.9,-4.5 -4.4,-1.2 -7.3,3.7 0.7,-9 -4.4,-5.3 -6.58,2.5 -7.32,-6.1 -0.73,-2.9 3.66,-2.8 -8.04,-7.3 v -2.9 l 6.58,-9.7 1.46,-5.7 2.19,-1.6 4.38,3.2 3.7,-3.2 1.4,-3.7 6.6,0.8 1.5,-2.8 -0.8,-5.7 -2.2,-3.2 3,-4.1 -6.6,1.2 -2.9,2.4 -6.61,-0.8 -6.59,1.3 -5.85,-1.3 -2.92,-5.2 0.73,-4.9 -4.39,-0.8 -3.65,-2.1 13.16,-9.7 6.58,-3.6 h 3.66 v 7.2 l 10.28,0.9 1.4,-5.9 6.6,0.7 v -3.7 l -8.8,-0.7 -2.9,-3.7 -6.58,-2.1 -2.2,-6.6 -13.16,-10.3 2.19,-8 10.24,-0.7 5.81,-6.6 1.5,-5.9 5.1,-8.8 6.6,-0.7 5.9,-6.6 8.7,-0.7 5.9,-7.3 9.5,2.2 m 0,0 2.2,5.1 7.3,-1.5 5.1,0.8 -0.7,5.1 17.5,0.7 8.1,4.4 10.2,1.5 5.9,2.9 7.3,-2.2 5.8,3.7 6.6,3.6 m 613,-14.6 -9,2.2 3.7,-7.3 4.5,-0.8 2.8,-2.9 2,7.3 -4,1.5 M 1029,513.6 v -1.2 l -6,-1.6 -1,2.4 h -2 l -8,9.4 -3,0.8 -5,-0.8 -6,-4.5 v -4.9 l -2,-5.3 1,-2 2,-0.8 -1,-3.7 -2,-2.4 v -2 l -2,-8.2 5,-10.1 2,-2.1 v -3.2 l 4,-1.2 v -2.5 h 5 l 4,-2.8 V 464 l 6,-2 2,-6.9 2,-3.6 5,-1.3 v -3.6 l 4,-3.7 2,-4.4 -2,-1.3 4,-6.9 3,-1.2 -2,-6 6,-4.4 4,-8.7 2,-3.7 6,-4.4 1,-6.6 3,-1.4 2,-5.1 3,-0.8 v -4.4 l 4,-2.2 2,-2.9 4,-0.7 1,2.2 5,-2.2 v -5.9 l 4,-2.2 7,1.5 6,-7.3 1,-3.7 5,-0.7 2,1.5 -4,5.8 2,2.9 6,-9.5 v 0 8.1 l 2,-0.7 3,-8.8 3,-1.5 4,3.7 5,2.2 6,4.4 m 158,10.2 -3,2.2 -5,-2.9 -3,-5.9 3,-3.6 8,10.2 m -158,-10.2 1,2.9 -8,5.1 1,1.5 5,0.7 m -444.3,6.6 -5.7,2.2 -3.3,-4.4 -4,-2.2 v -8.1 l 3.6,-2.2 6.5,3 4.9,5.1 0.8,3.6 -2.8,3 m 428.3,-11 4,4.4 4,1.5 v 3.6 l -2,3.7 1,2.9 m -46,0 4,-0.7 2,-3 5,8.1 4,1.4 4,-2.2 6,2.2 1,-2.2 2,-2.9 v -2.9 l 2,-7.3 2,-4.4 4,0.7 3,-2.9 m 756,5.1 -2,2.9 -6,-4.3 2,-2.2 6,3.6 m -1413.9,13.9 -4.4,5.1 -5.7,-0.7 -9.8,-4.4 2.9,-10.2 4.4,-5.9 9.8,10.3 2.8,5.8 m 15.9,-13.9 2.4,2.9 2.9,-1.4 4.4,4.4 1.2,7.3 -0.4,6.6 2.1,2.9 2,-10.2 2.4,-4.4 5.7,5.1 2.5,8 -3.3,4.4 1.6,5.1 4.5,6.6 2.9,-2.9 4.4,-8.8 1.2,-9.5 3.7,-5.1 -2.8,-4.4 v -8 h 6 l 9.4,4.3 6.5,7.4 0.8,6.5 -3.7,3 h -2.8 l 2,8 4.5,5.9 -1.6,6.5 -7.7,5.9 -4.5,5.3 -8.1,-1.6 -0.8,-3.7 -5.7,0.7 3.6,4.6 -2.8,5.7 -5.3,4.9 h -2.9 l -8.9,-7.7 -0.8,2.8 3.7,4.9 8.1,0.8 3.6,1.6 -5.7,12.2 -3.6,2.4 h -8.1 v 4.5 l -3.7,0.4 1.6,6.1 -8.9,4.9 -8.1,11.8 -3.7,8.1 -2,10.2 v 8.9 l 2,3.7 h 6.5 l 3.7,13.8 v 3.6 l 7.3,-2 12.6,3.7 4.4,3.6 2.1,4.1 9.7,3.6 2.1,2.5 4.4,1.2 3.7,-0.4 8.1,2.8 -0.8,6.5 2,5.3 -0.8,6.5 4.5,8.1 4.5,2.9 0.8,2.8 3.6,1.6 2.1,-3.6 2.8,-1.6 1.6,-5.7 -2,-3.7 -0.8,-9.7 -2.5,-7.3 9,-4.5 2.8,-3.7 3.7,-6.5 v -7.3 l -2.9,-9.3 -7.3,-6.5 6.5,-11.8 -4.4,-11.8 2.4,-3.7 -1.6,-5.6 -0.8,-7.4 3.2,-2.4 9.4,2.8 7.3,0.9 3.6,-2.9 8.2,7.3 2.8,6.5 8.1,2.5 2.1,4 v 12.6 l 2.4,6.5 h 2.8 l 6.5,6.5 8.2,-4.8 2,-3.7 0.8,-6.5 3.7,-6.9 3.6,3.6 0.8,3.7 2.9,4.5 4.4,8.1 3.7,8.9 v 9.4 l 2.8,6.5 5.7,1.6 v 3.7 l 3.3,2.8 2.8,-1.6 1.2,3.6 6.1,0.9 -4.4,6.5 -7.3,3.2 -0.9,3.7 11.8,-8.2 h 2.9 l 1.6,4.5 4.9,1.2 2.4,3.7 -1.6,3.6 2.4,6.1 -7.3,5.7 -3.6,-0.8 -5.3,1.6 -1.7,4.5 -5.6,4.1 -6.5,0.8 -13.9,-0.8 h -14.2 l -3.6,3.6 -1.2,4.5 h -3.7 l -5.3,4.5 -4.5,8.1 9,-6.5 8.1,-4.5 6.5,-1.6 5.3,1.6 2,3.7 -3.6,3.6 -5.7,2.1 1.2,2.4 4.5,-1.6 -0.8,3.6 2.4,9 6.5,3.6 4.5,1.2 2.8,-2 2,1.2 0.8,3.7 -8.9,3.6 -7.3,3.7 -3.7,3.7 -3.6,1.6 -2,-1.6 v -4.1 l 2.8,-3.3 4.5,-2.8 -2.9,-2.8 -7.3,3.6 -2.4,-1.6 m 513.7,-244.8 1,-4.4 5,-2.9 h 3 v -4.4 m -858.2,172 v -6.1 l -4.4,-4.5 -5.1,-2 -1.5,-4.5 -9.5,-16.6 -5.8,-6.1 -3,-4.9 -3.6,1.2 -2.2,5.3 -4.4,2 -0.7,-2.8 -11,-11.8 -2.2,1.6 -5.1,-0.8 V 380.7 m 848.7,2.9 v 4.4 l -6,-1.4 3,-5.9 3,2.9 m 169,8.1 -5,2.2 -3,-2.2 2,-8.1 4,-0.7 4,2.9 -2,5.9 m -143,7.3 -5,-1.5 -8,-8 m 0,0 -3,7.3 v 2.9 l -4,-2.2 h -6 l 1,5.2 -1,2.9 -4,-2.2 -3,2.9 -1,2.9 -2,3.7 1,3.7 -5,8.7 1,3 -6,2 1,3.7 -1,6.5 -4,9.7 2,1.3 -1,5.2 -3,-0.8 -4,2.5 -2,3.6 -2,4.1 2,3.2 -1,4.9 1,3.7 v 6.1 l 4,4 -2,3.3 -2,0.4 2,5.3 -1,5.2 -4,4.9 1,2.9 -2,3.6 m 87,-16.6 10,-11 6,-7.3 3,-3.7 2,-4.5 -2,-3.6 -4,-3.7 -3,-2.8 4,-3.7 -1,-2 -2,-2.4 -3,-8.2 1,-7.7 2,-0.8 -2,-5.3 -1,-2.8 -2,-8.8 4,-7.3 v -2.2 l -4,-5.8 -3,-1.5 -1,-5.1 3,-7.3 m -73,2.9 -2,3.7 h -3 l 3,-5.1 2,1.4 m 2,7.3 -4,0.8 3,-5.2 v -2.2 l 4,-0.7 1,4.4 -4,2.9 m 45,34.3 -3,-7.9 2,-4.4 -2,-5.1 1,-3.7 -2,-0.7 v -8.1 l -3,-5.1 m -524.6,14.6 -6.5,2.2 -3.7,-7.3 3.7,-8 8.1,0.7 v 8.8 l -1.6,3.6 m 284.8,38.7 -1.6,-2.1 v -2.8 h -6.5 l 2,-3.3 h 4.5 l -0.8,-2 1.6,-2.8 -3.7,-1.7 -1.6,0.9 h -6.5 l 5.3,-8.6 2.8,-3.8 h 2.1 l 2.8,2.2 3.7,5.7 -0.4,2.4 4.8,2.1 0.9,-2.9 -0.9,-4.5 5.3,4.5 v -4.5 l 3.7,-1.2 4.4,0.4 2.9,1.7 0.8,-2.1 2.8,0.4 1.3,-2 -0.4,-2.2 2.8,-1.5 2,4.5 3.7,0.8 1.6,1.2 -0.8,2.9 6.5,4.5 -0.8,2 1.6,3.6 -2.8,3.7 -5.3,6.1 h -2 l -3.7,1.2 -2.4,3.3 -7.8,2.8 v 2 l -3.6,1.7 -8.1,-2.5 -3.7,-2.8 -8.1,-0.8 2,-2.1 -0.4,-4.4 m -318.1,3.6 7.7,3.7 -4.9,4.4 -7.3,-4.4 h -6.1 l -7.3,7.3 -6.5,-11 -0.8,-6.5 1.6,-13.8 2.9,-2.8 7.3,9.3 9.7,5.7 3.7,8.1 M 1097,434 l -3,0.8 -2,0.8 -5,-1.6 -2,6.1 h -2 v 4 l -2,1.7 v 3.6 l 2,2.1 -3,3.6 -1,3.7 -3,2.4 -4,1.2 -1,3.3 -5,3.6 -2,4.9 -3,1.6 -1,11.8 -1,4.9 1,2.8 4,0.8 2,5.3 3,2.1 v 2.4 l -3,2.8 -7,0.9 7,2.8 -7,4.9 -1,2.4 -1,8.5 -1,4.5 -3,8.1 -3,0.9 h -3 l -3,2 v 5.3 h -7 l 1,-2.9 -4,-6.1 3,-2 -4,-3.7 -1,-5.2 -1,-2.9 v -4.5 l -3,-2 -1,-6.1 h 2 m 87,-16.6 -10,2 -3,0.8 -11,4.5 -4,-5.7 h -1 l -5,-3.3 v -2 l 1,-4.5 v -5.7 l -2,-2.4 1,-2 -1,-2.9 v -3.6 l 1,-1.7 1,-3.6 4,-0.8 1,-4.9 4,-2.8 4,-6.1 2,-1.3 1,-3.6 5,-1.6 -1,-2.9 v -4.4 l -6,-4.5 m -1030.87,33.7 -2.93,0.8 -7.32,-4.5 2.2,-2.8 8.05,6.5 M 526,477 l -4.9,2.5 0.4,-8.1 7.3,-2.1 1.6,3.7 -4.4,4 m 19.1,0.9 -1.7,5.2 h -4 l 1.2,-6.5 4.5,1.3 m 386.8,3.6 -1.2,-2 v -0.8 l 1.2,1.6 v 1.2 m 186.1,27.2 6,-6.1 -3,-4.8 -5,-0.8 m -1031.59,6.5 -5.85,0.8 -4.39,-3.7 7.32,-2 2.92,4.9 M 1074,499 l 2,0.8 -2,2.4 V 499 m 24,25.6 1,-3.7 -5,-1.2 v -2.4 l -1,-5.7 3,-1.6 2,-1.3 9,-2.4 5,2.4 m 2,19.5 1,-2.8 -1,-10.2 2,-2 2,-4.5 m -6,0 4,0.9 2,-0.9 m -27,6.5 -3,1.7 v -4.5 l 3,2.8 m -116.5,71.6 -1.6,1.6 -3.6,2 -6.5,-1.2 -5.3,2 -4.5,-0.8 -3.7,1.6 -0.4,2.1 -3.2,-0.4 -3.7,0.4 -2,2.4 -0.8,-1.6 4.5,-4.9 1.6,-3.7 3.6,-1.6 -1.6,-2 -2,0.4 -0.8,-1.2 h -2.9 l -1.6,-1.7 5.3,-2.8 1.2,-2 -0.8,-6.9 6.1,-1.3 v -2.8 l -2.9,-7.3 1.3,-4.5 -5.7,2.9 -4.5,-2.9 2.8,-4.4 -2,-3.7 h -1.6 l -1.2,-3.7 1.2,-2.8 h -2.9 l -1.6,-2.4 1.6,-1.3 1.7,-4.4 -1.7,-1.7 0.9,-2.8 2.8,-1.2 v -3.3 l 1.6,-3.6 2.1,0.8 8.1,-1.2 -0.8,3.6 -4.5,3.7 0.8,2 7.3,0.8 h 2.9 l 0.8,2.1 -2.1,6.1 -3.2,3.6 0.8,3.7 2.9,2 2.4,2.8 0.8,4.5 2,4.5 2.9,1.2 3.6,7.3 1.7,5.3 4.8,1.6 1.7,1.2 0.8,2.5 -0.8,3.6 -3.7,4.1 v 2.4 l 1.6,1.3 m 112.5,-65.1 -2,2.9 1,-2.9 -1,-2.8 5,-1.6 -1,3.6 -2,0.8 m -153.5,2.5 -2.8,2 v -5.3 l 4,-1.2 -1.2,4.5 m -686.8,1.2 -5.1,1.6 -1.5,-6.1 h 3 l 4.4,2.5 -0.8,2 m 6.6,2.8 -2.9,1.7 -2.2,-7.3 3.6,-0.9 1.5,3.7 v 2.8 m 853.7,-2.8 -4,-2.8 -5,2 m -944.8,-0.4 1.4,4.9 -10.2,6.1 -2.9,-4.5 2.9,-3.7 3.6,0.8 5.2,-3.6 m 914.8,7.3 -3,0.4 -1,-3.7 4,-3.6 h 2 l -2,1.6 v 5.3 m 13,11 -1,-3.3 1,-4 2,-2.5 1,-4.9 4,-1.6 4,3.7 v 2.8 l 2,0.8 4,-2 v -6.9 m 16,3.6 -3,-1.2 -1,1.2 -3,-2.8 m -94,11 -1,-0.8 2,-3.7 h 4 l 3,-4.1 h 2 l 1,3.7 -3,1.2 -6,1.6 -2,2.1 m -76.7,-3.7 -2,-0.8 -2.4,-2.8 0.8,-1.3 h 2 v 2.9 l 1.6,2 m 181.7,9 v -0.9 l -2,-6.5 v -3.6 l -2,-2.5 m -865.1,8.2 -2.2,0.8 -2.2,-5.7 h 3.7 l 0.7,4.9 m 9.5,0.8 -3.6,1.6 -1.5,-6.1 4.4,0.8 0.7,3.7 m 755.6,16.2 v -5.2 l -3,-0.9 1,-2.8 v -2 l -1,-4.5 h 2 l 5,-4.5 3,-0.8 1,1.6 v 2.9 l 4,1.6 v 2.9 l -4,1.2 -3,6.1 -1,4.8 m -82.5,-16.6 0.8,0.8 1.7,0.8 -3.7,2 1.2,-3.6 m 174.5,8.9 -5,-4.4 -3,-0.9 -1,-2 m 0,0 -2,2 -5,-1.6 -3,0.8 -6,-0.8 -4,3.3 m -818.9,5.7 -1.5,3.6 -3.6,-1.6 0.7,-3.7 -0.7,-6.5 5.1,8.2 m 862.9,-4.5 -7,-2 m -8,4.4 2,-1.6 4,0.8 2,-3.6 m -89,9.3 -1,-0.8 -1,-3.7 3,-1.2 3,-2.8 2,0.4 v 3.2 l -2,2.1 -2,4.4 -2,-1.6 m 52,-0.8 v -7.7 m 56,36.5 v -2.4 l -1,-8.1 3,1.6 4,-2 -2,-3.7 -3,-0.8 v -2.4 l -3,-1.3 -1,-4.4 -1,-6.5 -2,-3.7 -3,-0.8 -3,1.6 v -2.4 m -856.3,5.3 -2.2,1.2 -1.5,-1.2 0.7,-5.3 3,5.3 m 825.3,13 4,0.8 3,-2.1 4,-2.4 v -3.6 l 2,-2.1 3,-6.5 m -89,1.2 1,-0.8 4,1.7 v 3.6 h -4 l -1,-4.5 m 210,78.5 -4,-8.2 -3,-2.8 -5,-0.8 v -3.7 l -3,-0.8 2,-4.5 1,-7.3 3,-4.9 4,4.9 3,-1.2 -1,-5.3 4,-2 v -1.6 l 4,-2.1 2,-2.8 h 3 l 3,1.2 3,-2 1,2 h 5 l 1,2.4 5,1.3 6,4.4 5,-4.4 h 11 l 2,2.8 4,1.6 1,-2.8 4,2 4,-1.2 v -5.3 l -5,-2 v -4.5 l 3,-1.6 -2,-3.7 3,-4.4 -1,-6.5 11,-1.3 4,-1.6 5,-0.8 5,-2 13,-3.7 4,-4.4 4,1.6 m 0,0 3,1.2 3,-1.2 2,6.5 v 4.4 l 4,-0.8 2,2.9 7,-0.8 v 4.4 m -426.7,-5.7 1.2,11 -1.2,4.1 -6.9,3.6 -5.7,3.7 -3.6,0.8 -5.7,-3.7 4,-5.6 1.7,-5.3 -4.5,-2.1 1.6,-2.4 -0.8,-5.7 h 7.3 v -4.4 l 1.6,-2.9 5.7,-2 v 2 m -616.5,52.8 -2.1,-0.8 -8.1,-6.5 0.7,-3.2 -4.3,-1.3 h -4.4 v -3.6 l -4.4,0.8 -4.4,-3.2 v -9.4 l -5.9,-11.8 -2.2,1.6 -4.3,-5.2 v -6.5 l 0.7,-5.7 m 807.2,0.4 4,2.4 h 3 l 2,0.8 v 4.9 m -16,-0.8 v -4.1 h 7 v -3.2 m -152,1.6 4.5,-1.6 h 1.2 l 3.3,5.3 0.4,3.6 -4.1,1.6 m -5.3,-8.9 -2.8,5.3 1.2,3.6 3.7,-2.4 1.6,2.9 1.6,-0.5 m -838.2,-5.2 -4.38,0.8 -2.19,-2.9 5.07,-1.6 1.5,3.7 m 921.9,-1.7 -4,-0.4 m -7,15.1 v -3.7 l 4,-0.4 1,0.4 4,-4.1 v -3.6 l -2,-3.7 m 30,11 v -1.6 l 9,-2.9 2,-0.8 1,-2 4,-2.4 6,-0.9 1,3.7 m -49,-3.7 3,0.9 1,2.8 4,1.6 3,3.7 1,-2.1 3,-0.8 1,-2.4 h 3 l 3,3.2 h 1 l 1,2.9 2,0.8 m 23,-6.9 2,0.8 5,-0.8 m 16,0.8 -8,0.8 -8,-1.6 m -132,3.7 1.7,-2.9 0.8,1.6 -2.5,1.3 m 152,0.8 -1,-2.1 -3,-1.6 m 279,5.3 3,-2.9 4,-1.6 v 2.9 l 8,6.1 9,15.8 2,6.1 3,-1.6 v -2.9 l 5,4.9 h 5 l 2,-1.2 4,-0.8 3,2.8 v 2.9 l 5,1.6 1,4.5 5,0.8 2,-0.8 4,3.6 m -1170.5,-35.7 -2.9,1.2 -2.9,-3.7 1.5,-4.9 5.8,0.5 -1.5,6.9 m -183.48,-3.3 -4.39,1.6 0.74,-4.4 3.65,2.8 M 1094,583.1 v -3.7 l -2,-2 1,-2 3,-2.5 v -2 l -2,-8.9 m 638,60.9 -7,1.2 1,-3.6 V 616 l 1,-2.8 -2,-6.5 2,-5.7 -1,-9 1,-5.2 -3,-6.6 1,-9.3 v -3.7 l 6,-3.6 2,5.7 -1,10.1 2,4.5 2,10.2 2,6.9 -4,0.4 -3,2.4 -2,11 2,4.5 1,3.6 m -363,-59.3 -7,2 -1,0.8 m 8,-2.8 -7,2 -1,0.8 m 0,0 8,-2.8 m -8,2.8 8,-2.8 m -325,0.8 v 3.7 l -1,4 3,2.5 1,4.4 -1,2.1 2,5.2 -1,2.9 m 581,-23.6 4,3.7 h 4 l 3,1.6 7,13.8 v 2.9 l 1,2.8 2,7.3 1,2.5 h 5 l 4,1.2 4,4.4 3,4.5 -1,1.6 2,3.7 9,-0.8 3,-2.9 6,-2.4 2,6.1 -3,3.7 -1,7.3 -5,9.3 -6,-2 -4,4 1,5.3 -1,5.7 v 3.3 l -4,0.4 m -73,-54.9 6,3.3 4,-3.7 4,-1.6 v -4.5 l 2,-3.6 2,-4.9 3,-2.5 v -5.6 l -2,-2.5 4,-3.6 5,-1.3 9,-1.6 m -637.6,19.1 h -2.1 l 0.9,-2.8 3.6,-6.5 5.2,2 1,-2 -2,-3.3 -2,1.6 -1,-2 5,-4.5 5,-0.8 3,2.1 m -7,21.5 1,-6.1 -1,-2.8 4,-0.9 2,-2.8 v -3.7 l 1,-5.2 m 885,2.4 v 2 l -2,-2 h 2 m -744,5.7 -2,-0.8 -3,1.6 -3,-0.8 -1,2.4 h -2 m 35,40.7 V 616 l 3,-2 h 5 l 1,-3.7 -1,-2 2,-5.3 -1,-2.8 -3,-1.7 -6,-2.8 h -2 l -2,-2.9 -4,2.1 -3,-2.1 -2,0.9 -3,-7.4 h -5 v -4.4 l -3,-5.3 m -11,2.4 h -4 l -2,4.1 -5,0.8 -2,-1.2 -6,0.4 h -5 l -1,-1.2 -6,-1.7 h -8 l -2,2.9 h -2 m 342,19.9 3,-2 h 3 l 5,-3.7 11,-6.9 6,-0.4 1,1.2 6,0.8 2,4.5 5,0.8 2,-0.8 6,0.8 2,1.2 4,-2.8 1,-3.7 -3,-3.6 1,-4.5 6,-4.9 5,2.9 h 2 l 5,2 4,1.6 1,6.5 m -420,-8.9 v 2.4 l 2,4.5 v 2.8 l -2,0.9 -5,6.5 -1,3.6 m -420.2,-8.9 2.4,6.1 h 6.5 l 5.7,1.2 -2,5.3 v 7.3 l 5.7,3.6 -1.3,4.9 -9.7,-5.7 -2,3.7 -3.7,-5.7 h -11 l -6.5,0.4 v -2 l 2.1,-5.7 7.3,-17.1 3.6,-4.9 3.7,-2.4 1.6,3.7 -2.4,7.3 m 322.6,-10.2 2.8,-0.8 2.8,2.4 3,0.5 -1,2.4 2,0.8 m -9.6,-4.5 v -0.8 m -4.5,0 4.5,0.8 m -4.5,-0.8 3.3,1.6 1.2,-0.8 m -8.6,2.1 4.1,-2.9 m 13.1,15.5 h -2 l -3.8,-3.7 h -3.6 l 0.8,-2.4 -4.9,-2.1 -0.8,-2 h -2.4 l -0.5,-2.4 m -23.5,60.9 2,-2.4 0.8,-7.3 0.8,-11 -0.8,-1.2 -2.8,-1.7 -1.6,-2.8 0.8,-1.6 -2.9,-2.1 -0.8,-2.4 h -2 l -2.5,-1.2 h -3.6 l -0.4,-3.7 -3.3,-0.8 1.7,-2.8 7.3,-1.7 2.8,2.9 1.6,-1.2 3.7,0.8 V 603 l -0.8,-4.5 h 2 l 1.6,2.9 h 6.5 v -2.1 l 5.3,-2 2.1,-2.4 0.8,-5.7 5.2,-1.6 m 60.2,19.1 -6,-5.3 -3,-5.7 2,-2 9,-4.5 h 3 m -741,13.8 2.2,7.3 -10.3,-5.3 -5.8,-3.6 -8.1,-8.6 v -3.6 l 13.9,4.5 3.7,7.3 4.4,2 m 741,-13.8 8,2.8 v 1.7 l 2,2 2,-2 4,2.8 4,0.8 2,3.7 m -69,-11 2,3.7 -1,2 m 513,-3.7 7,3.7 9,-2.9 5,1.3 2,-0.4 4,2.8 3,0.8 v 2 l 3,2.1 12,2.4 3,-1.6 10,-2 v -2.5 l 5,-3.6 h 3 l 4,3.2 4,-0.8 3,1.2 m -589,3.3 v -3.3 l -1,-2 m 0,0 -2,2 v 2.5 m -361.7,1.2 -1.6,3.2 -7.3,-3.2 0.8,-3.7 8.1,3.7 m 798.7,1.6 2,5.3 3,2 3,2 5,1.7 2,2.8 3,7.3 v 4.5 l -2,2 1,3.7 4,0.8 10,1.6 7,4.5 h 4 l 2,7.7 2,3.7 h 4 l 6,0.8 7,0.8 7,-0.8 5,0.8 1,2 4,0.8 6,2.5 h 4 l 2,1.2 10,-4.9 7,-0.8 h 8 l 4,-2.4 3,-4.1 4,-1.6 1,-2 -2,-4.5 1,-4.5 11,2 2,-1.2 2,-3.6 7,-0.8 4,-5.7 5,-0.8 4,-2.9 8,0.4 v -4 l -6,-7 h -4 l -2,3.3 -3,-1.6 -7,-0.8 1,-4.5 5,-11 m -503,6.1 -5,-2.8 -6,0.4 -3,1.6 -2,-2.8 m 0,0 -2,1.2 -1,-0.4 m -70,-0.8 3,0.8 m 67,0 -4,1.2 -3,3.6 h -3 v 2.5 M 953.8,601.4 V 601 h 0.8 v 0.4 h -0.8 m 48.2,-0.4 2,2 4,0.8 4,1.2 -3,3.3 -1,3.6 1,4.1 m -52.3,-13 h -0.5 v -0.8 l 0.5,0.8 m 477.3,0.8 2,-0.8 m -40,53.2 -1,-2.8 2,-2.9 2,-0.8 -2,-6.5 -1,-5.7 -1,-1.6 9,-2 2,1.2 1,-3.7 4,-10.9 1,-1.2 4,1.2 5,1.6 4,-1.6 1,-2.9 -1,-4.4 1,-3.7 h 2 l 3,-2.8 v -2.9 h 3 m -346,0 -2,5.7 m -476.7,24.8 -3.7,-3.7 v -10.9 l -2.8,-2.1 -2.9,1.7 -1.6,-2.5 -4.1,6.1 -1.6,6.5 -2.8,4.9 -3.7,1.6 h -17.5 l -8.9,6.5 -2,3.7 h -10.6 l -2,1.6 1.6,4.5 -6.9,2.8 -5.7,1.2 -6.5,4.5 -2.8,-1.2 v -3.7 l 2.8,-2.4 2,-6.5 -2,-13 -11,-9 -1.6,-3.6 -19.1,-11 -4.9,2.9 -4.4,-1.3 -6.5,0.4 -10.2,-4.8 h -3.2 l -6.5,-2.9 m 0,0 H 312.5 m 30,117 -2.2,-5.2 -5.1,-4.5 -5.8,-2 H 325 l -0.8,-4.5 -6.5,-8.1 v -4.5 l -3,-1.2 -4.3,-9 -3,-2.8 -0.7,-6.5 -2.2,-2.9 1.5,-8.1 -3,-10.9 2.2,-6.1 0.8,-11 0.7,-10.2 -0.7,-2.8 -3.7,-9 3.7,-1.2 8.7,2.1 -0.7,-6.5 -1.5,-2.1 m 729.5,2.1 2,1.6 6,-3.3 3,1.7 2,-0.9 4,2.5 m -40,7.7 h 1 l 3,2.4 2,-2.4 1,1.6 4,-1.6 6,-0.4 1,-4.5 4,-2.4 1,-2 m 17,1.6 -1,1.2 1,2.8 m 24,-4 1,1.2 h 2 m -27,2.8 4,2.5 h 4 l 2,-2.5 5,-0.4 3,-1.6 v -2 h 6 m 27,2.8 6,-2.4 2,2.4 3,0.8 3,2.9 v 1.2 l 2,5.3 2,1.6 m -42,-13.4 4,3.7 m 22,-1.3 -2,-0.8 m -20,2.1 2,-1.3 5,1.3 h 2 l 2,1.6 2,-1.6 h 4 l 3,-2.1 m 8,22 v -4.5 -3.7 -4.4 l -5,-4.9 -1,-3.7 m -53,0.4 v 3.3 l -2,0.4 -3,6.1 m 36,-8.9 -3,1.6 -6,11.8 -5,1.2 m -57,-11.8 -5,-2 -3,2 h -2 m 0,0 -4,2.4 -4,5.7 v 1.7 l 3,-0.9 2,4.5 m 13,-11 V 616 m 0,3.7 v -1.3 m 0,1.3 -1,-0.4 1,-0.9 m -351.6,1.3 0.4,0.8 h -0.4 v 0 0 0 0.8 0 0 h 0.4 v 0.8 l -0.4,0.8 v -0.8 h -0.8 0.8 v 0 -0.8 -0.8 -0.8 0 0 0 m 580.6,40.2 v -2 l 1,-4.5 -4,-2 -3,-3.7 -3,-6.1 -3,-0.4 v -2.4 h 4 v -2.1 l 2,-3.6 h 6 l 4,-4.5 v -7.3 l -4,-0.8 -1,1.6 -6,-2.4 -10,6.1 m -50,22.7 -5,-6.1 -8,-3.6 -4,-4.5 5,-1.2 1,-5.3 v -2.9 l 3,-2.8 -1,-2.4 m 0,0 h -4 l -3,3.2 -6,0.4 -2,2.5 -3,1.2 -1,3.2 4,3.7 4,-1.6 2,0.8 -2,2.8 -3,-0.8 -2,1.6 -4,1.2 -3,3.3 -2,-1.6 v -4.5 l -5,-2.8 6,-2.9 -2,-2.4 h -6 l -2,-3.7 h -3 l -3,2.5 -3,3.6 -3,1.2 v 3.7 m -102,-12.2 h -2 l -3,-2.4 m 22,4.4 -7,-1.2 -1,-1.6 h -5 l -1,1.6 -3,-0.8 m 0,0 -1,2 -5,2.5 -2,2 -4,-3.7 -2,4.5 h -4 m 50,-4.5 -2,-2.8 m 0,0 -1,0.8 -5,0.4 -3,1.6 -4,-0.8 m 87,-1.2 -2,2.9 -2,-1.7 -2,0.8 1,3.7 -5,4.5 m -77,-1.7 -1,-4.8 1,-2.5 m -1,8.2 h 10 v -2.1 l 2,-0.8 -1,-2.8 4,-1.7 h 1 m 0,0 5,3.7 5,1.6 3,-0.8 m -424.4,2 h -4.5 l -0.8,-2.8 6.1,-2.8 -0.8,5.6 m 586.4,-5.6 -6,4.4 h -3 l -1,4.1 -3,3.6 4,8.2 v 4.4 l 5,9 m -151,-31.7 -4,-0.8 -3,2.4 m 20,12.2 -2,-2.8 h -4 l -1,-1.3 1,-2.4 -4,-2.8 -2,-2.9 -1,-1.6 m -70,1.6 -2,1.2 2,3.7 -2,0.8 2,2.4 -2,2.1 2,2 3,1.6 -1,2.1 m 61,-15.9 1,4.9 v 3.2 m -29,-6.1 -3,-1.2 -5,3.7 2,2.8 -2,1.2 v 3.7 l 6,4.5 1,0.8 2,5.7 5,5.3 2,0.8 h 4 v 3.6 l 6,2.1 5,3.6 2,2.9 v 3.2 l -2,-3.7 -5,-1.6 -1,1.6 -2,2.9 3,2.8 v 3.7 l -3,1.6 v 1.2 l -3,3.7 h -1 l 2,-7.3 -2,-7.3 -2,0.8 -2,-2.1 -1,-3.2 -4,-0.4 -2,-3.3 h -4 l -6,-5.6 -1,-2.1 -6,-4.4 -2,-6.5 -4,-3.7 -3,-0.8 -4,4.5 -3,0.4 m 32,-13 1,-0.9 m 254,10.2 -14,-10.2 -14,4.5 v 27.2 M 1738,644 h 3 l 3,4.9 -2,2.5 -6,0.8 -2,4 -1,2.5 -8,-4.5 h -2 l -3,1.6 -2,-1.6 -2,2 4,2.5 -5,2 1,-2 -2,-2.5 v -2.8 l 6,-3.7 h 2 l 1,-4.4 1,-2.1 1,-5.3 -1,-3.6 2,-2 5,6.5 5,3.6 2,1.6 m -677,7.4 -3,-3.7 -5,-1.6 -4,-4.5 1,-0.4 -2,-3.3 -1,-2.8 -3,-1.2 v 2 l -2,0.8 -2,-4.4 m 78,0.8 3,1.2 3,-1.2 2,1.2 m -65,17.1 -1,-3.7 -1,-0.8 -4,-5.3 -1,-2 -2,-2.1 v -3.2 h 4 l 11,0.8 3,2.4 m 56,-3.2 v 3.2 l -5,0.4 v 7.4 m -511.7,-11 v 4.5 l -7.8,2.4 -4.4,3.7 -3.7,0.4 -3.6,6.1 v 2.8 l 2,6.5 -5.7,2.4 -7.3,0.4 -7.3,4.5 1.2,2.4 -0.4,4.1 -3.7,4.5 -1.6,3.6 -2.8,4.5 h -2.9 l -1.6,3.7 2,2.8 1.7,6.5 -2.9,6.9 -7.3,2.9 -1.6,3.6 h -3.7 l -2.8,4.5 -9,6.5 -3.6,8.1 1.6,6.5 3.7,7.3 -0.9,0.8 3.7,8.6 -0.8,6.5 -1.2,4.4 h -3.7 l -1.6,-3.6 -2,-0.8 -1.7,-6.5 h -1.2 l -3.2,-4.9 1.6,-6.9 -5.3,-5.7 -3.6,-1.6 -5.7,3.2 -5.3,-5.3 h -4.1 l -3.6,1.7 -5.3,-0.8 -2.9,2.8 -7.3,5.3 -2.8,-1.6 -3.7,-3.7 -1.6,2 -8.1,-1.2 -4.5,1.2 -5.7,5.3 -7.3,4.5 -2.8,5.7 2,6.1 M 1070,637.5 h 2 l -2,3.7 2,2 v 3.7 h -1 m 18,-5.3 1,3.3 h 3 l 4,1.2 2,-0.8 4,0.8 2,-0.8 2,-1.3 6,-1.6 8,2.9 m -34,10.5 1,-3.6 3,-2.5 -3,-2 -1,-2.8 2,-3.3 m 258,14.6 -4,1.7 -2,2 -5,2.4 -2,3.7 -3,0.8 -1,-3.3 h -6 l -1,-5.6 h -3 l 1,-6.5 -2,0.8 -2,-3.7 -3,-3.2 -2,1.6 -6,-0.8 -7,1.6 -4,-6.1 m -261,2.4 v 0 0 0 m 0,0 v 0 0 0 m -26,1.3 h -1 m -86.1,13.4 -0.8,-1.7 -1.2,-7.3 4.4,-2 2.9,-2.4 2.8,1.6 h 9 m 69,-1.6 v 0 m 0,0 v 0 0 m 114,0 -1,3.2 -2,-0.8 -2,4.9 v 2.4 l 2,3.7 m -111,-13.4 -4,4.4 h -3 l -7.2,-1.2 -3.2,-1.6 -4.9,2.8 -0.8,2.5 0.8,2.8 m -46.7,-8.1 7.3,1.6 3.6,-1.6 7.3,2 2.1,-0.4 m 222.7,0 4,-1.6 5,2 7,0.8 5,2.9 h 7 l 2,3.6 4,2.5 m -144,-11.8 2,2.8 3,1.7 m -5,-4.5 -3,2 -2,5.3 m 123,7.3 2,-2.8 -2,-6.1 -8,-4.1 m -205.2,4.9 h -8.2 l -1.2,-1.2 -5.3,-1.7 -2.8,-2 m 408.7,2.9 -2,-2.5 -4,1.6 -1,2.9 -2,0.8 -6,-2 -4,0.4 -1,3.6 m -271,-4.8 3,-2.5 5,4.5 -1,2.8 m 581,-3.6 -2,-2.1 m 0,0 -2,4.5 h -2 l -2,2.9 h -5 l 1,4.4 -5,-0.8 -1,-2 -6,6.5 -5,2.8 -3,2.9 m -236,-15.5 -4,-1.2 -1,-2.4 -4,-0.4 -15,-0.8 v 0.8 l -5,-0.8 m -305,0.8 -1,-0.8 m 14,2.8 1,-2.8 m -10,2.8 -4,-2 m 4,2.8 -4,-2.8 m 602,0.4 1,3.6 m -646,0.8 -2,6.1 -2,-1.6 -1,-3.6 v -2.1 l 3,-3.2 1,0.8 1,3.6 m 248,6.1 6,0.4 -1,-4 2,-2.5 h 2 l 3,-3.6 6,2.4 3,0.4 1,6.1 m -311.6,-8.1 v 0 0 0 0 -0.8 0 h -0.8 -0.8 v 0 m 0,0 v 0.8 0 0.8 0 0 0 h 0.8 l 0.8,-0.8 v 0 m 88.6,0 v 0.8 m 9,-0.8 2,2 v 3.3 m -5,0 -1,-2.5 1,-2.8 h 3 m -90.3,0.8 -6.1,0.8 -1.2,-1.6 m 94.6,5.3 -3,-3.3 -3,-1.2 m -81.3,0 0.8,3.7 -5.3,3.6 -0.8,0.8 -6.5,2.1 -4.5,6.5 -2,4.4 1.2,3.7 1.7,1.6 -4.5,2.9 -2,4.8 -1.7,-0.4 -2,2.1 -2.4,3.6 h -12.2 l -1.7,1.6 -2.8,0.5 -0.8,1.6 m 327.8,-35 -2,0.4 -5,-5.6 -3,-2.1 -6,1.2 -3,2.9 m -163,-4.1 h 2 m -8,2.1 6,-2.1 m 0,0 -2,0.4 m 8,6.9 -1,-5.2 -3,-2.1 m 578,0.4 -1,-0.4 -4,5.7 v 5.3 l -7,4.9 -4,2.4 v 4.1 l 4,3.6 m -570,-25.6 -4,1.7 m 268,12.5 4,0.5 2,-1.3 3,-0.8 -1,-2.8 -3,-2.5 h -6 l -3,-2.8 4,-4.5 m 14,19.9 2,-4.4 4,-2.1 4,-1.6 1,2.4 3,-0.8 4,-4.4 h 5 l 2,-2.9 5,-2.8 5,-1.6 v -1.7 m -472.3,0.8 0.9,2.5 5.2,-0.8 h 2.9 l 2,2.8 -2.8,2.9 -0.8,8.1 -0.4,2 -2.5,1.6 2.5,2.9 -1.7,2.4 v 4.9 l -1.6,2.8 0.8,1.7 m -7.3,-32.1 2.8,-1.7 m 189.3,0 5,1.7 m -5,-1.7 5,1.7 m 0,0 h -3 l -2,-1.7 m 5,1.7 h -3 l -2,-1.7 m -4,2.9 1,-1.2 3,-1.7 m 143,33.8 -1,-8.2 1,-2.8 -2,-1.6 -2,-5.7 -3,-2.1 3,-3.6 2,1.2 6,-2 v -1.6 l -3,-2.1 -1,-4.4 -4,-0.9 -2,2.9 h -2 m -169,-2 -2,1.6 m 82,41.4 v -3.6 l 2,-2.1 -1,-2.4 -3,3.2 -4,-1.6 -4,2.9 -6,1.6 -4,-2.9 -4,-2.4 -4,0.8 v 2.9 l -5,1.6 -2,-1.6 -1,-2.1 -3,-1.6 -5,-0.8 -2,-2 v -4.5 l -4,-1.2 2,-6.9 -1,-2.9 -3,0.8 v -2.8 l 3,-3.7 6,-0.8 5,0.8 2,-1.6 1,-4.5 11,0.9 5,-4.5 6,-2 8,0.8 3,1.2 h 3 l 2,3.6 3,-0.4 3,2.1 4,0.8 6,-1.6 4,1.6 7,-2.9 v -1.6 m -71,-2.8 v 5.7 l -3,0.8 -2,2.8 -3,0.8 -2,2.9 -1,-4.5 m -178.8,23.6 -1.2,1.6 h -5.3 l -0.8,-2.4 v -6.5 l -3.7,-2.1 0.9,-4.5 1.6,-2.4 2,-9.3 -1.6,-5.7 0.8,-0.8 m 113.1,0 v 0 0 0 m 0,0 v 0 0 0 m 181,0 7,4.8 h 2 l 3,-4 m -19,3.6 2,-0.8 3,1.2 4,-0.4 -3,-2.4 1,-2 m -138,0.8 v 2 l 2,4.5 1,0.8 m -6,7.7 v -2.8 l -2,-0.8 -1,-2.9 1,-8.5 v 0 m 155,0 7,10.9 -1,0.5 -4,10.5 v 1.2 m -122,-15.4 3,-4.1 -1,-3.2 m 0,0 -2,3.2 -4,0.4 -4,-2 -5,0.8 -3,0.8 m 109,1.3 -3,-3.7 -3,1.6 -5,-0.8 m 100,0.8 2,1.2 h 6 l 1,0.9 3,7.3 7,6.1 11,6.5 5,2 -1,3.7 m -243,-23.2 3,-0.8 2,-1.6 h 3 l 3,-1.3 m 117,0 3,2.9 v 2.8 l 2,2.1 v 2.4 l 2,3.6 1,2.9 m -16,-15.4 8,-1.3 m -188,8.6 -1,6.5 -5,1.6 -2,-2 1,-7.4 -1,-3.2 3,-1.2 3,-2.5 1,0.9 1,4.4 v 2.9 m 651,36.5 5,-4.8 h 4 l 8,-1.7 3,0.8 7,-7.3 8,-3.6 3,-4.5 3,-2 4,-9.8 -2,-7.3 5,-4.5 4,3.7 2,3.2 1,4.9 -2,3.7 -1,4.5 h -2 l -1,9.3 -1,3.2 v 2.9 l 1,2 -3,3.7 -3,1.6 -1,-2.4 -2,1.6 h -3 l -3,2.8 h -4 l -6,2.9 -3,4.4 -3,-2 v -2.8 l 2,-1.7 -4,-1.6 -4,2.5 -4,0.8 -6,0.4 -5,1.6 -1,-2 4,-2.5 m -464,-33.7 -3,-2.8 -2,0.8 -2,-3.7 v -4.4 m -93,2.8 -5,-2 -4,0.8 -2,3.6 -4,1.3 -4,-1.3 v 2.1 l 2,2.4 1,7.3 5,2.9 v 3.6 l -2,-1.6 h -2 v 3.7 l -3,-0.9 3,5.3 h -5 l -1,-1.6 -2,0.8 -1,-3.6 -1,-2.1 v -2.8 l 3,-1.6 5,2.8 -1,-2 -4,-0.8 h -4 l -2,-2.9 -1,-1.6 -4,-4.5 m 558,-2.8 -2,1.6 -5,1.2 -7,3.7 -2,-2.9 5,-6.5 -2,-1.6 -4,-1.2 -5,4.9 -4,2.4 -3,4.5 -6,0.4 -1,5.3 2,1.6 h 5 v 1.2 4.5 l 3,1.6 7,-5.3 4,2.9 h 5 l -1,3.6 -2,-0.8 -4,2.4 -3,3.7 -2,0.8 -3,2.8 -3,4.9 6,3.3 4,8.5 -1,2.4 4,3.7 -3,1.2 3,2.9 2,2.4 -5,2 5,6.5 v 2.5 l -7,8.5 -1,3.3 -2,2.8 -3,0.8 1,2 -4,8.2 -10,8.1 -3,3.6 -5,1.3 -6,-0.8 -2,1.6 m -247,-90.6 v -2.5 l 2,-4 -1,-1.6 V 679 l -3,-1.2 2,-3.3 2,0.8 4,-2 4,-6.1 3,-1.2 v 2.8 l 4,1.6 m -267,-3.6 -2,4.9 -2,2.8 h -2 m 287,1.6 -3,0.8 h -6 l -2,-2.4 -12,0.8 v -2.8 l 3,-1.6 4,1.6 2,-2.1 m 297,16.7 h -6 l -4,-2 3,-6.9 v -2.9 l -5,-2 v -1.6 m -641.6,0.8 0.8,0.8 -0.8,0.8 -1.2,-1.6 h -0.9 v 0 -0.8 h 2.1 v 0.8 m -5.7,1.6 h 0.8 0.4 0.8 l -0.8,0.4 v 0.8 h -0.4 v 1.6 l -0.8,0.9 -0.8,-0.9 h -0.8 l -0.8,-0.8 v -0.8 0 h -0.5 -0.8 v -0.8 l 1.3,-1.2 h 1.6 l 0.8,-0.8 v 1.6 m 229.3,6.5 -2,-5.3 -5,-0.8 m 0,0 v 0 m 22,8.1 -2,0.9 -4,-9 -7,5.3 m -2,0.8 -3,-1.6 -4,-4.5 m 0,17.1 -1,-3.7 -2,-1.6 v -5.7 l -1,-3.7 4,-2.4 m -405.1,2.4 H 801 v -0.8 0 -0.8 l 0.9,0.8 v 0.8 m 566.1,14.7 -1,-9 -2,-1.2 -2,1.2 -2,-2 v -4.5 m -386.1,4.5 v 0 -0.8 h -0.4 l 0.4,-0.8 v 0 l 0.9,-0.8 h 0.8 v 0.8 l -1.7,1.6 m 241.1,-0.8 -2,0.8 m -390.2,1.2 H 823 l -1.6,-0.4 v -0.8 h 0.8 v 0 h 1.6 v 0.8 0.4 m -4.5,0 v 0.8 0 l -1.6,-0.8 -0.4,-0.4 v 0 l 2,0.4 m 157.3,-0.4 h -0.8 v 0 l -0.9,0.4 v -0.4 h 0.9 0.8 m -161.7,0.4 v 0.8 H 814 v -0.8 h 0.9 v 0 m 838.1,0 1,3.3 4,5.7 1,3.6 v 8.1 l -1,2.9 -4,1.6 -3,-0.8 -4,1.2 -5,0.8 1,-9.3 v -4.5 l 2,-1.6 -2,-4.1 1,-1.6 m 9,-5.3 -2,2.5 h -4 l -3,2.8 m -826.3,-4.5 v 0.9 h -0.4 v 0 0 h -0.8 l -0.8,-0.9 v 0 -0.8 l 0.8,0.8 h 0.8 0.4 m 411.3,0 v 3.7 l 3,2.8 3,0.9 2,2.4 4,2 4,1.6 9,-2.4 h 2 v -2.8 m 74,0.8 1,1.2 8,-4.1 3,0.4 1,-2.8 2,-2.8 4,0.8 1,2.8 m -299,-2.8 -3,6.5 2,2 -2,2.4 -3,-1.6 -11,-6.5 2,-2 h 2 l 3,1.2 7,-1.2 3,-0.8 m 205,6.5 4,-1.7 4,-3.6 h 4 l 1,-1.2 h 5 l 1,2 10,2.8 5,6.1 h 5 l 1,6.5 M 831.1,687.1 h 0.8 0.4 0.8 0.9 l -0.9,0.8 h -1.2 -1.6 l -0.8,-0.8 0.8,-0.8 v 0.8 h 0.8 m 518.9,0 v 5.3 l 2,2.8 7,-5.2 h 7 l 2,1.6 m -72,10.1 6,2.9 3,-1.2 v -3.3 l 4,-1.2 4,-2.4 1,-4.1 4,-1.6 1,-1.7 4,0.9 m -129,2.4 3,-2.4 10,1.6 m 116,-1.6 7,1.6 m -316,1.2 6,-2 2,0.8 3,8.5 2,1.6 v 2.9 l -3,4.4 -3,2.1 1,2.4 6,4.5 h 1 m -89.8,-19.9 -1.2,1.6 -2.5,-1.6 -2.8,-5.3 -4.5,-2 m 439.8,0.8 -2,0.8 m -139,45.1 -5,-5.7 2,-5.3 -3,-4.5 -6,-2.8 -2,-4.5 -2,-2 v -2.8 l 3,-5.3 -2,-4.5 -2,-1.2 -3,-7.3 m -48,9.3 3,-1.2 2,-6.1 4,1.6 4,-1.6 3,0.8 h 6 l 5,-2 h 8 m -19,24 12,-6.5 1,-2.5 1,-11.4 2,-0.8 3,-2.8 m -237.8,13 h 2.1 l 4.5,-3.7 4.4,-0.8 2.1,-2 5.2,-2.9 h 6.5 l 2.1,-1.6 10.1,-0.8 3.8,1.6 5,-2.8 3,1.2 2,-1.2 7,0.4 m 356,0 -4,-0.4 m 0,0 -3,1.2 h -2 -6 l -7,5.3 2,4.5 v 2.8 l -3,2 v 3.7 l -3,0.8 -3,-0.8 2,5.3 -4,1.2 -1,3.6 v 3.7 l -2,1.6 -4,-0.8 -4,3.7 -3,-0.8 -3,2.4 v 6.5 l -7,2 h -4 l -10,0.8 -8,-2.8 M 834,692.8 h 0.8 -1.7 v 0 h 0.9 m 180,0 -2,7.3 v 4.5 3.6 l -4,3.7 1,6.1 h 2 l 1,3.6 4,2.9 3,11.8 m 364,-33.8 -2,0.9 -7,-3.3 -1,-4.9 -3,-2.4 m -430.8,5.3 v 0 m 0,0 v 0.8 0 -0.8 m 105.8,2.8 -1,-0.8 v -0.4 l 2,0.4 -1,0.8 m -147.3,51.2 1.3,-2 7.3,-2.5 7.3,-6.5 2.8,-4.4 -1.2,-2.9 0.8,-4.8 2.1,-5.3 v -2.1 l 4.4,-4.4 4.9,-2.5 4.5,-2.8 2.4,-4.5 2,-6.5 2.9,-0.8 0.8,2.4 2.8,2.1 5.3,-0.8 8.1,1.6 m 203.8,2.8 -1,-7.3 m 134,37.4 5,-5.7 V 729 l -6,-0.9 v -4.4 l -1,-5.7 v -6.1 l 2,-3.7 3,-6.5 m -202,2.9 v -2.1 h 3 v 1.3 h 8 l 1,1.6 -6,0.8 -2,-1.6 h -4 m 307,31.7 -8,-4.5 -6,-8.1 5,-2.1 v -3.2 l -4,-2.9 v -3.6 l 2,-2 -4,-1.7 -3,-5.7 m -51,72.4 3,-0.8 V 772 l 6,0.8 6,-3.2 -2,-5.7 -3,-1.6 1,-3.7 -4,-1.2 V 755 l 5,-5.7 2,2 6,-1.2 2,-5.3 4,-1.2 2,-5.3 5,-5.7 1,-3.6 -1,-2.5 5,-2 -3,-2.5 -1,-2 -3,-2 v -6.9 l 2,-3.7 6,1.6 5,-1.6 3,0.8 1,-2.8 3,-2.9 m -237,4.9 -3,0.8 -3,-2.8 4,-1.6 5,-0.4 -2,3.6 -1,0.4 m -224.3,44.7 v -6.5 l 8.1,-5.7 8.6,-1.6 3.2,-3.7 2.9,-0.8 2.8,-3.6 v -2.9 l 4.5,-1.6 0.8,-1.2 h 8.5 l 1.7,-2.5 -2.5,-1.2 -1.2,-3.6 v -5.3 l -2.9,-6.5 m 203.8,2.8 h 2 l 1,2.9 -5,6.1 m -3,1.2 5,-10.2 m 521,3.7 3,-2 3,3.6 -8,3.7 -3,3.6 v -3.6 l 1,-5.3 h 4 m -21,9.7 V 718 l 5,-5.3 3,2 3,0.8 2,3.7 -2,2.4 -2,7.4 -2,2 -2,-3.7 -3,-2 3,-3.3 v -2 l -5,1.6 m -483,2.9 -2,-8.1 m -17,4.4 7,2.1 10,-6.5 m -17,0.8 v 3.6 m -3,-2.4 3,-1.2 m -129,0.8 4,2 6,-0.8 10,3.7 2,5.2 2,1.3 8,1.6 4,1.6 5,3.7 2,-0.8 3,-5.3 -1,-4.9 3,-3.7 6,-2.4 5,0.8 4,1.6 v 2.1 l 4,1.6 h 6 l 1,2 m -84,9 1,-0.8 2,-3.7 V 729 l 4,-4.5 3,-2.5 v -4 m 123,10.1 3,-9.7 M 876.2,720 h 0.8 0.8 l 0.8,0.8 h -0.8 l -0.8,0.8 -0.8,-0.8 v 0 l -0.4,-0.8 h 0.4 m -533.7,1.6 13.2,-0.8 -0.7,1.2 8,3.3 12.4,4.1 h 8.1 6.6 v -2.9 h 8.7 0.8 l 1.4,2.5 h 0.8 l 3.6,3.6 2.2,1.2 1.5,2.9 v 1.6 l 0.7,1.6 2.9,2.1 4.4,2 1.5,-2 1.5,-2.9 1.4,-0.8 1.3,0.8 h 3.6 l 3.7,4.5 0.8,0.4 1.6,2.4 v 1.2 l 1.2,1.7 1.7,2.8 h 1.6 v 3.7 l 1.2,1.6 0.8,2 3.7,0.8 1.6,1.7 h 2 l 1.7,0.4 1.2,-0.4 M 1158,720.8 v 2.1 m -682.4,104 -5.3,-5.3 -3.6,-2 -0.4,-0.9 -1.7,-0.8 h -2 l -0.8,-1.2 -2.9,2.9 h -2.4 l -2,1.2 h -1.7 -2 l -1.6,-1.2 h -3.7 l -1.2,-1.7 -2.4,-0.8 -1.2,-1.2 -5.3,-0.8 -0.8,-0.8 -5.7,-2.1 -3.7,-2.4 -1.6,-2 -1.2,0.8 -3.1,-0.8 -3.6,-1.6 -1.5,-2.1 -2.9,-2 -2.2,-0.8 -2.2,-0.8 -2.2,-4.5 -0.7,-2 h 1.4 v -1.7 h -0.7 l 1.5,-2 v -1.6 l -1.5,-1.2 -0.7,-2.5 v -2 l -0.8,-1.6 -2.9,-2.9 -2.9,-2.8 -0.7,-2.5 -3,-1.2 -1.4,-1.6 -0.8,-0.8 -2.2,-2.9 -2.2,-1.2 h -1.4 l -2.2,-2.4 v -1.2 l 1.5,-2.5 -1.5,-2 h -2.2 v -1.6 l -2.2,-0.8 -1.5,-1.3 v -1.6 l -2.9,-2 -2.2,-2.5 -0.7,-0.4 -1.5,-2.4 -1.4,-0.8 v -2 l -2.2,-2.1 -0.8,-2.4 v -1.2 l -1.4,-3.3 v -2.8 l -3,-0.8 -0.7,-1.3 h -2.9 l -1.5,-1.6 H 355 v 3.7 l 0.7,1.6 v 2.8 1.7 l 1.5,2.8 h 0.7 l 2.2,2 1.4,2.5 2.2,1.2 v 2.4 h 1.5 l 0.7,2.9 v 0.8 l 0.8,1.2 1.4,0.8 0.8,1.7 1.4,2 2.2,0.8 0.7,2 v 2.9 l 3.7,6.9 v 2 l 1.5,1.6 1.4,-0.8 2.2,1.2 v 2.5 l 1.5,1.2 v 0.8 l -2.2,2.4 -0.7,-0.8 -0.8,-1.6 -0.7,-1.2 -1.5,-1.6 -2.2,-1.7 -2.9,-1.2 -1.4,-2.4 -1.5,-0.4 -0.7,-0.8 0.7,-2.9 v -2.4 l -1.5,-2.9 -1.4,-0.8 -3.7,-2 h -1.5 l -1.4,-1.6 -3.7,-1.2 v -0.9 l -2.2,-2.4 3.7,-0.4 0.7,-1.6 v -2.1 l -4.4,-6.1 h -1.4 l -2.2,-2 -1.5,-3.6 -0.7,-1.7 v -1.2 l -1.5,-0.8 v -1.6 l -2.2,-2.1 0.8,-1.6 -1.5,-1.2 v -1.6 l -1.5,-2.1 m 814.5,6.5 1,-5.2 m 0,0 -3,1.6 v 2.8 l 2,0.8 m -535.1,-4.4 v 0 l 0.8,-0.8 v 0 l -0.8,0.8 v 0 0 m 594.1,19.9 -9,-0.8 -15,-11.8 -9,-5.3 -6,-1.2 m 0,0 -1,0.8 -11,2.8 6,6.5 -3,2.9 -4,0.8 -4,4.5 -6,-1.7 m -51,-13.8 11,1.7 3,2 h 3 l 3,1.6 2,-0.8 5,-2.8 h 7 l 2,2 9,-0.8 2,-0.8 m -48,55.6 v -43 l -2,-5.3 2,-4.1 -1,-3.6 2,-1.7 m 49,0.8 -2,2.1 m 0,-0.8 2,-1.3 m 5,0 -2,12.2 m -5,-10.9 v 0.8 m 0,0 4,10.1 m 252,-3.6 -4,-1.2 -1,0.8 m -382,0 -1,0.4 2,4.4 v 7.4 l 1,2.4 -1,2.8 v 4.9 l -2,2.5 3,4.8 1,3.3 8,2.8 2,4.5 m 369,-40.2 -4,2.8 -2,5.7 7,2.8 7,5.3 h 8 l 3,2.9 6,2.8 5,0.8 6,-0.8 2,-1.2 -1,-2.4 1,-4.1 m 0,0 h -10 l -3,-3.3 h -4 l -5,-3.6 -1,-2 -2,0.8 -8,-6.1 m -182,0.8 -3,-0.8 -5,6.9 m 11,-6.1 h -3 m 0,0 v 3.6 l 2,5.3 m 71,20.3 -1,0.9 -11,-2.9 h -7 l -4,-1.6 -2,-6.5 -3,-2 -4,1.2 -6,2.8 -4,-1.2 -7,-3.7 v -1.6 l -6,-2.8 -2,-5.7 -5,-6.1 h -8 m 66,0.8 4,6.5 4,2.4 2,2.9 v 2.8 l 3,1.7 -1,3.6 h -2 l -5,2 -1,6.5 m -143,-25.6 1,-0.8 m 0,0 h -1 m 0,0 -4,10.6 -4,-4.5 -1,-2.4 h -3 l 5,6.9 2,7.3 8,14.6 v 3.7 l 2,4.1 2,1.6 4,2.8 m 323,-36.5 -6,-2.1 -1,-5.3 -3,2.5 -5,-1.6 -4,3.6 -2,0.8 -6,4.5 -4,0.4 m -260,65.8 -3,-6.5 -4,-2 -2,-4.5 v -2 l -4,-5.3 -4,-2.8 -3,-3.7 -1,-4.5 v -3.6 l -3,-7.3 -4,-2.9 -3,-2.8 1,-2.5 -3,-4.4 -3,-4.1 -4,-7.3 -4,-3.3 1,-6.5 M 894.5,744 v 0.8 l -0.4,-0.8 v -0.4 h 0.4 l 0.8,-0.8 0.8,-0.8 v 0.8 0.8 l -0.8,0.4 h -0.8 m -526.4,1.6 -1.4,-1.6 0.7,-1.2 1.5,0.8 -0.8,2 m 857.9,0.8 h -4 v -2.8 h -6 m -343.4,2.8 v 0.8 l -0.5,-1.6 v 0 -0.8 h 0.5 l 0.8,0.8 v 0.8 h -0.8 m 19.9,2.1 -0.9,0.8 h -0.8 0.8 v -0.8 l 0.9,-0.9 v -0.4 l 0.8,-0.8 v -0.8 h 0.8 v 0 0.8 1.2 l -0.8,0.9 h -0.8 m -11,-1.3 -0.8,0.4 -0.8,1.7 v 0.8 h -1.3 l -0.8,-1.6 v -0.9 0 h 0.8 l 1.3,-0.4 0.8,-0.8 v 0 h 0.8 v 0.8 0 m 344.5,-0.8 1,2.9 5,5.7 3,2.4 v 4.5 l 4,7.3 m 253,-20.7 -3,-2.1 -1,2.1 m -46,5.2 3,-4.4 3,-1.7 2,1.7 7,2 m 31,-2.8 v 1.6 l -4,3.6 -3,0.9 -5,3.6 v 3.7 l -3,3.6 1,2.1 -4,6.5 -2,-0.9 -2,6.1 v 4.5 l -4,1.2 M 20.78,748.5 v 0 0 0 m 855.42,1.6 h -0.4 v -0.8 -0.8 0 l 0.4,0.8 v 0 0.8 m 631.8,37.8 h -4 l -2,-0.4 V 785 l -5,-0.8 2,-4.1 -3,-1.6 v -3.6 l -3,-2.1 -2,0.4 -2,-2.8 1,-4.5 3,-2 2,-3.7 v -2.8 l -1,-5.3 -1,0.8 -1,-4.4 m -606,2.4 -1.7,0.4 v 0 l -0.8,-0.4 v -0.8 0 l 0.8,-0.8 v 0 h 0.8 l 0.9,0.8 v 0.8 0 m 556,2.8 -1,-4.4 -2,1.6 m -566.9,0.4 v 0.8 l -0.8,-0.8 v 0 0 l 0.8,-0.4 v 0 0.4 m 584.9,0 2,1.6 1,3.7 m -538.3,-4.5 h -24 M 877,792.3 v -6.5 l 2.9,-2 3.6,-9.7 4.5,-4.5 0.8,-4.1 1.6,-4.4 4.9,-3.7 2.4,-5.3 m 24,2.5 v -2.5 m 538.3,4.5 -2,0.8 h -6 l -2,-0.8 -3,0.8 -5,-1.6 v -2.1 m -500,13.9 -20.3,-13 M 877,792.3 v -3.6 h 21.1 v -6.5 -2.9 l 6.1,-2.4 v -15 h 17.5 v -7.3 m -370.1,2 v 0 h 0.8 0.8 0.8 v 0 l 0.4,0.8 0.8,0.8 v 0 0 l 0.8,0.4 h 0.9 v 0 0.8 0.8 0 H 556 v 0.9 0.8 0.4 0 h -0.8 v 0 0 -0.4 h -0.8 0.8 v -0.8 -1.7 0 h 0.8 v -0.8 0 -0.4 h -0.8 v 0 0 l -1.2,-0.8 v -0.8 0 0 h -0.8 -0.8 v 0 h -0.8 m -3.7,2 h -0.8 v 0 l -0.4,-0.4 v 0 0 h 0.4 0.8 v 0 0 -0.8 h 0.8 l 0.8,0.8 v 0 l 0.9,-0.8 h 0.4 0.8 v 0 0 0.8 0 0 0 h -1.2 -0.9 l -0.8,0.4 h -0.8 v 0 m 896.1,28.9 -1,-3.3 -1,-7.3 V 772 l -4,-2.4 3,-4.9 -3,-2.4 2,-3.7 5,3.3 h 2 v 3.6 m -209,-4.4 v 0.8 l -1,0.4 v -2.1 l 1,0.9 m 6,10.9 2,2.1 3,-1.3 6,0.4 4,-1.2 8,-10.1 1,2 1,3.7 m -30,1.6 v -4.5 l 2,-2.8 3,0.4 v 5.3 l -3,2 m -791.8,-7.7 -0.8,2.8 -1.2,2.9 -0.9,4.4 -0.8,5.7 0.8,2.4 -0.8,1.3 0.8,6.1 0.9,1.2 2,3.6 2.4,3.7 1.2,1.6 0.9,2.9 0.8,0.8 2,2.8 h 2.8 l 2.5,0.8 2,2.1 2.9,-0.8 1.6,-0.5 h 3.6 l 2.9,-1.6 h 2.4 l 0.4,1.6 h 1.7 l 1.6,-1.6 -0.8,-1.6 2,-1.2 1.6,-0.8 v -2.1 l 1.2,-1.6 v -0.8 -3.7 l 0.9,-1.2 0.8,-0.8 2,-0.8 3.7,-0.8 2.4,-0.8 3.6,-0.4 3.7,1.2 1.2,-1.2 0.8,1.2 0.9,1.6 -0.9,2 -2,1.7 -0.8,1.2 v 1.6 l -1.6,2 h 1.6 l -2,6.9 h -2.5 m 61.4,-41 v 0 0 l 0.8,0.8 0.8,0.4 0.8,-0.4 v 0.4 l 0.8,0.8 h 0.4 v 0.9 l -0.4,1.6 v 0.4 0 l -0.8,-0.4 v 0 l 0.8,-0.8 v -0.8 0 -0.9 0 h -0.8 v -0.8 0 l -0.8,-0.4 h -0.8 v 0 h -0.8 v -0.8 m 773.9,10.2 -4,-0.8 -2,-4.5 -2,-0.4 v -2.5 l -2,-1.2 -9,0.4 -4,-0.4 -3,1.2 -5,-0.8 -3,1.7 -1,-0.9 m 150,-1.2 3,1.2 h 9 l 1,0.9 v 3.6 l -4,1.6 -1,2.9 2,4.4 1,-3.2 2,-1.2 2,9.3 m 147,-8.5 5,-8.9 3,-1.7 2,2.5 -3,9.3 -2,5.3 -4,-0.8 -1,-3.7 v -2 M 550.4,766.7 v 0 h 0.4 0.8 v 0.9 l 0.8,0.8 v 0.8 0 l 0.8,1.2 v 0 0.8 h -0.8 v 0 0 l -0.8,0.8 h -0.8 v -0.8 0 h -0.4 l -0.9,-0.8 v 0 -0.8 0 h 0.9 v 0 l -0.9,0.8 h 0.9 v 0 -0.8 0 0 0 -0.4 0 -0.8 0 l 0.4,-0.8 v 0 l -0.4,-0.9 v 0 m 4,0.9 H 554 v 0 0 h 0.4 v 0 h 0.8 v 0 0 h -0.8 v 0 m 436,33.7 -4.9,1.2 v -4.9 l -7.3,-2.4 -3.7,-2.9 v -2 l -6.9,-4.5 -16.6,-12.6 -9,-5.6 m -39.4,58.5 2.5,-0.9 0.4,-3.6 1.6,-0.8 3.6,1.6 7.4,-0.8 h 20.3 l 0.8,-4.5 -1.2,-0.4 -5.3,-49.1 h 9.3 m 321,13.8 v -2.1 l 2,-7.3 2,-4.4 h 2 m 0,0 1,2.8 4,3.7 7,1.6 6,8.1 -2,4.9 -3,1.6 -2,4.5 -2,-1.6 -2,5.2 v 3.7 l -3,1.2 -3,1.6 -1,3.7 h -4 l -2,1.2 -1,3.7 h -5 l -5,2.4 m -11,-46.3 -2,-0.4 m -674,3.6 v 0 0 0 -0.8 -0.8 h -0.8 v -0.8 -0.8 h -0.8 v 0 0 h 0.8 v 0 0.8 l 0.8,0.8 v 0 l 0.4,0.8 v 0.8 h 0.8 -0.8 v 0 H 565 m 676,-3.2 3,2.4 m -690.8,0 v 0 0 0 0.8 0 0.4 0 -0.4 0 h 0.8 v 0 0 1.3 0.8 0 0 h -1.6 0.8 v 0 0 h 0.8 v 0 h -0.8 v 0 l -0.8,-0.8 v -0.9 -0.4 l 0.8,-0.8 m 690.8,0 v 0.8 l 4,7.3 15,1.3 v 0 m -693.1,-8.2 v -0.4 h 0.8 v 0.4 0 l -0.8,0.9 v 0 -0.9 0 m 874.1,14.3 -3,0.4 -4,-2.1 -4,2.1 -2,2.4 1,2.9 -3,3.6 -4,1.6 -4,2.9 -4,4.9 -4,3.6 -5,2.4 -1,3.7 -3,1.2 h -2 l -2,3.3 -4,0.4 -1,3.6 v 4.5 l 2,5.3 -2,4.5 -1,2 v 9.3 h -2 l -3,3.7 1,1.6 -5,2.9 -1,2.8 -2,1.6 -3,-1.6 -3,-2.8 -1,-6.5 -3,-7.4 -5,-8.1 -1,-4.9 -2,-5.2 -4,-7.4 -1,-5.6 -2,-9 -1,-7.3 2,-4.5 -3,-2 -1,-5.3 -2,5.3 -5,2 -3,-0.8 -6,-5.7 v -2 l 5,-1.6 -6,-1.6 -3,-2.1 -2,-3.6 m -770.3,0.8 v 0 h 0.8 l 0.9,0.8 v 0 h -0.9 l -0.8,-0.8 v 0 0 m 3.7,0 h 0.8 l 0.8,1.2 v 0.8 0.8 0 h 0.8 v 0 0.8 h 0.8 v 0.8 0 h -0.8 v -0.8 0 0 H 567 v -0.8 0 -0.8 0 -0.8 h -0.8 v -0.4 l -0.8,-0.8 v 0 m 482.6,3.6 -4,2.1 -4,-2.9 -8,-2 m 0,0 -24,15 -9,8.2 -8.6,1.6 m 57.6,-22 5,-2.4 13,6.1 18,10.2 12,6.5 m 449,-11.8 -4,-0.4 -4,-2.5 v -4.9 h -4 l -3,-2.4 m 0,0 -2,0.8 -1,2.1 -4,1.6 -3,-1.6 -1,2.4 -4,-2.4 -2,2.4 m -946,14.6 -5.7,-0.8 -4.4,0.8 -2.9,-1.6 2,-2 -0.8,-1.7 h -4.4 l -2.1,-1.2 -1.6,-3.6 -2,0.4 -4.5,-1.2 -2.8,-1.7 -6.5,-0.8 -0.9,-2.8 h -3.6 l -3.7,2.8 h -2.8 l -2.9,2.5 V 783 l 1.3,-1.6 5.2,-2.1 4.9,-0.8 h 9 l 2.8,1.6 4.5,2.9 h 2 l 7.3,3.7 0.8,1.2 8.2,2.4 1.6,2 h 2.8 l 2.1,2.5 0.8,1.2 -3.7,1.6 m 474,34.1 -1,-4 4,-6.9 7,-6.5 v -11 l 1,-6.5 1,-2 -4,-6.9 -1,-8.6 m -475.7,2.1 v 0 h -0.8 l -0.8,-0.8 v 0 h 0.8 v 0 l 1.2,0.8 h -0.4 m 887.7,9.7 -2,-8.9 -2,-1.6 -3,1.6 -4,4.5 -2,-0.9 -3,1.7 m -872.5,-3.3 v -0.4 h 0.8 l 0.4,-0.8 0.8,-0.8 v 0 0 -0.8 l -0.8,0.8 v -0.8 0 h 0.8 v -0.8 1.6 0 l -0.8,0.8 v 0.8 h -0.4 v 0 l -0.8,0.4 m 674.5,18.3 16,-5.7 3,-11.8 -2,-3.6 m 314,0.8 -2,-0.8 m 2,0.8 v 1.6 l -2,-2.4 m 0,0 -1,2.4 m -994.4,0 -0.8,-0.8 v 0 H 578 v 0 0 h -0.8 v 0 h 0.8 v 0 h 1.6 v 0 0.8 0 m 933.4,-0.8 3,2.8 2,0.9 -1,2.8 m -4,-6.5 -3,1.2 1,1.6 v 3.7 l -2,0.8 -1,-2.4 m -1390.7,-3.7 -0.7,1.6 -1.5,-0.8 v -0.8 l 2.2,-0.4 v 0.4 M 1574,783.8 v 0.4 0 m 0,-0.4 v 0.4 m 0,0 -10,3.7 h -4 l -4,3.2 2,2.9 -3,1.2 -1,-3.7 v -3.6 l -3,0.8 -3,-2.9 -3,2.1 m -380,-2.9 1,5.3 2,11 4,4.9 2,1.6 m -9,-22.8 h -63 m 0,0 v 11.8 h -6 v 2.9 M 1462,785 v 2.9 l -2,3.2 m -876.3,-4.4 v 0 l -0.4,-0.9 v 0 l 0.4,-0.8 v 0.8 0.9 m 1.6,0 v 0 0 h -0.8 v 0 0 -0.9 h 0.8 v 0.9 0 0 m -460.7,0.8 0.8,2 h -1.5 l -0.7,-2 h 1.4 m 1377.4,7.3 2,-2.5 1,-2.8 3,-1.6 m 37,0 -4,3.2 -3,0.4 -1,3.7 -3,1.6 -2,5.7 1,2.4 4,3.7 -1,0.8 5,4.9 2,2.4 6,6.5 2,8.1 v 2.1 l 1,2.8 -1,1.6 v 4.5 l -1,2 -9,5.3 h -4 l 1,1.2 -1,3.7 -5,2.4 -2,2.1 -2,0.8 v -5.3 l 1,-2.8 -3,-2.1 M 574.3,791.5 v 0 -0.4 l 0.8,-0.8 v 0 l 0.9,-0.8 0.4,0.8 v 0 0 h 0.8 v 0 l 0.8,-0.8 v 0 -0.8 0.8 0.8 0 l -0.8,0.8 v 0 h -0.8 l -0.4,0.4 -0.9,-0.4 -0.8,0.4 m 942.7,-2 2,2 2,1.7 2,-2.1 3,2.1 2,2 -2,3.6 -2,-0.4 -2,2.1 5,2.8 4,3.7 1,2.8 5,3.7 4,6.1 v 0.8 l 2,2.8 -1,2.9 m -47,23.9 -2,-2 2,-1.6 v -3.7 l -1,-4 V 835 l -1,-3.3 -3,-6.5 v -2.8 l -4,-9.3 -1,2 -6,2.8 -1,2.9 -3,-0.4 -4,-1.7 2,-8.1 -4,-10.1 -2,-4.5 h -3 v -2 l -3,-1.7 -1,-1.2 m -1325.1,2.1 h -1.5 l -0.7,-1.7 0.7,-0.4 1.5,1.2 v 0.9 m 745,25.5 v -2.8 l 2.4,-6.5 v -4.5 l -1.6,-2.8 0.8,-6.9 -1.6,-2 -2.9,-0.9 m 627,3.7 -2,-1.2 m 0,0 -5,2.8 -6,1.2 -1,0.9 v 5.2 h -2 l 2,4.9 4,4.5 1,2.8 -1,5.3 -2,0.8 1,2.9 4,5.6 v 3.7 l 3,6.9 -3,4.1 -2,1.6 v 2 m -1355.7,-49.5 -2.2,2 -1.5,-1.2 v -0.8 l -0.7,-2.1 1.4,-1.6 v -1.6 l 3,1.6 1.4,2 h 0.8 l -0.8,1.7 h -1.4 m 1418.7,2.8 -4,3.7 -6,-2.1 v -4.4 l 3,-3.7 8,-0.8 2,2.8 -2,1.7 -1,2.8 m -28,24.8 2,-1.2 -1,-2.9 1,-3.2 -5,-4.1 v -4.5 l -4,-6 -4,-0.5 -3,3.7 -4,-2.4 -4,3.6 h -1 l 1,-2.8 v -2.1 l 1,-3.6 -2,-3.3 h -3 V 796 m -919.5,11.8 -1.2,-1.6 -3.7,0.8 -3.6,-0.8 -3.3,0.8 -2,-0.8 1.6,-2.1 2,0.8 5.3,0.8 0.4,-2.4 -0.4,-1.2 v -2.4 l -3.2,-0.9 3.2,-2 4.9,1.6 m 0,0 4.5,-1.6 3.6,2 h 2 l 0.9,1.7 h 2.8 l 4.5,3.6 -0.8,2.1 -7.4,-0.5 -3.6,1.3 h -2.9 l -1.2,2.8 -2.4,-2 m 0,0 0.8,-1.6 -0.8,-7.8 m 511.5,1.3 v 21.1 h -4 l -1,3.2 -3,2.9 v 2.8 l -3,1.6 2,2.1 -2,2 6,8.9 -1,2.9 M 533.3,800.5 h 1.6 v 0 h -0.8 -0.8 m 1324.7,0 v 0 h -1 1 v 0 m -867.6,0.8 v 15.8 l -1.2,3.3 -2.5,2 -11.8,0.8 -2,1.2 -3.6,0.8 m 282.7,-9.3 -6,-13.4 m 0,0 -18,3.7 -13,14.6 v -5.7 l -10,-3.7 h -7 l 1,3.7 -2,2 m 428,14.6 2,-1.2 2,4.5 2,0.4 -1,2.4 -3,-2.4 -1,-2 -4,-2.9 -2,0.8 -2,-0.8 -2,2.1 -4,-2.1 v -1.6 l 2,-2 -5,-0.8 -1,-5.7 2,-2.5 v -2 -2.4 -4.1 l 1,-3.7 4,-1.6 4,2.1 2,5.2 1,1.7 -1,2.8 -5,7.3 2,5.7 2,1.6 1,-2.4 3,0.8 1,2.8 m -1066.1,-23.1 -1.2,-0.8 -1.7,0.8 -0.8,0.8 -0.8,-0.8 h -1.2 l -2.4,-1.6 -1.3,-0.8 0.9,-0.5 2,-0.8 5.3,0.8 0.4,0.5 2.4,0.8 v 1.6 h -1.6 m 57.7,-0.8 h -0.4 v 0 0 l -1.6,0.8 h -0.9 v -0.8 0.8 l -0.8,-0.8 -0.4,0.8 h -0.8 v -0.8 0 l -0.8,0.8 v 0 h -0.8 v 0 h -0.8 v 0 -0.8 -0.8 0 -0.8 h -0.4 v 0 -0.5 l 0.4,-0.8 v 0 h 0.8 v 0 h 1.6 v 0 l 0.8,0.8 h 0.4 1.7 0.8 v 0 0 h 0.8 0.4 0.8 v 0 0.5 0 0.8 0 l -0.8,0.8 v 0 m 7.3,-2.1 v 0 -0.8 h 0.8 l -0.8,0.8 m -132.1,3.7 0.5,-1.6 1.6,0.8 1.6,-2.9 h 1.2 m 0,0 h 0.8 v 2.1 m 125.2,-2.1 v 0 0 h 0.8 v 0 0 h -0.8 m 8.1,1.3 v 0 l 0.4,-0.8 v 0 0 l -0.4,0.8 v 0 m 0.4,0.8 v -0.8 0 0 0 h 0.8 v 0 0 0 0.8 0 m -13.8,0 v 0 l -0.8,-0.8 h 0.8 0.8 v 0 0 0 l -0.8,0.8 v 0 m 13.8,0 v 0 h -0.8 v 0 0 m 0.8,0 h -0.8 v 0 0 0 0 0 0 m -133.7,0 -0.8,2 v 0.8 5.3 l -1.2,1.2 -1.6,0.8 -0.8,1.7 m 680.9,-11.8 1,3.6 3,10.2 h 3 l 2,2.8 5,2.5 2,3.6 4,2.1 v 1.6 l 4,2.8 m -35,-8.1 -1,-4.9 3,-6.9 1,-3.6 2,-2.1 4,-0.8 2,-2.8 m -542,0.8 v 0 0 0 0 0 0 0 m 0,0 v 0 0 0 0 0 h 0.8 v 0 0 H 632 v 0 m 0,0 v 0 0 h 0.8 v 0 0 0 0 0 0 H 632 v 0 0 0 0 0 0 0 0 0 0 0 0 m -138.9,11 h -1.3 v -6.1 -4.1 m -16.2,17.5 0.8,-2.9 -0.8,-0.8 2.8,-4.5 h 6.5 v -1.6 l -1.2,-0.4 -0.8,-2.4 -1.6,-0.8 -1.2,-1.3 h 2 v -2.8 h 9.7 m 129.3,0.4 0.8,-0.4 h -0.8 0.8 v 0 0 0 h 0.8 v 0 h 0.8 -0.8 v 0 0 l -0.8,0.4 v 0 h -0.8 m 7.3,0 v 0 -0.4 0 0 0 0 0.4 m 9.7,0.8 v -0.8 0 h -0.8 l 0.8,-0.4 v 0.4 0 h 0.4 l -0.4,0.8 m -7.3,0 v 0 H 630 v 0 l 0.8,-0.8 v 0.8 0 m 2.8,1.6 v 0 0 h -0.8 l -0.8,-0.8 v 0 0 h 0.8 v 0 0 l 0.8,0.8 v 0 m 199.5,2.1 -0.8,-1.2 1.7,-0.9 0.8,0.9 -1.7,1.2 m -195,-0.8 v 0 h -0.8 v -0.4 h 0.8 v 0 0 h 0.4 v 0.4 h -0.4 m -2.4,1.6 v 0 0 -0.8 0 0.8 h 0.8 -0.8 m -572.5,0 v 0 0 0 m 1188.8,0.8 -4,0.8 -2,2.9 m -343.4,6.5 -3.6,-2.1 -2.9,-5.3 -2.8,-0.8 -2.5,-2 -6.5,0.8 h -3.6 l -0.8,2 m -240.6,-1.6 v 0 0 -0.4 0 0 h 0.8 v 0 0.4 0 h 0.9 v 0 0.8 h -0.9 -0.8 v 0 0 0 0.8 0 l -0.8,0.9 v 0 l -0.4,-0.9 v -0.8 -0.8 0 h 0.4 v 0 0 h 0.8 v 0 m 606.7,2.5 v 2 l -5,2.4 -4,0.4 -7,3.3 -2,2 h -4 l -3,2 -4,1.7 -6,0.8 -3,2.8 -6,0.8 -2,-1.6 -4,-15.4 1,-3.7 m -317.5,15.5 -3.3,-7.4 3.3,-4.8 0.4,-1.7 M 641,819.6 v 0 h -0.9 0.9 v -0.9 0.9 0 0 0 m -144.3,1.2 2.9,-1.2 0.8,0.8 h 1.6 1.2 3.7 l 3.2,-0.8 h 1.3 2.8 l 2.4,0.8 h 1.3 l 1.6,1.2 -0.8,0.8 1.6,0.8 h 2.8 l 0.8,1.2 m -30.8,-4.8 1.6,1.2 0.8,-0.4 1.2,0.4 m 0,0 -2,1.6 -2.9,2 v 0.8 0.9 l -0.8,1.6 M 640.1,824 v 0 -0.8 -0.8 0 l -0.8,-0.8 v 0 -0.8 h 0.8 l 0.9,0.8 v 0 0.8 0.8 0 0 l -0.9,0.8 m 202,1.2 -0.8,-0.8 v -1.2 l 1.6,1.2 -0.8,0.8 M 1746,824 v 0 -0.8 h 1 l -1,0.8 v 0 m -807.6,25.6 2.4,-8.1 2,-0.8 2.5,-3.7 v -2 l 1.2,-2.4 4.5,1.6 2,-4.5 1.6,-1.6 7.3,-2 2.1,-2.1 5.3,1.2 m -445.4,-0.8 h -1.6 l -2,1.7 -3.7,0.8 -2.4,-0.8 -0.4,1.6 -0.8,0.4 -0.8,0.8 -2.1,2.4 -1.6,-1.6 -1.6,1.6 h -2 v 2.9 H 504 l -0.8,1.2 H 502 m 21.9,-11 -0.8,0.8 0.8,2.9 -1.6,2.4 -0.8,2.9 0.8,5.7 -1.2,0.8 v 2.4 0.4 l -0.8,2.5 v 0.8 l 1.2,1.2 m 447.8,-22 -0.9,2.5 2.9,4 1.6,0.9 v 2.8 l 5.7,3.7 2.4,2.4 m 213,-2.8 -3,-3.7 -3,-2.4 -2,-3.7 -7,-2 -6,0.8 -3,-2.5 -2,2.5 -5,1.2 m -520,-0.8 v -0.4 h -0.8 v 0 l -0.4,-1.6 -0.8,-0.9 v 0 h 0.8 v 0 h 0.4 v 0.9 0 l 0.8,0.8 v 0 0.8 0 0.4 m 264.1,11 v -3.7 l -3.7,-2.8 v -3.7 l -0.8,-2.8 m 623.4,23.5 h 2 l 4,-3.2 2,-4.1 2,0.4 4,-2.8 2,-0.8 v -3.7 l 1,-2 -2,-3.7 1,-3.6 m 0,0 -4,2 -1,-1.2 -7,2.8 v -1.6 m -1043.4,3.6 -2.1,-1.2 h -3.2 -1.2 l -2.5,-1.6 -2,-2 m 1054.4,1.2 -3,-0.4 -4,0.4 -2,-0.4 -3,1.2 -2,2.4 -1,1.3 v 4.4 l 1,1.7 v 1.2 0.8 m -1025,-13 4.5,2.8 h 1.2 2.5 l 0.4,0.8 -0.4,2.1 m -12.6,-1.7 0.8,-0.4 3.6,-3.6 m 659,26.8 3,-7.3 h 1 l 1,-4.5 4,-4.9 2,-0.8 2,-8.1 m -520.8,2.8 v 0 -0.4 h -0.4 v -0.8 h 0.4 v -0.8 0 l 0.8,0.8 v 0 0.8 0 l -0.8,0.4 v 0 m 398.8,0 -4,1.7 -3,2 -5,-2 -7,0.8 -2,2.8 -6,-0.8 -5,-2.8 -3,2 -4,-2.8 -4,-2.1 -6,1.2 -2.4,1.7 v 2 l -2.1,3.3 -0.8,4 m -487.5,-9.3 -0.9,1.6 -4.4,-0.8 -2.9,-0.8 -2.8,-0.8 -1.6,-0.9 m 556.4,3.7 -2,-3.7 m -161.5,0.9 h 5.6 l -1.6,2.4 -4.9,0.4 m 0,0 0.9,-2.8 m 734.5,6.5 -4,-6.5 h 4 l 2,2.4 1,2.8 -3,1.3 m -98,1.6 -2,-0.8 -3,-2.9 h -5 l 1,-3.6 h -5 v 6.5 l -5,10.1 v 3.7 l 2,2.4 h 2 l 1,2.9 1,1.6 v 3.7 1.2 l 5,3.6 h 2 l 2,2.9 m 229,-37.8 -1,-0.8 h 1 v 0.8 m -1239,1.2 -1.6,-2 h -1.2 M 641.8,835 H 641 v 0 -0.8 -0.8 h 0.8 v 0 0.8 0 0.8 0 m 8.5,0.4 h -0.8 v 0 -0.4 0 -0.8 0 0 0 l 0.8,0.8 v 0 0 0 0 0.4 0 m 392.7,0 2,-0.4 3,5.7 v 7.3 l 3,3.7 -4,0.8 h -3 l -2,1.2 7,6.9 2,4.9 m -37,14.6 1,-2 v -3.7 l 7,-6.5 h 3 l 4,3.7 3,-6.9 2,-4.9 3,-0.8 v -3.7 l 1,-0.8 1,-2.8 3,-5.7 4,-2.5 v -3.6 l -2,-0.8 -1,-3.7 m -164.4,3.7 v -3.7 m -368.1,10.6 -1.2,-0.8 -2.8,-2.9 -1.6,-2.4 -1.7,-0.8 -3.6,-2.9 0.8,-0.8 h 1.6 m 696,1.6 2,4.5 -1,2.8 m -5,-5.6 4,-1.7 m 56,0 1,0.8 2,-0.8 v 0.8 l 1,0.9 -1,0.4 h -2 l -2,-0.4 v -0.9 l 1,-0.8 m -375.4,2.1 5.7,-0.4 2.5,-1.7 7.7,0.8 m 0,0 h 3.6 l 3.7,1.3 h 5.3 m -19.1,7.3 1.6,-2.9 4.9,-0.8 v -4.9 m -299.9,1.3 -0.4,-0.4 v -0.9 l 0.4,0.9 v 0.4 m 1044.4,7.3 -2,-0.4 -1,-3.3 -4,-2.8 -1,-2.1 h 5 l 2,2.1 v 3.6 l 1,2.9 m -442,0 h -5 l -1,-2.9 3,-4.8 m -639.6,20.3 3.7,3.6 V 859 l 4.4,-3.7 h 1.7 l 0.8,-6.5 3.6,-2.8 1.3,0.4 2.8,-2.1 3.7,0.9 9.7,-6.5 0.8,3.6 m 320.2,-3.2 3.6,2.4 2.1,-1.6 3.2,0.8 4.1,-1.6 0.8,3.6 2.4,1.6 0.4,5.3 1.7,1.2 m 55.6,-9.3 2.1,-2.4 3.6,3.6 m -386,-2 v 0 l -1.6,-0.8 -0.8,-0.8 h 0.8 l 0.8,0.8 h 0.8 v 0.8 m 287.3,5.7 -2,-0.4 -0.9,-3.3 h -2 l -4.5,-3.6 m -275,0.8 v 0 0.8 0 0 0 0 0 0 0 0 -0.8 0 0 h -0.8 v 0 0 0 0 0 0 -0.8 0 0 0 l 0.8,0.8 m 34.9,0 v 0 l 0.8,0.8 h -0.8 v 0 0.8 h -0.4 v 0 l 0.4,-1.6 v 0 0 0 m -51.6,2.4 -3.2,1.2 2.4,4.5 -2.8,5.3 2.8,4 2,-1.2 v -2.8 l -2,-4.5 0.8,-2.4 7.3,-2.1 -1.6,-2 1.6,-2.4 2.1,4.4 h 4.4 l 2.1,1.7 m 491.2,10.1 3,-2.8 4,-0.8 2,-1.7 3,-0.8 2,3.3 2,0.4 h 6 l 2,-2 6,1.6 5,-2.5 3,1.7 h 4 l 4,-6.5 1,-4.5 3,-0.8 2,6.5 3,4.5 1,2.8 m 376,-4.9 -3,-0.8 -6,-6.1 -1,-2 m -317,3.6 5,5.3 h 4 l 4,-2.4 5,0.8 4,-2 h 3 l 11,-2.5 4,-2 2,0.8 v 3.7 3.6 l -1,0.4 -1,5.3 -3,4.5 -2,3.6 -4,5.7 v 3.7 l -6,8.9 -6,7.3 -5,4.5 -8,4.1 -5,4.4 -8,9 -2,3.6 m -217.1,-68.2 2,-2.1 h 3.7 l 2.4,-2.8 m -394.1,0.8 -3.2,0.4 m 1041.3,2.5 -2,2.8 -4,1.6 v -6.9 l 6,2.5 m -638.3,-2.5 v 1.6 l 1.6,3.7 -1.6,5.3 -3.6,3.6 -0.8,6.5 v 8.2 m -398.6,-28.9 -4.9,6.9 -0.8,6.5 3.6,4.9 -0.8,4.5 2.5,2.4 10.1,0.4 3.7,4.5 10.1,-0.8 -1.6,5.3 v 4.4 l 1.6,4.5 1.7,1.2 -3.3,2.9 3.3,2.4 2,6.5 m 588.1,-54.9 -2,2.1 m 439,2.4 h -2 l -2,-3.6 3,-0.9 1,3.7 v 0.8 M 643,846 v 0 0 l 0.8,-0.8 v 0 l 0.8,-0.9 v 0.9 0 l -0.8,0.8 v 0 0 H 643 m -132.5,-0.8 3.7,1.2 1.6,-0.4 1.6,0.4 1.3,1.6 h 1.6 0.8 l 0.4,-0.8 m -11,-1.2 v -0.8 m 92.3,0.8 1.6,3.6 10.2,-0.8 1.6,1.2 5.7,1.7 2.8,-2.1 8.1,-1.6 -0.8,1.6 2.9,4.1 2.4,-1.2 2.8,0.8 2.9,2 -2.9,5.3 3.7,-0.8 h 3.7 m -20.4,-13.4 v 0 0.8 h -0.8 v 0 l -0.8,-0.8 v 0 l -0.8,0.8 v 0 l -0.8,-0.8 v 0 0 l 0.8,-0.4 0.8,0.4 v 0 h 0.8 v 0 -0.4 0 h 0.8 v 0 0.4 m 345.8,0 -5.7,-0.4 m 0,0 0.4,2 2.5,2.8 1.2,9.8 v 5.7 l -0.4,2 1.2,3.3 2.4,1.2 m -7.3,-26.8 -3.2,0.4 h -11.4 v 1.6 l 1.2,6.5 m -428.6,8.1 -0.4,-0.8 -3.3,-1.2 -0.4,-0.8 0.4,-1.6 v -0.9 l -2,-1.2 -1.6,-1.6 h -2.1 l -0.8,-2 -1.2,-1.7 v 1.7 h 0.4 v 0.8 l -0.4,1.2 -1.6,-1.2 -1.7,-0.8 -0.4,-0.8 -0.8,-1.7 1.2,-1.2 V 846 m 686.5,0.4 -1,2.4 4,6.5 2,2 17,5.3 h 5 l -5,5.3 -11,11.4 h -6 l -4,1.6 -1,1.6 -6,2.1 m -140,-19.1 h 1 l 4,-1.2 2,0.4 4,-2.1 h 5 l 3,-3.6 v -1.7 l 7,-0.4 6,-6.1 1,-2 4,-2.4 2,0.8 M 976.6,872 v -14.7 l -1.7,-4.8 -2.8,-2.5 0.8,-3.6 m -353.1,0.8 v 0 H 619 v 0 -0.8 0 h 0.8 v 0.8 m 277.1,10.1 -1.6,-4 -3.7,-1.6 -3.6,-5.3 m -366.5,0.8 0.8,2.4 1.6,2.1 2.1,2 0.8,0.8 m 563.2,-7.3 4,5.3 v 3.6 m 528,-0.8 -1,-2.8 3,-0.8 1,-4.5 2,0.8 -2,4.5 1,3.6 -1,0.8 -3,-1.6 m -980.2,-8.1 h 0.4 v 0 h 0.8 l -0.8,0.8 v 0 0 1.6 0.4 0 0.8 l -0.4,0.9 v 0 h -0.8 -0.9 -0.8 -0.8 -0.4 v 0 h -0.8 v 0 0 h 0.8 l 0.4,-0.9 h 0.8 V 850 h 0.8 l -0.8,-1.2 v 0 0 l -0.8,-0.8 v 0 0 0 h 1.6 v 0 h 0.9 v 0 l 0.8,-0.8 v 0 m 961.2,4.5 3,-4.5 1,2.4 -3,2.1 h -1 m 24,0 2,-4.5 1,2.8 -3,1.7 m -701.6,-0.9 1.6,-1.2 2.8,1.2 4.9,-2 v 2 l 3.7,-1.2 m 15.4,4.9 -2.8,-2 -5.7,0.8 -3.7,-0.8 -3.2,-2.9 m 563.6,22 -2,-3.3 -2,-3.6 -3,-2.1 -1,-2 -2,-0.8 1,-3.7 2,-6.1 m -572.5,14.7 1.2,0.8 2.5,-4.9 v -5.3 l -1.6,-1.6 0.8,-2.9 m 706.6,0.9 v 2 h -3 l 2,-2.9 1,0.9 m -719.2,8.9 -2.1,-4.5 -2.8,-4.4 -6.5,0.8 -0.8,2.8 -3.7,2 m 730.1,9 -2,-2 -2,1.2 -1,-1.2 -2,4 -1,-0.4 2,-5.3 4,-0.8 2,-2.8 4,2.8 3,-4.5 2,0.9 2,-1.3 v -3.2 l 4,3.6 1,6.1 v 3.7 l -2,2 -3,-2 v 2 l 1,4.5 -1,2 -2,-1.2 -4,-1.6 -2,-1.2 -1,-3.7 2,-1.6 -2,-1.6 -2,-0.4 v 2 m -223,3.6 -2,2.9 -4,1.6 -2,-0.8 -2,-4.5 V 861 l 1,-5.7 2,-1.6 2,2.4 3,4.5 3,5.7 -1,3.6 m -877.2,-15.4 -1.6,-0.8 v 0.8 2.4 l 0.8,0.4 -0.8,1.7 v 1.6 0.4 1.6 m 1.6,-8.1 1.6,0.8 0.4,2 h 0.8 1.7 l 1.2,0.9 1.6,0.8 2,-0.8 1.7,-0.9 2,-1.2 3.2,-2.4 2.9,0.8 v 0.8 h 2.8 l 2.9,0.8 1.6,2.1 1.2,0.8 m 605.6,21.9 -1,-3.6 -4,-1.7 -1,-5.7 -6,-5.2 -4,-0.4 2,-3.7 4,-0.8 v -5.3 m -196.2,0 0.8,7.3 -1.6,1.6 -2.8,6.5 2.4,5.7 v 3.3 M 1094,856.1 v 2.9 h 3 v 2 l 4,0.8 2,3.7 6,4.4 -1,1.7 5,3.6 2,3.7 m -208.7,-10.6 -4.9,-2 -2.4,-3.7 -2.1,-2 v -3.3 m -344.5,9.8 -1.6,-1.6 -1.3,-2.9 v 0 l 0.9,-2 h -1.7 v -0.8 l -2,-1.6 -1.6,-0.9 h -1.7 l -1.2,0.9 v 1.6 l -2.4,1.2 h -1.2 v 1.6 l 2,2.1 -2,2.4 h -2.5 l -0.4,-1.6 -3.2,-1.2 -1.2,-2.5 h -2.5 l -0.4,-0.8 h -3.2 v 1.6 m 29.2,-3.6 v 0.8 l 0.8,2.8 v 1.7 l -1.2,1.2 -0.8,-0.8 -0.8,2.4 m 95.1,-7.3 4.4,2.8 3.7,3.7 v 2 l 2.8,0.8 4.5,4.5 -0.8,2.8 m -18.3,1.7 -3.7,-4.5 0.9,-2.8 4.4,-2.5 -1.6,-2.8 5.3,-4.5 -1.6,-1.2 m 275,4.9 -2.8,2.4 -2.5,-0.8 V 861 l -4.4,-0.4 m -6.5,7.7 5.3,-4 1.2,-3.7 m 773.2,5.7 v -0.8 -0.8 l 1,0.8 -1,0.8 m -758.6,16.2 v -8.1 l -1.2,-2.4 -4.5,-1.2 2,-1.7 -1.2,-4.4 m 128.5,0.8 -4,6.5 -2,5.3 3,7.3 6,5.7 v 3.6 m 815,-26.8 v -0.8 0 0.8 m -1321.9,30.5 -0.4,-1.2 2,-3.7 3.7,-1.6 3.6,-4.9 -1.6,-2.4 0.8,-2.1 -1.2,-8.9 1.2,-2 -2.8,-3.7 m 807.6,2 -1,1.7 -1,-1.7 1,-0.8 1,0.8 m 452,0 v -0.8 0 h 1 l -1,0.8 m -884.6,13.4 -8.5,-3.6 -6.5,-6.1 -5.3,-2.9 -0.8,-1.6 m 675.7,11 h 1 l 2,-1.2 3,-5.3 2,-2 4,-0.9 2,0.9 1,3.6 3,0.8 4,2.9 m -102,-6.5 2,-0.8 4,1.2 m -494,8.1 -2,1.6 -5,-0.8 -3,1.6 -4,0.4 -3,-2.8 -1,-2.8 -4.8,-5.7 h -4.5 -4.4 m 529.7,0.4 7,5.3 1,2 1,3.2 -1,3.7 v 3.7 l 2,2 3,5.7 h -5 l -7,-3.7 -3,-3.6 -1,-2.1 -4,-3.6 v -4.5 l -1,-1.2 v -5.3 l -2,-2 m -519.7,0 -5.7,0.4 m 531.4,0 v 3.6 l 2,-1.2 2,0.8 V 872 m -535.4,0 -2.1,0.8 m 0,0 -1.6,1.6 -3.6,0.8 -5.7,2.9 -4.5,0.8 -1.6,1.2 -4.9,-1.2 m -290.5,-2.5 1.6,-2.8 9.4,0.8 0.8,-0.8 5.7,0.8 -0.9,2.9 m 0,0 0.9,-2.1 5.2,1.2 5.7,4.5 1.7,2.9 m 787.8,-6.5 3,0.8 6,-0.8 2,2 2,4.5 5,2.8 5,4.5 1,2.4 h 2 l 3,2.9 4,2 1,2.4 6,2.1 1,1.6 -1,4.9 2,1.6 3,0.8 1,3.6 2,2.9 4,0.8 2,4.1 v 3.2 l -2,11 -2,-2.5 -4,1.7 -5,-4.5 -1,-0.8 -6,-4.5 v -1.2 l -4,-2.4 -4,-5.7 v -1.6 l -2,-5.7 -3,-2.9 -1,-1.6 -3,-1.2 -2,-6.9 -2,-2.1 -4,-2.8 v -2.4 l -4,-4.1 h -2 l -5,-5.3 -2,-4.5 2,-0.8 2,1.7 m -817.9,-0.9 -2,2.5 -2.5,5.7 2.5,2.8 6.5,8.1 m 448.4,-16.6 -2,-0.8 -3,0.8 -5,-1.6 -2,1.6 h -5 l -6,2 -2,-0.8 -2,2.8 -10,-0.4 -4,-3.6 -2,-0.8 -4,4.4 -1,4.1 m -390.3,6.9 2,-3.2 0.9,-3.7 -2.1,-2.8 v -4.9 l 1.2,-1.6 m 273.9,1.6 -8.1,-1.6 -7.3,1.6 -9.8,3.6 m -283.6,-4.4 h 2.8 l 1.7,3.6 1.2,2.9 -1.2,2 -0.8,4.5 1.6,4.4 2.8,2.9 3.7,0.8 6.5,-4.5 4.5,0.8 m -55.7,3.7 2.8,2 6.5,-1.2 4.5,-2.4 1.6,-2.1 -1.6,-6.5 1.6,-4.4 h 3.7 l 9.3,-1.7 2.9,-1.2 1.6,-3.6 m 960.2,0 -5,1.2 2,3.2 -5,0.4 m -19,-2 v 0 0 h 1 1 l 1,-0.8 1,-1.2 h 1 v 0 0.4 0 0 m -467,-0.4 1,2 3,2 3,-1.2 h 5 l 2,2.9 3,2 m 445,-5.7 v 0 0.8 h 1 v 0.8 l 1,0.4 1,0.9 v 0 0 h 1 v -1.3 -0.8 0 -1.6 0 h 1 v -0.8 m 1,0 -1,0.8 v 0 0 0 0.8 1.6 h 1 1 v 0 0 -0.8 0 -0.8 l -1,-0.8 v -0.8 m 0,0 v 0 m -563,14.2 1,-4.5 -2,-3.6 -3,-1.6 -2,-3.7 m 177,4.5 h -3 l -2,-1.7 -5,2.5 -2,2 -8,-1.2 -6,-4.5 -5,-0.8 m -11,2 2,-2 h 9 m 394,13.8 1,2.5 6,0.4 1,-4.9 2,-2.4 7,-1.3 5,-6.1 1,-2 m -218,0.8 v 0 h -1 v -0.8 l 1,0.8 m 195,13 v 2.5 l 4,4 4,-1.2 2,0.8 3,-3.2 3,-0.4 5,2 1,-1.6 h 4 l 1,-2.1 v -2 l 3,-3.6 v 0 l 2,-7 v -0.4 h 8 l 2,0.4 M 1149,911 v -6.1 l 1,-2.9 5,-4.8 -1,-5.3 -1,-2 -4,-7 m 0,0 -2,2.5 -3,-0.8 -6,2 -3,-1.2 -3,1.2 m 464,-3.7 1,3.3 h -3 l 2,4.1 2,3.2 -1,2.9 4,2 2,2.8 -4,-0.4 -2,0.4 -2,3.7 v 3.6 l -3,3.7 -2,1.6 v 5.3 l -3,4.9 -1,1.6 -6,2 -1,-3.6 h -4 l -3,-2.1 -3,2.1 h -4 v -2.1 h -9 v -4.4 -5.3 l -4,-2 -2,-2.9 -1,-6.1 1,-3.6 4,-2.9 m -861.8,-10.9 2.8,0.8 0.8,4.4 2.9,7.4 h 2.8 v 3.6 l -4.5,4.5 -2.8,2 -4.5,8.1 6.9,-3.6 v 2 0 l 0.4,2.4 v 0 l 4.5,0.5 5.3,-0.5 2.8,-3.2 1.7,-2.8 4,-0.9 3.7,2.5 9.7,2.8 2.1,4.5 2.4,0.8 3.6,-0.8 4.1,2 6.9,0.8 h 6.5 l 8.1,4.5 7.4,6.5 2,0.8 h 3.6 l 3.7,0.8 0.8,2.1 2,8.1 v 4.5 l -2,5.6 -6.1,8.2 -3.6,1.2 -3.7,7.3 -3.6,4.5 h -2.1 l -1.6,2.8 0.8,3.3 -0.8,3.6 0.8,6.1 -1.2,8 -0.8,3 -1.7,2 -0.8,7 -3.6,7 -1.2,2 -1.7,4 v 2 l -4.8,3 -1.7,3 h -7.3 -1.6 -4.1 l -0.8,2 -5.3,2 -6.5,4 -5.6,4 -3.3,4 v 11 l -0.4,6 -3.3,2 -3.6,6 -1.2,3 -2.5,1 -2,-1 -2.8,5 -1.7,1 -1.2,6 -3.2,6 -2.9,3 m -6.5,-203.5 2,0.4 2.5,-0.4 4.5,0.4 1.2,-0.4 3.6,-8.1 1.7,-1.6 m 498.8,0.8 -5,5.7 v 19.9 l 4,4.4 m -175,-28 v 1.6 h -2 l 1,-2 1,-0.8 v 1.2 m 346,-0.4 v 0.4 h -1 l 1,-0.4 v 0 m -294,0.4 -6,-0.4 -4,0.4 -3,7.3 m 78,-7.3 v 6.1 l 3,1.2 -1,2.5 -6,4.8 -2,3.7 v 8.1 m -59,-26.4 v 1.6 l -3,5.7 v 6.1 3.7 l -2,4.8 -4,4.5 -4,4.5 1,5.7 -2,3.6 -2,0.8 -4,3.7 h -3 v -2.9 l -4,3.7 -2,-1.6 m 320,-40.2 v 0.8 l -1,-0.8 v 0 h 1 m -691.4,5.2 2,-2 6.1,-1.6 2,1.6 m 352.3,0.4 h -8 l -1,-0.4 m 0,7.3 -2,-1.6 2,-3.7 v -2 m 19,0.4 -10,-0.4 v 0.4 m 25,0 -2,1.6 -7,-1.6 h -6 m -10,0 v 6.9 h -9 m 7,26 2,-2.8 2,-4.9 6,-0.8 6,0.8 1,-3.7 1,-6.9 -3,-2 v -2 l 3,-3.7 -1,-2.4 h -4 l -2,-1.2 1,-3.3 m -444.8,34.5 2.9,-14.6 -0.8,-5.3 -2.1,-2 v -3.6 l 0.4,-6.5 h 9.8 l 4.5,-2.5 2,5.3 m 746.1,-4.5 h -1 -1 1 1 m -1228.7,1.7 h -0.7 l 0.7,-0.9 0.7,0.9 h -0.7 m 1520.7,3.6 3,-0.8 v 2.8 l 2,2.5 -5,-1.6 -1,-3.7 1,-2.8 2,1.2 -2,2.4 m -14,0.8 -3,2.9 -4,0.8 -3,-1.6 h -7 l -4,0.8 -3,-0.8 -2,2 -1,2.4 2,2.9 2,2.8 h 2 l 2,-2.8 4,0.8 v -1.6 h 4 l -3,4.4 -2,0.8 -2,2.9 2,0.8 2,4.5 v 2.8 l 1,0.8 v 2.9 l -2,0.8 -1,1.2 -3,-0.8 1,-2.9 -3,-2.8 v -4.5 l -2,-0.8 -2,1.6 v 1.7 7.7 4.4 l -2,1.7 -3,-2.5 1,-4 v -3.3 l -1,-2.8 -2,0.8 v -5.3 l 2,-3.6 V 913 l 2,-3.6 1,-7.4 3,-0.8 2,-2.8 10,2.4 6,0.4 3,-1.2 3,-3.6 1,1.2 -1,3.2 m -274,-2.4 v 0 -0.8 0.8 m 162,0 v 0 0 0 l -1,0.8 v 0 l -1,-0.8 v 0 0 -0.8 h 1 l 1,0.8 v 0 m -975.9,-0.8 3.7,3.2 M 539.4,924 v -3.7 l -3.3,-2 v -4.5 l 1.7,-5.7 2.8,-3.2 v -3.7 l 6.1,-2 0.4,-1.6 m 479.9,29.2 -2,-3.6 -5,-3.7 -2,-4.1 -3,-5.2 3,-2.9 V 902 l 2,-1.2 m -469.2,0 6.9,3.7 4,-0.8 4.5,2.8 M 25.17,901.2 v 0 0 0 M 1363,902 h -1 -1 l 1,-0.8 1,0.8 m -360,3.7 -1,-1.2 1,-0.8 h 1 l -1,2 M 25.9,904.5 v 0 0 0 m 456.2,6.5 h -2 v -0.8 l 1.2,-1.7 -1.2,-1.2 v -1.6 h -1.7 l 1.7,-0.8 0.8,0.8 0.4,1.6 0.8,0.8 0.8,1.3 -0.8,1.6 m 84.1,-4.5 4.5,2 0.8,2.5 2.8,1.2 1.7,3.2 2,0.5 0.8,3.2 h 3.7 l 2,-1.6 h 5.3 l 4.4,2.8 -3.6,5.3 2,0.8 1.6,2 m -28,-21.9 v 2 l -1.2,5.3 -3.3,2.9 -2.8,2.8 -6.5,2.4 -2.9,2.1 -3.6,8.1 -6.1,-5.7 -0.4,-2.4 m 167.4,-10.2 -9,1.6 -1.2,-2.4 v -3.6 l 1.2,-2.9 5.3,0.8 2.1,-0.8 4.4,0.8 -0.8,2.9 -2,3.6 m 651.2,-7.3 v 0.8 l -1,-0.8 v 0 h 1 m 363,48.4 -6,-6.1 h -6 l 1,-1.3 v -1.6 l -1,-1.2 v -2.4 l -2,-2.9 -2,-4.5 -4,-2.8 h -3 l -5,-2 -3,-0.8 -5,-2.9 -4,-1.6 -2,2.8 h -2 l 1,-2.8 -2,-2 v -2.5 l 3,-1.2 -6,-2.4 -1,-2.1 -1,-0.8 -4,-0.8 1,-2.8 4,-0.8 3,-2.1 4,1.2 1,0.9 h 3 l 1,3.6 v 1.6 3.7 l 3,1.2 v 2.4 l 2,1.3 h 2 l 4,-3.7 1,-2 h 2 l 2,-2.1 3,-1.6 5,1.6 6,2.9 h 2 l 5,1.2 M 113.7,908.1 v 0 0 0 0 m 1745.3,0.4 v 0 0 0 m -1372.4,0 -0.8,1.7 -1.3,-0.8 0.4,-0.9 h 1.7 m 874.4,0.9 -1,0.8 v -0.8 0 h 1 m 137,6 -2,-0.8 -2,-2.8 2,-0.8 2,4.4 m -367,-3.6 1,-0.8 h 17 m 28,19.5 -7,-4.9 -1,-3.7 -19,-10.9 h -1 m -18,0.8 1,2.8 v 3.7 h -1 m -5,-5.3 2,0.8 3,-2 m -5,1.2 -2,2.4 -2,2.9 1,2 m 232,-5.7 v 0 h -1 l 1,-0.8 v 0.8 m 182,6.5 -4,-0.8 -1,-3.6 h -2 v -2.1 l 4,-0.8 1,4.5 2,1.2 v 1.6 m -347,-6.5 -4,2.1 v 1.6 l -4,2 v 2.9 l -4,7.3 h -1 m -46,-12.2 1,3.6 -4,6.5 -3,0.9 m -2,-9 h 4 l 4,-2 m 415,2.8 -1,1.7 h -2 v -3.3 l 3,1.6 m 175,-1.6 10,3.7 3,0.8 3,2.4 h 2 l 3,2.9 4,2 v 3.6 l 3,0.9 4,1.6 2,0.4 2,3.7 h -4 l 2,4.4 3,2.9 1,2.4 9,8.5 1,2.5 -5,-1.7 -7,-0.8 -1,-0.4 -2,-2.4 -5,-5.7 -1,-2.4 -8,-2.1 -2,0.8 -5,4.5 1,2 -5,1.7 -2,-0.8 h -4 -2 m 0,-35.4 v 35.4 m -365,-34.6 v 0 -0.8 l 1,0.8 h -1 m -231,9 -1,-2.5 v -4.9 l -1,-1.6 m 537,0 5,2.5 2,2 -1,1.6 -4,-2.4 -5,-0.8 -3,0.8 -2,-2.9 h 6 l 2,-0.8 m -13,4.5 -3,1.6 -4,-2.4 1,-1.2 3,-0.9 3,0.9 v 2 m -1055.2,80.2 -4.5,-3 -1.2,-2 -19.1,-11 -4.5,-4.1 -1.2,-3.2 V 978 l -5.3,-9.3 -2.8,-2.1 -6.1,-14.6 -2.8,-3.6 -2.1,-4.5 -2.4,-2.9 -4.9,-3.2 1.2,-2 -2.8,-5.3 0.8,-2.1 5.3,-4.4 m 1242.6,2.4 4,2 -3,1.7 -1,-3.7 m -750,6.5 -1,-2.8 -4,-3.3 m 332,0 v 0.8 l -1,-0.8 h 1 v 0 M 596.3,964.2 591,965 v -8.9 l -4.1,3.2 h -4.4 l -0.9,-2.4 -4.4,-0.8 1.6,-2.1 -3.7,-3.6 -0.8,-5.3 v -2.8 l 2.9,-2.1 v -2.4 l 1.6,-4.1 4.9,-3.6 6.9,-2.5 3.6,0.8 m 669.8,2.9 -1,-2 1,-1.7 2,3.7 h -2 m 501,4.5 6,-0.9 h 3 l 1,-2.8 3,-0.8 -1,-3.7 4,1.7 1,2 -3,2.4 v 2.1 l -8,3.6 h -4 l -7,-2.8 v -1.7 l 5,0.9 m -733,-2.9 4,-3.6 2,1.2 m 87,-1.2 v 2.8 l 1,4.5 v 4.4 l 3,2.9 3,5.7 m -94,-19.1 -3,2.4 -2,3.7 m 144,-6.1 -2,7.3 v 2.4 l 4,3.7 -1,8.5 2,2.5 1,4.4 2,2.9 m -150,-25.6 -1,-3.7 m 766,4.9 2,2.4 -1,2.1 -2,-0.8 -1,-2.1 -2,-2 2,-1.6 2,2 m -760,-0.4 -4,0.4 -1,-1.2 m 5,0.8 h 8 9 1 l 3,7.3 2,4.1 5,-0.4 h 6 v -5.3 h 4 l 1,1.6 h 7 l 1,6.5 -1,4.9 3,5.3 -1,2.8 6,0.8 4,-0.8 m -65,35.8 v -9 l 1,-1.2 1,-4.5 2,-7.3 5,-4 2,-4.5 V 965 l -3,-5.7 -2,-5.3 3,-3.6 -7,-12.6 5,-0.4 m 496,0 4,0.4 h 3 l 2,1.6 h 3 l 2,2.1 9,1.6 2,-2.9 3,1.3 1,-0.5 4,1.3 h 2 l 1,4.4 3,0.8 4,-0.8 2,1.7 -1,3.6 -3,-0.8 -2,-0.8 -3,0.8 -6,-1.6 h -5 l -3,-1.2 -5,-1.7 -4,0.8 -5,-1.6 -5,-0.8 v -1.2 l -3,-1.6 -1,-2.1 1,-2.8 m 275,7.7 -3,-0.4 -2,-2.4 -1,-0.8 1,-0.5 3,2.1 v 0.8 l 2,1.2 m -455,0 v -0.4 0 0.4 m 353,5.3 h -4 l 2,-3.7 4,-1.6 1,1.6 -1,2.1 -2,1.6 m 114,0.8 -4,-2.4 -2,-1.3 v -0.8 l 2,1.7 h 2 l 2,2 v 0.8 m -11,0.8 -1,-2.4 h -2 l 1,-1.2 1,0.4 2,2.4 -1,0.8 m -227,-3.2 2,2.4 -2,0.8 -3,-1.6 3,-1.6 m 53,1.6 -4,0.8 v -2.4 l 4,1.6 m -37,-0.8 4,0.8 v 1.6 l -10,2.9 -1,-3.7 2,-0.8 3,2 2,-2.8 m -455,6.5 -11,-4.9 -1,-1.6 m 0,0 -10,1.6 -2,4.5 1,2.8 v 4.5 l -2,4.9 1,1.6 4,2.8 3,-1.6 -1,6.1 -2,1.2 -5,-5.7 -2,-0.8 -4,-3.6 -3,2.4 -7,-2.4 v -1.3 l -5,0.4 -2,-2.8 m 494,-11.8 -3,0.8 1,-2.8 v -0.8 l 3,0.8 -1,2 m 24,-2 2,1.6 -2,1.2 -6,-0.8 2,-2.8 4,0.8 m 215,4.5 v 1.2 l -3,-2.1 v -3.6 0 l 1,1.6 v 0.4 l 2,2.5 m 95,-2.9 v -0.8 h 1 l -1,0.8 m -289,1.2 1,-1.2 4,-0.8 h 3 v 2 l -6,2.5 -1,0.4 m -1,-2.9 1,2.9 m 0,0 -4,4.4 -3,0.9 h -2 l 1,-3.7 2,-2 2,-0.8 3,-1.7 M 50.77,954.9 v 0 0 0 0 M 1826,958.5 v 0.8 l -2,-0.8 h -3 l -1,-1.6 v -1.2 l 2,0.4 h 2 l 1,0.8 1,1.6 m -218,-2.8 5,3.6 -1,1.2 h -3 l -3,-2 -2,-0.8 1,-2 h 3 m -464,0.4 2,0.8 4,-0.8 2,3.6 1,5.3 1,2.9 m -8,13.4 -3,-2.5 3,-8.1 v -4.9 l 2,-3.6 -2,-5.3 -2,-0.8 m -547.7,8.1 7.3,0.8 1.6,-2 4.1,-2.5 3.2,-1.2 2.9,-0.8 h 3.6 l -0.8,3.7 0.8,4.8 2.1,3.3 2.8,2.8 h 3.7 l 10.5,5.7 4.1,-0.8 3.2,2.5 0.4,4.4 0.8,8.1 9.8,1 v 6 l 3.7,1 0.4,3 -2.9,9 v 2 M 1834,963.4 h -2 l -2,-1.2 v -1.7 l 2,1.7 h 1 l 1,1.2 m -302,-1.2 v -0.8 0 0.8 0 m -349,0 2,1.2 -2,3.2 2,7.8 -1,3.6 2,7.3 -2,3.7 -4,6 -3,2 -7,3 -3,2 -6,6 -2,2 -5,4 v 4 l 2,3 2,11 -1,4 v 2 l -1,3 -8,3 -5,3 -1,3 1,4 m 11,-86.1 7,0.8 4,-0.8 3,0.8 3,-2.1 2,0.4 5,-1.2 5,-3.6 m -87,2 V 976 h -11 v 17 l 1,2 8,7 m -496.9,-1 -0.8,-2 2.8,-3 -2,-6.2 0.8,-3.7 2,-3.6 -0.8,-2.9 0.8,-4.4 1.6,-2.1 -5.2,-8.9 m 1137.7,15.4 2,3.7 3,-0.8 5,3.6 v 8.9 l 4,7 -1,3 2,3 7,6 6,2 v 3 l 2,2 3,7 6,2 1,4 3,4 5,5 3,4 1,11 2,5 -2,17 -4,9 -4,2 -4,10 -2,5 -3,5 -1,7 v 4 l -2,2 h -6 l -5,2 -3,4 -5,1 -4,-3 -1,-4 -8,7 -6,-3 h -4 l -7,-4 -3,-4 v -3 l -3,-9 -3,2 -1,-6 -2,-3 -3,4 1,-7 1,-2 -1,-3 -3,4 -4,3 -4,6 -2,-4 v -3 l -5,-5 -1,-5 -8,-3 h -3 l -5,-3 -11,1 -9,4 h -8 l -10,6 -3,4 h -8 -11 l -4,4 h -1 l -5,4 h -9 l -4,-4 -3,-2 -1,-3 5,-3 v -8 l -4,-9 v -7 l -5,-8 v -4 l -2,-4 2,-2 v -5 l -4,-8 2,-4 v -6 h 3 l 2,-4 4,-1 7,-6 4,1 19,-6 2,-3 2,-3 3,-3 -1,-4 3,-4 h 5 l 4,-5 6,-8.5 h 5 l 3,-2.9 5,4.5 9,2 -1,-2.8 2,-5.3 2,-0.4 1,-3.6 4,-1.7 5,-0.8 3,-0.8 V 967 l 11,3.3 10,0.4 v 6.1 l -4,2 v 2.9 l -2,4.4 7,4.9 5,2 2,3 5,1 2,3 3,2 5,-2 3,-7 1,-6 -1,-6.5 1,-2.9 v -5.3 l 1,-1.6 2,-6.5 3,0.8 1,4.5 2,0.8 1,4.9 v 4.4 m -534,-9.3 -1,-0.8 V 967 h 1 v 0.9 2.4 m 20,-2.4 v 0 0 0 0 m -66,0 h -1 l -2,2.8 2,6.9 2,2 4,5.3 v 7.1 l -2,1 v 6 l -7,-7 3,-3.4 -2,-5.3 -3,0.8 -2,-2.8 m 339,-9.8 v -0.8 0 0.8 0 M 1215,991 v -2 l 4,-1.2 2,-1.7 1,0.9 1,-2.1 v -2.4 l 1,-0.8 v -2.1 -0.8 h 2 l 2,-1.2 2,-3.2 -2,-1.3 2,-1.6 3,0.8 v 1.7 l 1,0.4 1,4.4 1,3.7 v 1.6 l 1,2.9 v 0.8 2 l -1,2.2 -1,-1 -1,-2 -1,0.8 1,3.2 v 3 l -1,1 -1,3 1,2 -1,3 -3,8 v 1 l -2,3 -1,5 -1,3 -1,4 -1,2 -1,6 -1,3 -2,3 -2,2 h -2 -2 l -2,2 h -2 l -2,-2 -4,-2 -1,-2 -1,-1 -1,-8 v -1 l -1,-3 1,-3 v -1 -2 h 2 v -2 l 1,-1 2,-4 -1,-2 1,-1 -1,-2 v -2 l -1,-2 v -4 l -1,-2 2,-4 1,-1 v -2 h 3 l 2,-1 h 1 l 2,-1 h 2 m -6,-15 -1,-1.6 h 1 V 976 M 46.38,978 h 1.46 l 0.73,0.8 v 1.7 H 47.11 L 46.38,978 m 6.58,3.7 -2.93,-0.4 0.74,-0.8 2.19,1.2 m 1077.04,8.1 -1,-3.7 17,-4.8 m -1128.87,2 v -0.8 0 0.8 m 39.49,0 v 0 0 h -0.74 v -0.8 0 h 0.74 v 0 h 0.73 v 0 0 h -0.73 v 0.8 m 1803.38,4.5 1,2 -2,1.2 -1,-2 v -1.2 l -1,-2.5 2,0.8 v 1.7 h 1 m -730,2 h -3 l -5,2.2 -1,3 -5,3 -4,5 -2,1 -7,-2 m 32,27 6,-7 v -4 l 2,-2 v -9 l 1,-7 -1,-4 -4,-1 -4,-3 h -5 v -2.2 M 937.2,992 v -1 l 0.8,1 h -0.8 m 924.8,4 -1,-1 v -2 l 3,1 -2,2 m 65,-1 -2,1 v 0 l -2,1 -1,-1 2,-1 2,-1 v -1 h 3 v 0 l -2,2 m -833,7 -12,2 -5,-2 h -9 l -2,-2 h -22 l -6,-3 -3,3 h -4 m 165,-2 v 0 0 0 0 m -151,32 -5,-10 -5,-10 -4,-7 v -3 m 888,4 -1,1 -3,-1 -1,-2 h 1 l 1,-1 1,-1 h 2 l 2,2 1,2 h -1 -2 m -825,-2 3,-2 4,1 2,1 m -493.7,29 h -3.7 l -0.4,-5 -1.6,-4 -2.1,-4 -0.8,-3 2.1,-3 -2.9,-2 -1.6,-6 -1.2,-3 m -5.3,4 2.4,-1 0.4,-2 2.5,-1 m 1268.9,1 v 0 l 1,-1 1,1 h -2 m -1694.6,0 h -2.2 l 1.5,-1 0.7,1 m 931.6,0 4,7 v 3 l 6,3 3,3 v 3 l 2,3 5,1 2,2 m -51,16 v -17 h 6 v -21 l 12,-2 2,3 4,-3 5,-1 m -500.2,233 -4.5,-1 -2.8,2 -5.7,2 -0.8,9 -1.7,2 -4,-2 -2.5,-2 6.1,-4 v -3 l -6.1,5 -2,-1 -0.8,-3 -3.7,-3 0.8,-4 -3.6,-9 0.8,-2 -1.6,-5 v -12 l 1.6,-2 0.4,-5 -3.7,-1 3.3,-6 -1.6,-1 h -2.9 l -2.8,-2 3.6,-3 0.4,-2 h 3.3 l 2,2 1.7,-3 v -6 l 3.6,-2 0.4,-2 -2.8,-1 0.8,-4 v -4 l 1.6,-2 -0.8,-5 1.2,-1 -0.4,-4 -2.4,2 h -2.9 l -1.6,-6 1.6,-7 h 1.7 l 1.2,-5 -2.1,-6 v -8 h 2.1 l 0.8,-3 2,-8 2.5,-3 0.8,-6 2,-4 1.6,-7 -1.6,-9 v -4 l 2,-3 -1.2,-6 2,-3 0.9,-8 h 0.8 l 2,-11 -0.8,-3 0.8,-5 -0.8,-5 1.6,-1 1.6,-8 -0.8,-4 v -4 l -1.6,-10 m 1280.2,4 -2,-1 1,-1 1,2 m -1810.99,1 h -0.73 l 0.73,-1 0.73,1 h -0.73 m 596.59,5 v -1 l -4.9,-3 h -5.2 l -9.4,2 -2.4,5 v 3 l -2.1,6 m 641.4,-9 -1,-1 1,-2 h 1 v 1 1 l -1,1 m -598.3,29 1.6,-7 -2.4,-3 -2.8,1 -1.7,-3 -0.4,-5 -1.6,-2 -8.1,-1 -2.9,-1 0.8,-5 -1.6,-6 m 1201.4,14 -4,-2 -2,-3 h -2 l -3,-2 v -1 l -2,-1 -2,-2 v -2 h 2 l 2,2 2,1 2,3 2,1 4,3 h 1 v 3 m -594,-8 h 1 l -1,2 -2,-1 v -1 l 1,-1 1,1 m 596,0 h -1 l 1,-1 1,1 h -1 m -1827.78,1 -1.47,-1 h 0.74 0.73 v 1 m 81.88,0 h -0.7 v -1 h 0.7 v 1 m 494.2,9 2.4,-4 2,-1 h 7.4 l 2.4,1 1.2,3 1.6,-4 h 6.5 l 0.8,1 m 491.4,0 10,2 m -10,-2 -6,2 -4,5 -3,2 -1,3 -2,2 -3,1 -1,5 -5,1 -4,-1 -3,-2 -3,1 -2,4 -5,5 -5,-1 1,-3 -2,-7 -3,-1 m -440.4,-16 3.7,5 4.9,5 5.3,1 6.5,5 6.1,2 0.4,2 -2.9,4 -0.8,3 -2,2 8.1,2 4.9,-1 h 3.2 l 1.2,-1 3.7,-4 0.8,-5 m 462.3,2 v -9 l -3,-6 -1,-5 m -79,36 -4,-3 -2,-5 -3,-9 v -5 l -2,-5 v -5 -3 m -442.2,205 -8.6,-3 h -10.5 l -2.1,-5 0.9,-5 -1.7,-3 -3.6,2 -2.1,-5 0.9,-7 4.8,-4 0.8,-4 -0.8,-4 3.7,-5 0.8,-4 -0.8,-9 2.8,-1 -1.2,-3 2,-3 -3.6,-1 0.8,-4 -2,-7 0.4,-9 0.8,-10 1.6,-5 0.8,-5 2.9,-1 -1.7,-7 -0.8,-5 1.7,-3 2.8,-2 0.8,-10 2,-3 0.9,-5 -4.5,-11 1.6,-6 2,-3 -0.4,-6 2.1,-5 2,-2 2.4,-6 2.9,-1 -2.1,-2 1.3,-2 -1.3,-5 0.5,-3 1.6,-3 5.3,-2 1.2,-6 -0.8,-1 m -326.7,9 h 0.7 v 0 h -0.7 v 0 m 791.4,3 v 21 l -2,1 -2,2 h -5 l -3,-1 -3,-4 -3,3 m -379.3,-18 h 3.7 l 0.8,4 -0.8,5 -5.3,2 -5.7,6 -2.8,4 -6.1,7 m 478.5,-26 -3,-2 -4,5 3,4 4,1 v -3 m 0,0 v -5 m 4,5 h -4 m 4,0 -2,11 -6,4 -5,10 -8,9 -5,5 -4,3 -4,1 h -4 l -5,3 -6,-1 h -6 l -4,3 -3,-1 -8,3 -7,-7 -3,-6 2,-1 1,-2 -1,-4 -5,-8 -2,-8 -2,-3 m 67,9 -3,1 -2,3 -4,-2 -2,-5 h 1 l 3,-4 5,-2 4,4 -2,5 m 741,-6 v 0 -1 h 1 l -1,1 m -1180.8,29 -0.8,-4 v -3 l -2,-4 -3.7,-3 -6.5,-3 -5.3,-5 h -4.4 m 0,0 -3.7,14 m 26.4,8 -4.5,6 -3.6,2 -4.1,-1 -3.2,1 -4.9,-3 h -3.7 l -2.8,-4 0.4,-9 m 0,0 -1.2,8 v 5 l 6.5,4 -0.8,4 1.6,3 2,2 0.8,3 -4.4,6 -0.8,2 -7.8,4 -9.7,2 h -6.5 v 3 l -2,6 0.4,4 -4.1,2 h -3.7 l -6,-3 -1.3,2 2.1,8 2,1 3.7,-1 v 5 l -4.1,1 -4.5,5 0.8,3 -3.2,6 -6.5,3 -2.9,7 1.7,4 2.8,3 4.9,2 v 2 l -1.2,4 -5.3,5 -4.5,5 v 5 l -4.9,3 -2.4,6 0.8,5 3.7,7 m 1298.2,-118 3,2 3,-2 v 6 l 6,2 5,-3 2,3 v 4 l -2,3 -5,1 -1,8 -4,6 -4,4 -3,-4 2,-6 v -2 l -7,-4 v -2 l 4,-2 2,-7 -3,-9 -2,-2 -5,-6 2,-2 5,2 v 3 l 2,3 v 4 m -13,26 4,5 4,-1 3,5 -6,8 -2,2 -1,3 -7,5 -1,5 -3,7 -5,5 -4,1 -4,-2 -8,-2 -2,-1 5,-8 5,-7 8,-3 8,-8 1,-5 2,-1 2,-6 1,-2 m -144,2 4,2 7,-1 h 5 v 6 l -3,8 -3,1 -1,4 -6,-1 -3,-7 -1,-4 -3,-7 4,-1 m -1168.9,15 -0.8,3 -3.6,-1 1.6,-2 v -5 l 2,-3 1.7,3 -2.5,2 1.6,3 m -8.9,51 2.8,-6 1.7,9 H 567 l -0.8,-3 m 769.8,-5 1,2 2,-2 1,3 3,-2 h 1 l -1,3 -2,-1 v 3 l -5,-1 v -1 l -1,-3 1,-1 m -684.9,29 -2,-2 3.6,-6 5.7,-1 1.7,3 -7.4,4 -1.6,2 m -5.3,-5 1.7,-3 3.6,1 -2.8,4 -2.5,-2 m -44.3,27 -2.4,1 -3.6,-1 -2.9,1 -7.3,-3 4.5,-3 h 2.8 l 0.8,-5 4.5,-2 v -1 h -6.1 l 1.6,-3 3.7,-5 2,2 2.4,-1 m 0,0 h 1.3 l 2.8,9 2.5,3 6.5,7 2,1 -3.7,2 -2,-1 -9.4,-1 m 0,0 v -20 m 760.5,4 -2,1 1,-1 h 1 m -785.6,7 -0.4,-3 h 3.6 l 3.7,3 h -6.9 m 193.8,4 2.4,-2 3.7,6 -6.1,-4 m 215.7,2 v -1 0 1 0 m -381.5,7 -0.8,-3 h 4.5 l 0.8,4 -4.5,-1 m 0,2 h -7.3 l -1.6,-4 6.5,-1 2.4,5"
            ]
            []
        ]

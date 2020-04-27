module Chart exposing
    ( Config
    , Model
    , Msg(..)
    , Popover
    , init
    , update
    , view
    )

import Axis
import Color exposing (Color)
import Date exposing (Date, Unit(..))
import Element
import Element.Background
import Element.Border
import Html.Attributes
import List.Extra as List
import Maybe.Extra as Maybe
import Path exposing (Path)
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Shape exposing (StackConfig, StackResult)
import TypedSvg exposing (circle, g, line, path, rect, style, svg)
import TypedSvg.Attributes exposing (class, d, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, strokeWidth, width, x, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types exposing (Length(..), Paint(..), Transform(..))


rgba : Int -> Int -> Int -> Float -> Element.Color
rgba r g b a =
    Element.rgba
        (toFloat r / 255)
        (toFloat g / 255)
        (toFloat b / 255)
        a


style_ k v =
    Element.htmlAttribute <| Html.Attributes.style k v



-- Types
------------------------------------------------------------------------------------------------------------------------


type alias Model =
    { hoveredDate : Maybe Date
    , selectedDate : Maybe Date
    }


type alias Config a msg =
    { id : String
    , toMsg : Msg -> msg
    , toDate : a -> Date
    , bars : List { label : String, color : Element.Color, accessor : a -> Float }
    , lines : List { label : String, color : Element.Color, accessor : a -> Maybe Float }
    , items : List a
    , popover : Maybe (Popover a msg)
    , yExtent : Maybe ( Float, Float )
    }


type alias Popover a msg =
    { width : a -> Float
    , height : a -> Float
    , visible : a -> Bool
    , view : a -> Element.Element msg
    }


type alias Line =
    { label : String
    , color : Element.Color
    , items : List ( Date, Maybe Float )
    }



-- Msg
------------------------------------------------------------------------------------------------------------------------


type Msg
    = Noop
    | OnColumnClick Date
    | OnColumnEnter Date
    | OnColumnLeave



-- Settings
------------------------------------------------------------------------------------------------------------------------


padding : { bottom : Float, left : Float, right : Float, top : Float }
padding =
    { top = 30
    , left = 50
    , right = 30
    , bottom = 50
    }


labelWidth : Float
labelWidth =
    94



-- State
------------------------------------------------------------------------------------------------------------------------


init : Maybe Date -> Model
init today =
    { hoveredDate = Nothing
    , selectedDate = today
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        OnColumnClick date ->
            ( { model | selectedDate = Just date }, Cmd.none )

        OnColumnEnter date ->
            ( { model | hoveredDate = Just date }, Cmd.none )

        OnColumnLeave ->
            ( { model | hoveredDate = Nothing }, Cmd.none )



-- View
------------------------------------------------------------------------------------------------------------------------


view : Config a msg -> Model -> { width : Float, height : Float } -> Element.Element msg
view cfg model size =
    let
        -- Lines
        lines : List Line
        lines =
            cfg.lines
                |> List.map
                    (\{ label, color, accessor } ->
                        { label = label, color = color, items = List.map (\v -> ( cfg.toDate v, accessor v )) cfg.items }
                    )

        lineExtent =
            ( 0
            , lines
                |> List.map .items
                |> List.concat
                |> List.map (Maybe.withDefault 0 << Tuple.second)
                |> List.maximum
                |> Maybe.withDefault 0
            )

        -- Dates
        dates =
            List.map cfg.toDate cfg.items

        showN =
            max (round <| (toFloat <| List.length dates) / max (size.width / labelWidth) 1) 1

        -- Bars
        { values, labels, extent } =
            Shape.stack
                { data = List.map (\{ label, accessor } -> ( label, List.map accessor cfg.items )) cfg.bars
                , offset = Shape.stackOffsetNone
                , order = identity
                }

        barsValues =
            List.transpose values

        colors =
            List.map (toColor << .color) cfg.bars

        xScale : BandScale Date
        xScale =
            Scale.band { defaultBandConfig | paddingInner = 0.0, paddingOuter = 0.3 } ( 0, size.width - (padding.top + padding.bottom) ) dates

        yExtent =
            case cfg.yExtent of
                Just yExtent_ ->
                    yExtent_

                Nothing ->
                    if Tuple.second lineExtent > Tuple.second extent then
                        lineExtent

                    else
                        extent

        yScale : ContinuousScale Float
        yScale =
            Scale.linear ( size.height - (padding.left + padding.right), 0 ) yExtent
                |> Scale.nice 5

        maxHeight =
            Scale.range yScale |> Tuple.first

        scaledValues =
            List.map (List.map (\( y1, y2 ) -> ( Scale.convert yScale y1, Scale.convert yScale y2 ))) barsValues

        id_ =
            if String.isEmpty cfg.id then
                ""

            else
                "#" ++ cfg.id
    in
    Element.column
        [ Element.width Element.fill
        , Element.inFront <| popoverView cfg model xScale yScale
        ]
        [ Element.html <|
            svg [ viewBox 0 0 size.width size.height ]
                [ style [] [ text <| """
                      """ ++ id_ ++ """ .column {opacity: 0.7;}
                      """ ++ id_ ++ """ .overlay {cursor:pointer;}
                      """ ++ id_ ++ """ .column.hover,""" ++ id_ ++ """ .column.active {opacity: 1;}
                      """ ++ id_ ++ """ .domain {stroke: #535C83; }
                      """ ++ id_ ++ """ .tick text {fill: #8D9AC0;font-family: Montserrat;font-weight: 600;font-size: 10px;text-transform: uppercase;}
                    """ ]
                , g [ transform [ Translate (padding.left - 1) (size.height - padding.bottom) ] ]
                    [ Axis.bottom [ Axis.tickCount 4, Axis.tickSizeInner 0, Axis.tickSizeOuter 0, Axis.tickPadding 12 ]
                        (Scale.toRenderable (dateToString showN dates) xScale)
                    ]
                , g [ transform [ Translate (padding.left - 1) padding.top ] ]
                    [ Axis.left [ Axis.tickCount 5, Axis.tickSizeInner 0, Axis.tickSizeOuter 0, Axis.tickPadding 8 ] yScale ]
                , g [ transform [ Translate padding.left padding.top ] ] <|
                    List.map (column (isSelected model) (isHovered model) xScale colors) (List.map2 (\a b -> ( a, b )) dates scaledValues)
                , g [ transform [ Translate padding.left padding.top ] ] <|
                    List.map (line (isSelected model) (isHovered model) xScale yScale) lines
                , g [ transform [ Translate padding.left padding.top ] ] <| List.map (overlayColumn cfg.toMsg xScale maxHeight) dates
                ]
        ]


column : (Date -> Bool) -> (Date -> Bool) -> BandScale Date -> List Color -> ( Date, List ( Float, Float ) ) -> Svg msg
column isSelected_ isHovered_ xScale colors ( date, values ) =
    let
        totalWidth_ =
            Scale.bandwidth xScale

        width_ =
            min 40 (totalWidth_ * 0.7)

        x_ =
            Scale.convert xScale date + ((totalWidth_ - width_) / 2)

        block isTop ( color, ( upperY, lowerY ) ) =
            let
                y_ =
                    lowerY

                height_ =
                    abs <| upperY - lowerY
            in
            if isTop && round height_ > 0 then
                let
                    radius_ =
                        min (width_ / 5) 8

                    parts =
                        [ [ "M", String.fromFloat x_, String.fromFloat (y_ + radius_) ]
                        , [ "Q", String.fromFloat x_, String.fromFloat y_, String.fromFloat (x_ + radius_), String.fromFloat y_ ]
                        , [ "L", String.fromFloat (x_ + width_ - radius_), String.fromFloat y_ ]
                        , [ "Q", String.fromFloat (x_ + width_), String.fromFloat y_, String.fromFloat (x_ + width_), String.fromFloat (y_ + radius_) ]
                        , [ "L", String.fromFloat (x_ + width_), String.fromFloat (y_ + height_) ]
                        , [ "L", String.fromFloat x_, String.fromFloat (y_ + height_) ]
                        , [ "L", String.fromFloat x_, String.fromFloat (y_ + radius_), "Z" ]
                        ]

                    d_ =
                        List.concat parts |> List.foldl (\a b -> b ++ " " ++ a) "" |> String.trim
                in
                path [ d d_, fill <| Paint color ] []

            else
                rect [ x x_, y y_, width width_, height height_, fill <| Paint color ] []

        length_ =
            List.length values
    in
    g
        [ class <|
            [ "column" ]
                ++ (if isSelected_ date then
                        [ "active" ]

                    else
                        []
                            ++ (if isHovered_ date then
                                    [ "hover" ]

                                else
                                    []
                               )
                   )
        ]
    <|
        (List.indexedMap (\i v -> block (i + 1 == length_) v) <| List.zip colors values)


overlayColumn : (Msg -> msg) -> BandScale Date -> Float -> Date -> Svg msg
overlayColumn toMsg xScale maxHeight date =
    let
        width_ =
            Scale.bandwidth xScale + 1

        x_ =
            Scale.convert xScale date
    in
    g [ class [ "overlay" ] ]
        [ rect
            [ x <| x_
            , y <| 0
            , width width_
            , height maxHeight
            , fill <| Paint <| toColor <| rgba 255 255 255 0
            , TypedSvg.Core.attribute "id" <| columnId date
            , onClick <| (toMsg << OnColumnClick) date
            , TypedSvg.Events.onMouseEnter <| (toMsg << OnColumnEnter) date
            , TypedSvg.Events.onMouseLeave (toMsg <| OnColumnLeave)
            ]
            []
        ]


line : (Date -> Bool) -> (Date -> Bool) -> BandScale Date -> ContinuousScale Float -> Line -> Svg msg
line _ isHovered_ xScale yScale lineData =
    let
        line_ : Path
        line_ =
            lineData.items
                |> List.map
                    (\( x, y ) ->
                        case y of
                            Just y_ ->
                                Just
                                    ( Scale.convert xScale x + (Scale.bandwidth xScale / 2)
                                    , Scale.convert yScale y_
                                    )

                            _ ->
                                Nothing
                    )
                |> Shape.line (Shape.stepCurve 0.5)

        points_ : List (Svg msg)
        points_ =
            lineData.items
                |> List.map
                    (\( date, value ) ->
                        case value of
                            Just v ->
                                Just <|
                                    circle
                                        [ r 3
                                        , strokeWidth 1
                                        , cx <| Scale.convert xScale date + Scale.bandwidth xScale / 2
                                        , cy <| Scale.convert yScale v
                                        , fill <|
                                            Paint <|
                                                toColor <|
                                                    if isHovered_ date then
                                                        rgba 150 150 150 0.8

                                                    else
                                                        rgba 150 150 150 1
                                        , stroke (Paint <| toColor lineData.color)
                                        ]
                                        []

                            _ ->
                                Nothing
                    )
                |> Maybe.values
    in
    g []
        ([ Path.element line_ [ stroke (Paint <| toColor lineData.color), strokeWidth 1, fill PaintNone ] ]
            ++ points_
        )



-- Popover
------------------------------------------------------------------------------------------------------------------------


popoverView : Config a msg -> Model -> BandScale Date -> ContinuousScale Float -> Element.Element msg
popoverView cfg model xScale yScale =
    case cfg.popover of
        Just popover ->
            let
                item =
                    cfg.items |> List.find (\item_ -> Just (cfg.toDate item_) == model.hoveredDate)

                visible =
                    Maybe.isJust item && Maybe.unwrap False popover.visible item

                popoverWidth =
                    Maybe.unwrap 0 popover.width item

                popoverHeight =
                    Maybe.unwrap 0 popover.height item

                bandwidth =
                    Scale.bandwidth xScale

                px_ =
                    (item |> Maybe.map cfg.toDate |> Maybe.unwrap 0 (Scale.convert xScale)) + (bandwidth / 2)

                x_ =
                    if px_ + (bandwidth / 2) + 10 + popoverWidth > (Tuple.second <| Scale.range xScale) then
                        px_ + padding.left - (bandwidth / 2) - 10 - popoverWidth |> String.fromFloat

                    else
                        px_ + padding.left + (bandwidth / 2) + 10 |> String.fromFloat

                maxY =
                    Tuple.first <| Scale.range yScale

                y_ =
                    if 100 + popoverHeight > maxY then
                        padding.bottom - 30 + maxY - popoverHeight |> String.fromFloat

                    else
                        100 |> String.fromFloat
            in
            Element.column
                (styles.popover
                    ++ [ Element.paddingXY 20 16
                       , Element.spacing 12
                       , Element.width (Element.px <| round popoverWidth)
                       , Element.height (Element.px <| round popoverHeight)
                       , style_ "transform" ("translate(" ++ x_ ++ "px, " ++ y_ ++ "px)")
                       , style_ "visibility" <|
                            if visible then
                                "visible"

                            else
                                "hidden"
                       , Element.behindContent <| Element.el (styles.popoverBg ++ [ Element.width Element.fill, Element.height Element.fill ]) Element.none
                       ]
                )
                [ case item of
                    Just i ->
                        popover.view i

                    Nothing ->
                        Element.none
                ]

        Nothing ->
            Element.none



-- Helpers
------------------------------------------------------------------------------------------------------------------------


isSelected : Model -> Date -> Bool
isSelected model date =
    model.selectedDate == Just date


isHovered : Model -> Date -> Bool
isHovered model date =
    model.hoveredDate == Just date


columnId : Date -> String
columnId date =
    "nc-" ++ Date.toIsoString date


dateToString : Int -> List Date -> Date -> String
dateToString n dates date =
    let
        index =
            List.elemIndex date dates |> Maybe.withDefault 0
    in
    if index == 0 || modBy n index == 0 then
        date |> Date.format "E d MMM"

    else
        ""


toColor : Element.Color -> Color
toColor color =
    Color.fromRgba <| Element.toRgb color



-- Styles
------------------------------------------------------------------------------------------------------------------------


styles =
    { popover =
        [ Element.Background.color <| rgba 100 100 100 1
        , Element.Border.rounded 4
        , style_ "position" "absolute"
        , style_ "top" "0px"
        , style_ "pointer-events" "none"
        , style_ "transition" "transform 400ms ease 0s"
        ]
    , popoverBg =
        [ style_ "pointer-events" "none"
        , style_ "box-shadow" "4px 4px 12px 4px rgba(0,0,0,0.3)"
        ]
    }

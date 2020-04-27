module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Chart
import Date
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html
import Html.Attributes as Attr
import Lamdera
import Time exposing (..)
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , chart = Chart.init Nothing
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        ChartMsg subMsg ->
            let
                ( newChart, _ ) =
                    Chart.update subMsg model.chart
            in
            ( { model | chart = newChart }, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    ( model, Cmd.none )


view model =
    let
        layoutAttrs =
            [ Font.family
                [ Font.external { name = "Montserrat", url = "https://fonts.googleapis.com/css?family=Montserrat" }
                , Font.sansSerif
                ]
            , width fill
            ]
    in
    { title = "Keystone Kitchen"
    , body =
        [ layout layoutAttrs <|
            column [ width fill, padding 20, spacing 20 ]
                [ viewLogo, viewCharts model ]
        ]
    }


viewLogo =
    let
        keystoneLogo =
            "https://keystone.fit/static/media/logo.623420ca.svg"

        lamderaLogo =
            "https://lamdera.app/lamdera-logo-black.png"

        logo =
            image [ width <| px 150 ]
    in
    row [ Border.width 2, Border.rounded 4, Border.color (rgb 0 0 0) ]
        [ el [ padding 20, Background.color <| rgba 0 0 0 1 ] <| logo { src = keystoneLogo, description = "Keystone logo" }
        , el [ padding 20, Background.color <| rgba 0 0 0 0 ] <| logo { src = lamderaLogo, description = "Lamdera logo" }
        ]


viewCharts model =
    column [ width fill, spacing 20 ]
        [ el [ Font.bold ] <|
            text "Chart examples"
        , el [ Font.variant Font.smallCaps, Font.bold, Font.size 16, moveDown 10 ] <|
            text "stacked bar chart with goal"
        , column [ width (fill |> maximum 1024), spacing 10 ]
            [ el [ Border.rounded 4, width fill, Background.color (rgba 0 0 0 0.1) ] <|
                Chart.view chartConfig model.chart { width = 1000, height = 400 }
            , el [ Font.size 16, Font.variant Font.smallCaps ] <| text "chart configuration"
            , el [ Border.rounded 4, Background.color <| rgba 0 0 0 0.1, Font.size 14, Font.family [ Font.monospace ], padding 10 ] <| text """
chartId =
    "my-chart"

type alias Datum =
    { date : Date.Date
    , goal : Maybe Float
    , value1 : Float
    , value2 : Float
    }

april =
    Date.fromCalendarDate 2020 Apr

sampleDays =
    [ Datum (april 1) (Just 10) 5 7
    , Datum (april 2) (Just 8) 6 8
    , Datum (april 3) (Just 11) 4 6
    , Datum (april 4) Nothing 0 4
    , Datum (april 5) (Just 10) 5 7
    , Datum (april 6) (Just 12) 6 7
    , Datum (april 7) (Just 12) 5 7
    , Datum (april 8) (Just 12) 2 4
    , Datum (april 9) (Just 12) 10 5
    , Datum (april 10) (Just 11) 6 5
    ]

chartConfig : Chart.Config Datum FrontendMsg
chartConfig =
    { id = chartId
    , toMsg = ChartMsg
    , toDate = .date
    , bars =
        [ { label = "v1", color = Chart.rgba 0 0 0 0.6, accessor = .value1 }
        , { label = "v2", color = Chart.rgba 0 0 0 0.4, accessor = .value2 }
        ]
    , lines =
        [ { label = "l1", color = Chart.rgba 0 0 0 1, accessor = .goal }
        ]
    , items = sampleDays
    , popover = Just popoverConfig
    , yExtent = Nothing
    }


popoverConfig : Chart.Popover Datum FrontendMsg
popoverConfig =
    { width = always 100
    , height = always 60
    , visible = always True
    , view =
        \\x ->
            column [ width fill, Font.size 14, Font.color <| rgb 1 1 1 ]
                [ el [] <| text <| "V1: " ++ String.fromFloat x.value1
                , el [] <| text <| "V2: " ++ String.fromFloat x.value2
                ]
    }
            """
            ]
        ]



---- CHART ----


chartId =
    "my-chart"


type alias Datum =
    { date : Date.Date
    , goal : Maybe Float
    , value1 : Float
    , value2 : Float
    }


april =
    Date.fromCalendarDate 2020 Apr


sampleDays =
    [ Datum (april 1) (Just 10) 5 7
    , Datum (april 2) (Just 8) 6 8
    , Datum (april 3) (Just 11) 4 6
    , Datum (april 4) Nothing 0 4
    , Datum (april 5) (Just 10) 5 7
    , Datum (april 6) (Just 12) 6 7
    , Datum (april 7) (Just 12) 5 7
    , Datum (april 8) (Just 12) 2 4
    , Datum (april 9) (Just 12) 10 5
    , Datum (april 10) (Just 11) 6 5
    ]


chartConfig : Chart.Config Datum FrontendMsg
chartConfig =
    { id = chartId
    , toMsg = ChartMsg
    , toDate = .date
    , bars =
        [ { label = "v1", color = rgba 0 0 0 0.6, accessor = .value1 }
        , { label = "v2", color = rgba 0 0 0 0.4, accessor = .value2 }
        ]
    , lines =
        [ { label = "l1", color = rgba 0 0 0 1, accessor = .goal }
        ]
    , items = sampleDays
    , popover = Just popoverConfig
    , yExtent = Nothing
    }


popoverConfig : Chart.Popover Datum FrontendMsg
popoverConfig =
    { width = always 100
    , height = always 60
    , visible = always True
    , view =
        \x ->
            column [ width fill, Font.size 14, Font.color <| rgb 1 1 1 ]
                [ el [] <| text <| "V1: " ++ String.fromFloat x.value1
                , el [] <| text <| "V2: " ++ String.fromFloat x.value2
                ]
    }

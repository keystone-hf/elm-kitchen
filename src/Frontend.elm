module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
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
import Maybe.Extra
import SyntaxHighlight exposing (elm, gitHub, toBlockHtml, useTheme)
import Task
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
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , chart = Chart.init Nothing
      , containerSize = Nothing
      }
    , Task.perform identity <| Task.succeed CalcChartSize
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

        CalcChartSize ->
            ( model, Task.attempt GotContainerSize <| getElementBox chartId )

        OnResize ->
            ( model, Task.perform identity <| Task.succeed CalcChartSize )

        GotContainerSize size ->
            ( { model | containerSize = Result.toMaybe size }, Cmd.none )

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
            column [ width fill, padding 20, spacing 20 ] [ viewLogo, viewCharts model ]
        ]
    }


viewLogo =
    let
        keystoneLogo =
            el
                [ height fill
                , width fill
                , Background.uncropped "https://keystone.fit/static/media/logo.623420ca.svg"
                ]
                none

        lamderaLogo =
            el
                [ height fill
                , width fill
                , Background.uncropped "https://lamdera.app/lamdera-logo-black.png"
                ]
                none

        logo =
            image [ width (px 150) ]
    in
    row
        [ Border.width 2
        , Border.rounded 4
        , Border.color <| rgb 0 0 0
        , width (fill |> maximum 320)
        , centerX
        , height (px 90)
        ]
        [ el [ paddingXY 20 0, height fill, width fill, Background.color <| rgb 0 0 0 ] keystoneLogo
        , el [ paddingXY 20 0, height fill, width fill ] lamderaLogo
        ]


viewCharts model =
    let
        viewChart =
            model.containerSize
                |> Maybe.map (\( w, h ) -> Chart.view chartConfig model.chart { width = w, height = h })
                |> Maybe.withDefault none
    in
    column [ width fill, spacing 10, height fill ]
        [ el [ paddingXY 0 10 ] <|
            text "Chart examples"
        , el [ Font.variant Font.smallCaps, Font.size 16 ] <|
            text "stacked bar chart with goal line"
        , column [ width fill, spacing 10 ]
            [ el
                [ Border.rounded 4
                , Border.width 1
                , Border.dashed
                , width fill
                , height (px 320)
                , htmlAttribute <| Attr.id chartId
                , onRight <|
                    newTabLink []
                        { url = "https://github.com/keystone-hf/elm-kitchen"
                        , label =
                            image
                                [ width (px 24)
                                , height (px 24)
                                , moveLeft 24
                                , moveUp 34
                                ]
                                { src = "/gh.png", description = "GitHub Octocat" }
                        }
                ]
                viewChart
            , column
                [ width fill
                , Border.width 1
                , Border.rounded 4
                , Border.dashed
                , Border.color <| rgba 0 0 0 0.5
                , Font.size 14
                , Font.family [ Font.monospace ]
                , padding 20
                , scrollbarX
                ]
                (List.map html
                    [ useTheme gitHub
                    , elm snippetChart
                        |> Result.map (toBlockHtml <| Just 1)
                        |> Result.withDefault
                            (Html.code [] [ Html.text snippetChart ])
                    ]
                )
            ]
        ]


subscriptions : Model -> Sub FrontendMsg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> OnResize)
        ]


getElementBox : String -> Task.Task Browser.Dom.Error ( Float, Float )
getElementBox id =
    Browser.Dom.getElement id
        |> Task.map (\v -> ( v.element.width, v.element.height ))



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
        [ { label = "v1", color = rgba 0 0 0 0.8, accessor = .value1 }
        , { label = "v2", color = rgba 0 0 0 0.6, accessor = .value2 }
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


snippetChart =
    """
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
        [ { label = "v1", color = rgba 0 0 0 1, accessor = .value1 }
        , { label = "v2", color = rgba 0 0 0 0.8, accessor = .value2 }
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
          \\x ->
              column [ width fill, Font.size 14, Font.color <| rgb 1 1 1 ]
                  [ el [] <| text <| "V1: " ++ String.fromFloat x.value1
                  , el [] <| text <| "V2: " ++ String.fromFloat x.value2
                  ]
      }
"""

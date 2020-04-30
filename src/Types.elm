module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Chart
import Dict exposing (Dict)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , chart : Chart.Model
    , linkedChart : Chart.Model
    , containerSize : Dict String ( Float, Float )
    }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | ChartMsg Chart.Msg
    | LinkedChartMsg Chart.Msg
    | CalcChartSize String
    | OnResize
    | GotContainerSize String (Result Browser.Dom.Error ( Float, Float ))
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend

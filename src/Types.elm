module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Chart
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , chart : Chart.Model
    , containerSize : Maybe ( Float, Float )
    }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | ChartMsg Chart.Msg
    | CalcChartSize
    | OnResize
    | GotContainerSize (Result Browser.Dom.Error ( Float, Float ))
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend

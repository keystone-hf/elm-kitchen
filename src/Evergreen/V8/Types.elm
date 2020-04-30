module Evergreen.V8.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Evergreen.V8.Chart as Chart
import Dict
import Url


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , chart : Chart.Model
    , linkedChart : Chart.Model
    , containerSize : (Dict.Dict String (Float, Float))
    }


type alias BackendModel =
    { 
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChartMsg Chart.Msg
    | LinkedChartMsg Chart.Msg
    | CalcChartSize String
    | OnResize
    | GotContainerSize String (Result Browser.Dom.Error (Float, Float))
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
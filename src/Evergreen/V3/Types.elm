module Evergreen.V3.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Evergreen.V3.Chart as Chart
import Url


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , chart : Chart.Model
    , containerSize : (Maybe (Float, Float))
    }


type alias BackendModel =
    { 
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChartMsg Chart.Msg
    | CalcChartSize
    | OnResize
    | GotContainerSize (Result Browser.Dom.Error (Float, Float))
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
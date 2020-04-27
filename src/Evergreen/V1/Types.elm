module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V1.Chart as Chart
import Url


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , chart : Chart.Model
    }


type alias BackendModel =
    { 
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChartMsg Chart.Msg
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
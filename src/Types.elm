module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Chart
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , chart : Chart.Model
    }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | ChartMsg Chart.Msg
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend

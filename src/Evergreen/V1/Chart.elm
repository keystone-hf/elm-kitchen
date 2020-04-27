module Evergreen.V1.Chart exposing (..)

import Date


type alias Model = 
    { hoveredDate : (Maybe Date.Date)
    , selectedDate : (Maybe Date.Date)
    }


type Msg
    = Noop
    | OnColumnClick Date.Date
    | OnColumnEnter Date.Date
    | OnColumnLeave
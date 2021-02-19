module World exposing
    ( World
    , init
    )

{- Types -}


type alias World =
    { rows : Int
    , columns : Int
    }



{- Build -}


init : World
init =
    { rows = 0
    , columns = 0
    }

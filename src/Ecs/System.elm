module Ecs.System
    exposing
        ( Focus
        , Id
        , System
        , focus
        , getComponent
        , having
        , having2
        , setComponent
        )

import Dict exposing (Dict)


type alias System store =
    { store
        | nextId : Id
    }


type alias Id =
    Int


type Focus store component
    = Focus
        { get : System store -> Dict Id component
        , set : Dict Id component -> System store -> System store
        }


focus :
    { get : System store -> Dict Id component
    , set : Dict Id component -> System store -> System store
    }
    -> Focus store component
focus =
    Focus


setComponent : Focus store component -> Id -> component -> System store -> System store
setComponent (Focus { get, set }) id component system =
    set (Dict.insert id component (get system)) system


getComponent : Focus store component -> Id -> System store -> Maybe component
getComponent (Focus { get }) id system =
    Dict.get id (get system)


having : Focus store component -> System store -> List Id
having (Focus { get }) system =
    Dict.keys (get system)


having2 : Focus store component1 -> Focus store component2 -> System store -> List Id
having2 focus1 focus2 system =
    let
        focusGet (Focus { get }) =
            get
    in
    Dict.intersect
        (Dict.map (\_ _ -> ()) (focusGet focus1 system))
        (Dict.map (\_ _ -> ()) (focusGet focus2 system))
        |> Dict.keys

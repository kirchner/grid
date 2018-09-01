module Ecs.System
    exposing
        ( Focus
        , Id
        , System
        , focus
        , getComponent
        , having
        , having2
        , removeComponent
        , setComponent
        , spawnEntity
        , with
        , with2
        )

{-

   Copyright 2018 Alexander Foremny and Fabian Kirchner

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

-}

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


removeComponent : Focus store component -> Id -> System store -> System store
removeComponent (Focus { get, set }) id system =
    set (Dict.remove id (get system)) system


spawnEntity : (Id -> System store -> System store) -> System store -> System store
spawnEntity f system =
    f system.nextId { system | nextId = system.nextId + 1 }


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


with : Focus store component -> System store -> List ( Id, component )
with (Focus { get }) system =
    Dict.toList (get system)


with2 :
    Focus store component1
    -> Focus store component2
    -> System store
    -> List ( Id, ( component1, component2 ) )
with2 focus1 focus2 system =
    having2 focus1 focus2 system
        |> List.filterMap
            (\id ->
                Maybe.map2 Tuple.pair
                    (getComponent focus1 id system)
                    (getComponent focus2 id system)
                    |> Maybe.map (Tuple.pair id)
            )

module AStar
    exposing
        ( Config
        , compute
        )

{-|

@docs compute, Config

-}

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
import Set exposing (Set)


{-| TODO
-}
type alias Config comparable =
    { heuristicCostEstimate : comparable -> comparable -> Float
    , neighbours : comparable -> List comparable
    }


{-| TODO
-}
compute : Config comparable -> comparable -> comparable -> Maybe (List comparable)
compute config start goal =
    let
        initialState =
            { closedSet = Set.empty
            , openSet = Set.singleton start
            , fScore = Dict.singleton start (config.heuristicCostEstimate goal start)
            , gScore = Dict.singleton start 0
            , cameFrom = Dict.empty
            }
    in
    loop config goal (Loop initialState)
        |> Maybe.map reconstructPath



---- ASTAR


type alias StarState comparable =
    { closedSet : Set comparable
    , openSet : Set comparable
    , fScore : Dict comparable Float
    , gScore : Dict comparable Float
    , cameFrom : Dict comparable comparable
    }


type Step comparable
    = Loop (StarState comparable)
    | Done (DoneData comparable)
    | Impossible


type alias DoneData comparable =
    { cameFrom : Dict comparable comparable
    , current : comparable
    }


loop : Config comparable -> comparable -> Step comparable -> Maybe (DoneData comparable)
loop config goal step =
    case step of
        Done data ->
            Just data

        Impossible ->
            Nothing

        Loop newState ->
            loop config goal (iterate config goal newState)


iterate : Config comparable -> comparable -> StarState comparable -> Step comparable
iterate config goal state =
    let
        { openSet, fScore, cameFrom } =
            state

        maybeCurrent =
            openSet
                |> Set.toList
                |> List.map attachFScore
                |> List.sortBy Tuple.second
                |> List.head

        attachFScore node =
            ( node, valueOrInfinity node fScore )
    in
    case maybeCurrent of
        Nothing ->
            Impossible

        Just ( current, currentFScore ) ->
            if current == goal then
                Done (DoneData cameFrom current)
            else
                Loop (checkNeighbours config goal current state)


checkNeighbours :
    Config comparable
    -> comparable
    -> comparable
    -> StarState comparable
    -> StarState comparable
checkNeighbours { neighbours, heuristicCostEstimate } goal current state =
    let
        { closedSet, openSet } =
            state
    in
    List.foldl (checkNeighbour heuristicCostEstimate goal current)
        { state
            | openSet = Set.remove current openSet
            , closedSet = Set.insert current closedSet
        }
        (neighbours current)


checkNeighbour :
    (comparable -> comparable -> Float)
    -> comparable
    -> comparable
    -> comparable
    -> StarState comparable
    -> StarState comparable
checkNeighbour heuristicCostEstimate goal current neighbour state =
    let
        { closedSet, openSet, fScore, gScore, cameFrom } =
            state

        tentativeGScore =
            1 + neighbourGScore

        neighbourGScore =
            valueOrInfinity current gScore

        alreadyChecked =
            Set.member neighbour closedSet

        notCheckedYet =
            not (Set.member neighbour openSet)

        worseGScore =
            tentativeGScore >= neighbourGScore
    in
    if alreadyChecked then
        state
    else if notCheckedYet then
        { state
            | openSet = Set.insert neighbour openSet
            , cameFrom = Dict.insert neighbour current cameFrom
            , gScore = Dict.insert neighbour tentativeGScore gScore
            , fScore =
                Dict.insert neighbour
                    (tentativeGScore + heuristicCostEstimate goal neighbour)
                    fScore
        }
    else if worseGScore then
        state
    else
        { state
            | cameFrom = Dict.insert neighbour current cameFrom
            , gScore = Dict.insert neighbour tentativeGScore gScore
            , fScore =
                Dict.insert neighbour
                    (tentativeGScore + heuristicCostEstimate goal neighbour)
                    fScore
        }


valueOrInfinity : comparable -> Dict comparable Float -> Float
valueOrInfinity key dict =
    Dict.get key dict
        |> Maybe.withDefault (0 / 1)



---- RECONSTRUCT PATH


reconstructPath : DoneData comparable -> List comparable
reconstructPath { cameFrom, current } =
    reconstructPathHelp [ current ] cameFrom current


reconstructPathHelp : List comparable -> Dict comparable comparable -> comparable -> List comparable
reconstructPathHelp path cameFrom current =
    case Dict.get current cameFrom of
        Nothing ->
            path

        Just next ->
            reconstructPathHelp (next :: path) cameFrom next

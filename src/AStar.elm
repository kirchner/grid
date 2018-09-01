module AStar
    exposing
        ( Config
        , Node
        , compute
        )

import Dict exposing (Dict)
import Set exposing (Set)


type alias Node =
    ( Int, Int )


type alias StarState =
    { closedSet : Set Node
    , openSet : Set Node
    , fScore : Dict Node Float
    , gScore : Dict Node Float
    , cameFrom : Dict Node Node
    }


type Step
    = Loop StarState
    | Done
        { cameFrom : Dict Node Node
        , current : Node
        }
    | Impossible


type alias Config =
    { heuristicCostEstimate : Node -> Node -> Float
    , neighbours : Node -> List Node
    }


compute : Config -> Node -> Node -> Maybe (List Node)
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


reconstructPath : { cameFrom : Dict Node Node, current : Node } -> List Node
reconstructPath { cameFrom, current } =
    reconstructPathHelp [ current ] cameFrom current


reconstructPathHelp : List Node -> Dict Node Node -> Node -> List Node
reconstructPathHelp path cameFrom current =
    case Dict.get current cameFrom of
        Nothing ->
            path

        Just next ->
            reconstructPathHelp (next :: path) cameFrom next


loop : Config -> Node -> Step -> Maybe { cameFrom : Dict Node Node, current : Node }
loop config goal step =
    case step of
        Done data ->
            Just data

        Impossible ->
            Nothing

        Loop newState ->
            loop config goal (iterate config goal newState)


iterate : Config -> Node -> StarState -> Step
iterate { neighbours, heuristicCostEstimate } goal ({ closedSet, openSet, fScore, gScore, cameFrom } as state) =
    case
        openSet
            |> Set.toList
            |> List.map
                (\node ->
                    ( node
                    , Dict.get node fScore
                        |> Maybe.withDefault (1 / 0)
                    )
                )
            |> List.sortBy Tuple.second
            |> List.head
    of
        Nothing ->
            Impossible

        Just ( current, currentFScore ) ->
            if current == goal then
                Done
                    { cameFrom = cameFrom
                    , current = current
                    }
            else
                let
                    notInClosed node =
                        not (Set.member node closedSet)

                    folder neighbour intermediateState =
                        let
                            tentativeGScore =
                                Dict.get current gScore
                                    |> Maybe.map ((+) 1)
                                    |> Maybe.withDefault (1 / 0)

                            neighbourGScore =
                                Dict.get current gScore
                                    |> Maybe.withDefault (1 / 0)

                            newCameFrom =
                                Dict.insert neighbour
                                    current
                                    intermediateState.cameFrom

                            newGScore =
                                Dict.insert neighbour
                                    tentativeGScore
                                    intermediateState.gScore

                            newFScore =
                                Dict.insert neighbour
                                    (tentativeGScore + heuristicCostEstimate goal neighbour)
                                    intermediateState.fScore
                        in
                        if Set.member neighbour closedSet then
                            intermediateState
                        else if not (Set.member neighbour intermediateState.openSet) then
                            { intermediateState
                                | openSet = Set.insert neighbour intermediateState.openSet
                                , cameFrom = newCameFrom
                                , gScore = newGScore
                                , fScore = newFScore
                            }
                        else if tentativeGScore >= neighbourGScore then
                            intermediateState
                        else
                            { intermediateState
                                | cameFrom = newCameFrom
                                , gScore = newGScore
                                , fScore = newFScore
                            }
                in
                Loop
                    (neighbours current
                        |> List.foldl folder
                            { state
                                | openSet = Set.remove current openSet
                                , closedSet = Set.insert current closedSet
                            }
                    )

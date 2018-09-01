module AStar exposing (Node, aStar)

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


aStar : Int -> Int -> Node -> Node -> Maybe (List Node)
aStar width height start goal =
    let
        initialState =
            { closedSet = Set.empty
            , openSet = Set.singleton start
            , fScore = Dict.singleton start (h goal start)
            , gScore = Dict.singleton start 0
            , cameFrom = Dict.empty
            }
    in
    loop width height goal (Loop initialState)
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


loop : Int -> Int -> Node -> Step -> Maybe { cameFrom : Dict Node Node, current : Node }
loop width height goal step =
    case step of
        Done data ->
            Just data

        Impossible ->
            Nothing

        Loop newState ->
            loop width height goal (iterate width height goal newState)


iterate : Int -> Int -> Node -> StarState -> Step
iterate width height goal ({ closedSet, openSet, fScore, gScore, cameFrom } as state) =
    case
        openSet
            |> Set.toList
            |> List.filterMap
                (\node ->
                    Dict.get node fScore
                        |> Maybe.map (Tuple.pair node)
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
                        in
                        if
                            Set.member neighbour intermediateState.openSet
                                && (tentativeGScore < neighbourGScore)
                        then
                            { intermediateState
                                | openSet = Set.insert current intermediateState.openSet
                                , cameFrom =
                                    Dict.insert neighbour
                                        current
                                        intermediateState.cameFrom
                                , gScore =
                                    Dict.insert neighbour
                                        tentativeGScore
                                        intermediateState.gScore
                                , fScore =
                                    Dict.insert neighbour
                                        (neighbourGScore + h goal neighbour)
                                        intermediateState.fScore
                            }
                        else
                            { intermediateState
                                | openSet = Set.insert current intermediateState.openSet
                            }
                in
                Loop
                    (neighbours width height current
                        |> List.filter notInClosed
                        |> List.foldl folder
                            { state
                                | openSet = Set.remove current openSet
                                , closedSet = Set.insert current closedSet
                            }
                    )


h : Node -> Node -> Float
h goal current =
    toFloat <|
        abs (Tuple.first goal - Tuple.first current)
            + abs (Tuple.second goal - Tuple.second current)


neighbours width height ( x, y ) =
    let
        outOfBounds ( neighbourX, neighbourY ) =
            (neighbourX < 0)
                || (neighbourX > width)
                || (neighbourY < 0)
                || (neighbourY > height)
    in
    List.filter outOfBounds
        [ ( x, y + 1 )
        , ( x - 1, y )
        , ( x + 1, y )
        , ( x, y - 1 )
        ]

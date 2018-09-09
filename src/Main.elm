port module Main exposing (main)

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

import AStar
import Dict exposing (Dict)
import Ecs.System as Ecs exposing (Focus, Id, System)
import Element exposing (Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Hex
import Html
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Value)
import Random exposing (Generator)
import Task


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



---- STORE


type alias Store =
    { nextId : Int
    , position : Dict Id Position
    , movement : Dict Id Movement
    , obstruction : Dict Id Obstruction
    , control : Dict Id Control
    , player : Dict Id Player
    , rester : Dict Id Rester
    , switcher : Dict Id Switcher
    , spawner : Dict Id Spawner
    , mine : Dict Id Mine
    , trapper : Dict Id Trapper
    , sprite : Dict Id Sprite
    , archer : Dict Id Archer
    , fightable : Dict Id Fightable
    }


removeEntity : Id -> System Store -> System Store
removeEntity id system =
    system
        |> Ecs.removeComponent withPosition id
        |> Ecs.removeComponent withMovement id
        |> Ecs.removeComponent withObstruction id
        |> Ecs.removeComponent withControl id
        |> Ecs.removeComponent withPlayer id
        |> Ecs.removeComponent withRester id
        |> Ecs.removeComponent withSwitcher id
        |> Ecs.removeComponent withSpawner id
        |> Ecs.removeComponent withMine id
        |> Ecs.removeComponent withTrapper id
        |> Ecs.removeComponent withSprite id
        |> Ecs.removeComponent withArcher id
        |> Ecs.removeComponent withFightable id


emptySystem =
    { nextId = 0
    , position = Dict.empty
    , player = Dict.empty
    , rester = Dict.empty
    , switcher = Dict.empty
    , spawner = Dict.empty
    , mine = Dict.empty
    , trapper = Dict.empty
    , movement = Dict.empty
    , obstruction = Dict.empty
    , control = Dict.empty
    , sprite = Dict.empty
    , archer = Dict.empty
    , fightable = Dict.empty
    }



-- COMPONENTS


type alias Position =
    { x : Int
    , y : Int
    }


positionFromTuple : ( Int, Int ) -> Position
positionFromTuple ( x, y ) =
    { x = x, y = y }


normalizePosition : Int -> Int -> Position -> Position
normalizePosition width height position =
    { position
        | x = clamp 0 (width - 1) position.x
        , y = clamp 0 (height - 1) position.y
    }


getPosition : Focus Store component -> System Store -> Maybe Position
getPosition focus =
    Ecs.with2 focus withPosition
        >> List.head
        >> Maybe.map (Tuple.second >> Tuple.second)


getPositions : Focus Store component -> System Store -> List Position
getPositions focus =
    Ecs.with2 focus withPosition
        >> List.map (Tuple.second >> Tuple.second)


type alias Movement =
    { strategy : MovementStrategy }


type MovementStrategy
    = Random
    | MoveToPlayer


type Obstruction
    = Obstruction


getObstructedPositions : Id -> System Store -> List Position
getObstructedPositions id system =
    Ecs.with2 withObstruction withPosition system
        |> List.filterMap
            (\( otherId, ( _, position ) ) ->
                if otherId /= id then
                    Just position
                else
                    Nothing
            )


type Control
    = SwitcherControl
    | MineControl
    | ChessRookControl
    | ArcherControl


rasterizeControl : Int -> Int -> Control -> Position -> List Position
rasterizeControl width height control position =
    List.map (\{ x, y } -> { x = position.x + x, y = position.y + y }) <|
        case control of
            SwitcherControl ->
                [ { x = -1, y = 0 }
                , { x = -1, y = 1 }
                , { x = 0, y = 1 }
                , { x = 1, y = 1 }
                , { x = 1, y = 0 }
                , { x = 1, y = -1 }
                , { x = 0, y = -1 }
                , { x = -1, y = -1 }
                , { x = 0, y = 0 }
                ]

            MineControl ->
                [ { x = 0, y = 0 } ]

            ChessRookControl ->
                List.map (\x -> { x = x, y = 0 }) (List.range (1 - width) (width - 1))
                    ++ List.map (\y -> { x = 0, y = y }) (List.range (1 - height) (height - 1))

            ArcherControl ->
                [ Position -1 -1
                , Position -1 1
                , Position 1 -1
                , Position 1 1
                ]


type Player
    = Player


type Rester
    = Rester


type alias Switcher =
    { left : Int
    }


type Spawner
    = Spawner
        { interval : Int
        , left : Int
        , spawn : Position -> System Store -> System Store
        }


type Mine
    = Mine


type alias Trapper =
    { left : Int
    , interval : Int
    }


type Sprite
    = SwitcherSprite
    | TrapperSprite
    | ChessRookSprite
    | PlayerSprite
    | ArcherSprite


type Fightable
    = Fightable


type Archer
    = Archer



---- ENTITIES


spawnPlayer : Position -> System Store -> System Store
spawnPlayer position system =
    Ecs.spawnEntity
        (\newId ->
            Ecs.setComponent withPlayer newId Player
                >> Ecs.setComponent withPosition newId position
                >> Ecs.setComponent withSprite newId PlayerSprite
        )
        system



-- ENEMIES


spawnSwitcher : Position -> Switcher -> System Store -> System Store
spawnSwitcher position switcher =
    Ecs.spawnEntity
        (\newId ->
            identity
                >> Ecs.setComponent withSwitcher newId switcher
                >> Ecs.setComponent withPosition newId position
                >> Ecs.setComponent withObstruction newId Obstruction
                >> Ecs.setComponent withMovement newId { strategy = MoveToPlayer }
                >> Ecs.setComponent withControl newId SwitcherControl
                >> Ecs.setComponent withSprite newId SwitcherSprite
                >> Ecs.setComponent withFightable newId Fightable
        )


spawnChessRook : Position -> System Store -> System Store
spawnChessRook position =
    Ecs.spawnEntity
        (\newId ->
            identity
                >> Ecs.setComponent withPosition newId position
                >> Ecs.setComponent withObstruction newId Obstruction
                >> Ecs.setComponent withMovement newId { strategy = MoveToPlayer }
                >> Ecs.setComponent withControl newId ChessRookControl
                >> Ecs.setComponent withSprite newId ChessRookSprite
                >> Ecs.setComponent withFightable newId Fightable
        )


spawnTrapper : Position -> Trapper -> System Store -> System Store
spawnTrapper position trapper =
    Ecs.spawnEntity
        (\newId ->
            identity
                >> Ecs.setComponent withTrapper newId trapper
                >> Ecs.setComponent withPosition newId position
                >> Ecs.setComponent withObstruction newId Obstruction
                >> Ecs.setComponent withMovement newId { strategy = Random }
                >> Ecs.setComponent withSprite newId TrapperSprite
                >> Ecs.setComponent withFightable newId Fightable
        )


spawnMine : Position -> System Store -> System Store
spawnMine position =
    Ecs.spawnEntity
        (\newId ->
            identity
                >> Ecs.setComponent withMine newId Mine
                >> Ecs.setComponent withPosition newId position
                >> Ecs.setComponent withObstruction newId Obstruction
                >> Ecs.setComponent withControl newId MineControl
        )


spawnSpawner : Position -> Spawner -> System Store -> System Store
spawnSpawner position spawner =
    Ecs.spawnEntity
        (\newId ->
            identity
                >> Ecs.setComponent withSpawner newId spawner
                >> Ecs.setComponent withPosition newId position
        )


spawnRester : Int -> Int -> System Store -> System Store
spawnRester x y =
    Ecs.spawnEntity
        (\newId ->
            identity
                >> Ecs.setComponent withRester newId Rester
                >> Ecs.setComponent withPosition newId (Position x y)
                >> Ecs.setComponent withObstruction newId Obstruction
                >> Ecs.setComponent withSprite newId SwitcherSprite
                >> Ecs.setComponent withFightable newId Fightable
        )



-- OTHER


spawnWall : Int -> Int -> System Store -> System Store
spawnWall x y =
    Ecs.spawnEntity
        (\newId ->
            identity
                >> Ecs.setComponent withPosition newId (Position x y)
                >> Ecs.setComponent withObstruction newId Obstruction
                >> Ecs.setComponent withControl newId MineControl
        )



---- MODEL


type alias Model =
    { gameModel : GameModel
    }


init : { inputs : List String } -> ( Model, Cmd Msg )
init flags =
    let
        inputs =
            flags.inputs
                |> List.map
                    (\input ->
                        case input of
                            "up" ->
                                ArrowUpPressed

                            "down" ->
                                ArrowDownPressed

                            "left" ->
                                ArrowLeftPressed

                            _ ->
                                ArrowRightPressed
                    )

        ( gameModel, gameCmd ) =
            gameInit (l3 0) inputs
    in
    ( { gameModel = gameModel }, Cmd.map GameMsg gameCmd )



---- UPDATE


type Msg
    = GameMsg GameMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameMsg gameMsg ->
            let
                ( newGameModel, gameCmd ) =
                    gameUpdate gameMsg model.gameModel
            in
            ( { model | gameModel = newGameModel }
            , Cmd.map GameMsg gameCmd
            )



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GameMsg (gameSubscriptions model.gameModel)
        ]



---- GAME


type GameModel
    = Playing
        { level : Level
        , inputs : List GameMsg
        }


gameInit : Level -> List GameMsg -> ( GameModel, Cmd GameMsg )
gameInit level inputs =
    let
        queue input =
            Task.perform identity (Task.succeed input)
    in
    ( Playing { level = level, inputs = List.drop 1 inputs }
    , List.head inputs
        |> Maybe.map queue
        |> Maybe.withDefault (notifyFail ())
    )



---- LEVELS


type alias Level =
    { name : String
    , width : Int
    , height : Int
    , system : System Store
    , info : String
    , isFinished : System Store -> Finished
    }


type Finished
    = Running
    | Lost Int
    | Won Int


allLevels : List Level
allLevels =
    [ l0 5
    , l1 10
    , l2 15
    , l3 20
    , l4 25
    , l5 30
    ]


l0 score =
    let
        ( width, height ) =
            ( 5, 5 )
    in
    { name = "#0"
    , width = width
    , height = height
    , system =
        emptySystem
            |> spawnPlayer (Position 4 4)
            |> spawnRester 1 1
    , info = "Use the Arrow Keys!"
    , isFinished =
        \system ->
            if List.isEmpty (Ecs.with withObstruction system) then
                Won score
            else
                Running
    }


l1 score =
    let
        ( width, height ) =
            ( 7, 7 )
    in
    { name = "#1"
    , width = width
    , height = height
    , system =
        emptySystem
            |> spawnPlayer (Position 1 5)
            |> spawnRester 1 1
            |> spawnRester 5 5
            |> spawnWall 0 3
            |> spawnWall 2 3
            |> spawnWall 3 3
            |> spawnWall 4 3
            |> spawnWall 5 3
            |> spawnWall 6 3
    , info = "Collect everything to proceed!"
    , isFinished =
        \system ->
            if List.isEmpty (Ecs.with withRester system) then
                Won score
            else
                Running
    }


l2 score =
    let
        ( width, height ) =
            ( 5, 5 )
    in
    { name = "#2"
    , width = width
    , height = height
    , system =
        emptySystem
            |> spawnPlayer (Position 1 1)
            |> spawnArcher (Position 4 4)
    , info = "Use the Arrow Keys!"
    , isFinished =
        \system ->
            if List.isEmpty (Ecs.with withObstruction system) then
                Won score
            else
                Running
    }


l3 score =
    let
        ( width, height ) =
            ( 5, 5 )
    in
    { name = "#3"
    , width = width
    , height = height
    , system =
        emptySystem
            |> spawnPlayer (Position 4 0)
            |> spawnTrapper { x = 1, y = 3 } { interval = 2, left = 1 }
    , info = "Don't get trapped!"
    , isFinished =
        \system ->
            let
                trapperGone =
                    List.isEmpty (Ecs.having withTrapper system)

                trapperTrapped =
                    system
                        |> getPosition withTrapper
                        |> Maybe.map
                            (\position ->
                                List.isEmpty
                                    (neighbours width height obstacles ( position.x, position.y ))
                            )
                        |> Maybe.withDefault False

                obstacles =
                    getObstructedPositions -1 system
            in
            if trapperGone then
                Won score
            else if trapperTrapped then
                Lost 0
            else
                Running
    }


l4 score =
    let
        ( width, height ) =
            ( 9, 9 )
    in
    { name = "#4"
    , width = width
    , height = height
    , system =
        emptySystem
            |> spawnPlayer (Position 4 4)
            |> spawnSwitcher { x = 1, y = 1 } { left = 2 }
    , info = "Watch out!"
    , isFinished = isFinished width height score
    }


l5 score =
    let
        ( width, height ) =
            ( 9, 9 )
    in
    { name = "#5"
    , width = width
    , height = height
    , system =
        emptySystem
            |> spawnPlayer (Position 7 4)
            |> spawnSwitcher { x = 1, y = 1 } { left = 2 }
            |> spawnSwitcher { x = 2, y = 7 } { left = 5 }
            |> spawnWall 4 2
            |> spawnWall 4 3
            |> spawnWall 4 4
            |> spawnWall 4 5
            |> spawnWall 4 6
    , info = "Watch out!"
    , isFinished = isFinished width height score
    }


spawnArcher : Position -> System Store -> System Store
spawnArcher position =
    Ecs.spawnEntity
        (\newId ->
            identity
                >> Ecs.setComponent withArcher newId Archer
                >> Ecs.setComponent withPosition newId position
                >> Ecs.setComponent withObstruction newId Obstruction
                >> Ecs.setComponent withControl newId ArcherControl
                >> Ecs.setComponent withSprite newId ArcherSprite
                >> Ecs.setComponent withFightable newId Fightable
        )


isFinished : Int -> Int -> Int -> System Store -> Finished
isFinished width height score system =
    let
        allEnemiesGone =
            List.isEmpty (Ecs.having withSwitcher system)
                && List.isEmpty (Ecs.having withTrapper system)

        playerSurrounded =
            Ecs.with2 withPlayer withPosition system
                |> List.head
                |> Maybe.andThen
                    (\( _, ( _, { x, y } ) ) ->
                        if
                            [ ( x + 1, y )
                            , ( x - 1, y )
                            , ( x, y + 1 )
                            , ( x, y - 1 )
                            ]
                                |> List.filter inBounds
                                |> List.all
                                    (\( neighbourX, neighbourY ) ->
                                        controlledBySwitcher
                                            width
                                            height
                                            { x = neighbourX, y = neighbourY }
                                            system
                                    )
                        then
                            Just True
                        else
                            Just False
                    )
                |> Maybe.withDefault False

        inBounds ( neighbourX, neighbourY ) =
            (neighbourX >= 0)
                && (neighbourX < width)
                && (neighbourY >= 0)
                && (neighbourY < height)
    in
    if allEnemiesGone then
        Won score
    else if playerSurrounded then
        Lost 0
    else
        Running



---- UPDATE


type GameMsg
    = ArrowUpPressed
    | ArrowDownPressed
    | ArrowLeftPressed
    | ArrowRightPressed


port notifyWon : () -> Cmd msg


port notifyLost : () -> Cmd msg


port notifyFail : () -> Cmd msg


randomInput : Generator GameMsg
randomInput =
    Random.uniform ArrowUpPressed
        [ ArrowDownPressed
        , ArrowLeftPressed
        , ArrowRightPressed
        ]


gameUpdate : GameMsg -> GameModel -> ( GameModel, Cmd GameMsg )
gameUpdate msg model =
    case model of
        Playing { level, inputs } ->
            let
                handleArrowPressed deltaX deltaY =
                    let
                        queue input =
                            Task.perform identity (Task.succeed input)
                    in
                    case
                        step level.width
                            level.height
                            deltaX
                            deltaY
                            level.system
                    of
                        Just newSystem ->
                            let
                                newLevel =
                                    { level | system = newSystem }
                            in
                            ( Playing
                                { level = newLevel
                                , inputs = List.drop 1 inputs
                                }
                            , case level.isFinished newSystem of
                                Running ->
                                    List.head inputs
                                        |> Maybe.map queue
                                        |> Maybe.withDefault (notifyFail ())

                                Lost levelScore ->
                                    notifyLost ()

                                Won levelScore ->
                                    notifyWon ()
                            )

                        Nothing ->
                            ( Playing
                                { level = level
                                , inputs = List.drop 1 inputs
                                }
                            , List.head inputs
                                |> Maybe.map queue
                                |> Maybe.withDefault (notifyFail ())
                            )
            in
            case msg of
                ArrowUpPressed ->
                    handleArrowPressed 0 -1

                ArrowDownPressed ->
                    handleArrowPressed 0 1

                ArrowLeftPressed ->
                    handleArrowPressed -1 0

                ArrowRightPressed ->
                    handleArrowPressed 1 0



---- SUBSCRIPTIONS


gameSubscriptions : GameModel -> Sub GameMsg
gameSubscriptions model =
    Sub.none



--- STEP


debugStep : Bool
debugStep =
    False


step : Int -> Int -> Int -> Int -> System Store -> Maybe (System Store)
step width height deltaX deltaY system =
    system
        |> movePlayer width height deltaX deltaY
        |> Maybe.map
            (fight
                >> moveEntities width height
                >> stepTrapper
                >> stepEnemies
                >> stepSpawners
                >> stepArchers width height
            )



-- MOVE PLAYER


movePlayer : Int -> Int -> Int -> Int -> System Store -> Maybe (System Store)
movePlayer width height deltaX deltaY system =
    let
        enemies =
            Ecs.with2 withPosition withSwitcher system
                |> List.map Tuple.second

        move ( playerId, ( _, playerPosition ) ) =
            let
                newPlayerPosition =
                    { playerPosition
                        | x = playerPosition.x + deltaX
                        , y = playerPosition.y + deltaY
                    }

                normalizedPlayerPosition =
                    normalizePosition width height newPlayerPosition

                playerActuallyMoved =
                    newPlayerPosition == normalizedPlayerPosition
            in
            if controlledBySwitcher width height newPlayerPosition system then
                Nothing
            else if not playerActuallyMoved then
                Nothing
            else
                Just <|
                    Ecs.setComponent withPosition playerId normalizedPlayerPosition system
    in
    Ecs.with2 withPlayer withPosition system
        |> List.head
        |> Maybe.andThen move



-- COMPONENT FIGHTABLE


fight : System Store -> System Store
fight system =
    Ecs.with2 withFightable withPosition system
        |> List.foldl fightEntity system


fightEntity : ( Id, ( Fightable, Position ) ) -> System Store -> System Store
fightEntity ( id, ( _, position ) ) system =
    let
        removeHitEntity playerPosition =
            if position == playerPosition then
                removeEntity id system
            else
                system
    in
    system
        |> getPosition withPlayer
        |> Maybe.map removeHitEntity
        |> Maybe.withDefault system



-- MOVE ENTITIES


moveEntities : Int -> Int -> System Store -> System Store
moveEntities width height system =
    List.foldl (moveEntity width height)
        system
        (Ecs.with2 withMovement withPosition system)


moveEntity : Int -> Int -> ( Id, ( Movement, Position ) ) -> System Store -> System Store
moveEntity width height ( id, ( movement, position ) ) system =
    let
        obstacles =
            getObstructedPositions id system

        move =
            case movement.strategy of
                Random ->
                    random width height obstacles position

                MoveToPlayer ->
                    walkTo width height obstacles position
    in
    system
        |> getPosition withPlayer
        |> Maybe.map move
        |> Maybe.map
            (\newPosition ->
                Ecs.setComponent withPosition id newPosition system
            )
        |> Maybe.withDefault system



-- STEP ARCHER ENTITIES


stepArchers : Int -> Int -> System Store -> System Store
stepArchers width height system =
    List.foldl (moveArcher width height)
        system
        (Ecs.with2 withArcher withPosition system)


moveArcher : Int -> Int -> ( Id, ( Archer, Position ) ) -> System Store -> System Store
moveArcher width height ( id, ( archer, position ) ) system =
    let
        obstacles =
            getObstructedPositions id system

        move playerPosition =
            let
                distanceToPlayer =
                    min (abs (position.x - playerPosition.x))
                        (abs (position.y - playerPosition.y))
            in
            if distanceToPlayer <= 1 then
                random width height obstacles position playerPosition
            else
                walkTo width height obstacles position playerPosition
    in
    system
        |> getPosition withPlayer
        |> Maybe.map move
        |> Maybe.map
            (\newPosition ->
                Ecs.setComponent withPosition id newPosition system
            )
        |> Maybe.withDefault system


random :
    Int
    -> Int
    -> List Position
    -> Position
    -> Position
    -> Position
random width height obstacles position playerPosition =
    let
        newPosition =
            normalizePosition width height <|
                { x = newX
                , y = newY
                }

        ( newX, newY ) =
            case neighbours width height obstacles ( position.x, position.y ) of
                [] ->
                    ( position.x, position.y )

                first :: rest ->
                    let
                        distribution =
                            1 / (1 + toFloat (List.length rest))
                    in
                    Random.step
                        (Random.weighted
                            ( distribution, first )
                            (List.map (Tuple.pair distribution) rest)
                        )
                        (Random.initialSeed
                            (width
                                * playerPosition.x
                                + playerPosition.y
                            )
                        )
                        |> Tuple.first
    in
    if newPosition == playerPosition then
        position
    else
        newPosition


walkTo :
    Int
    -> Int
    -> List Position
    -> Position
    -> Position
    -> Position
walkTo width height obstacles position playerPosition =
    let
        maybeNewPosition =
            aStarCompute width height obstacles position playerPosition
                |> Maybe.andThen (List.drop 1 >> List.head)
    in
    case maybeNewPosition of
        Nothing ->
            position

        Just newPosition ->
            if List.member newPosition obstacles || newPosition == playerPosition then
                position
            else
                newPosition


controlledBySwitcher : Int -> Int -> Position -> System Store -> Bool
controlledBySwitcher width height position system =
    let
        controlledPositions =
            Ecs.with2 withControl withPosition system
                |> List.concatMap
                    (\( id, ( control, controlPosition ) ) ->
                        rasterizeControl width height control controlPosition
                    )
    in
    List.member position controlledPositions


aStarConfig : Int -> Int -> List Position -> AStar.Config ( Int, Int )
aStarConfig width height obstacles =
    { heuristicCostEstimate =
        \goal current ->
            toFloat <|
                ((Tuple.first goal - Tuple.first current) ^ 2)
                    + ((Tuple.second goal - Tuple.second current) ^ 2)
    , neighbours = neighbours width height obstacles
    }


aStarCompute : Int -> Int -> List Position -> Position -> Position -> Maybe (List Position)
aStarCompute width height obstacles startPosition goalPosition =
    AStar.compute (aStarConfig width height obstacles)
        ( startPosition.x, startPosition.y )
        ( goalPosition.x, goalPosition.y )
        |> Maybe.map (List.map positionFromTuple)


neighbours : Int -> Int -> List Position -> ( Int, Int ) -> List ( Int, Int )
neighbours width height obstacles ( x, y ) =
    let
        inBounds ( neighbourX, neighbourY ) =
            (neighbourX >= 0)
                && (neighbourX < width)
                && (neighbourY >= 0)
                && (neighbourY < height)

        isFree neighbour =
            not (List.member (positionFromTuple neighbour) obstacles)
    in
    List.filter isFree <|
        List.filter inBounds <|
            [ ( x, y + 1 )
            , ( x - 1, y )
            , ( x + 1, y )
            , ( x, y - 1 )
            ]



-- STEP TRAPPER POSITION


stepTrapper : System Store -> System Store
stepTrapper system =
    List.foldl stepTrapperHelp system (Ecs.with2 withTrapper withPosition system)


stepTrapperHelp : ( Id, ( Trapper, Position ) ) -> System Store -> System Store
stepTrapperHelp ( id, ( trapper, trapperPosition ) ) system =
    if trapper.left > 0 then
        Ecs.setComponent withTrapper
            id
            { trapper | left = trapper.left - 1 }
            system
    else
        system
            |> Ecs.setComponent withTrapper id { trapper | left = trapper.interval }
            |> spawnMine trapperPosition



-- STEP ENEMIES


stepEnemies : System Store -> System Store
stepEnemies system =
    List.foldl stepSwitcher system (Ecs.with withSwitcher system)


stepSwitcher : ( Id, Switcher ) -> System Store -> System Store
stepSwitcher ( id, switcher ) system =
    let
        hasControl =
            Ecs.getComponent withControl id system /= Nothing
    in
    if switcher.left > 0 then
        Ecs.setComponent withSwitcher id { switcher | left = switcher.left - 1 } system
    else
        system
            |> Ecs.setComponent withSwitcher
                id
                { switcher
                    | left =
                        if hasControl then
                            2
                        else
                            4
                }
            |> (if hasControl then
                    Ecs.removeComponent withControl id
                else
                    Ecs.setComponent withControl id SwitcherControl
               )
            |> (if hasControl then
                    Ecs.setComponent withMovement id { strategy = Random }
                else
                    Ecs.setComponent withMovement id { strategy = MoveToPlayer }
               )



-- STEP SPAWNERS


stepSpawners : System Store -> System Store
stepSpawners system =
    List.foldl stepSpawner system (Ecs.with2 withSpawner withPosition system)


stepSpawner : ( Id, ( Spawner, Position ) ) -> System Store -> System Store
stepSpawner ( id, ( Spawner spawner, spawnerPosition ) ) =
    if spawner.left > 0 then
        Ecs.setComponent withSpawner id (Spawner { spawner | left = spawner.left - 1 })
    else
        Ecs.setComponent withSpawner id (Spawner { spawner | left = spawner.interval })
            >> spawner.spawn spawnerPosition



---- FOCI


withPosition : Focus Store Position
withPosition =
    Ecs.focus
        { get = .position
        , set = \value store -> { store | position = value }
        }


withMovement : Focus Store Movement
withMovement =
    Ecs.focus
        { get = .movement
        , set = \value store -> { store | movement = value }
        }


withObstruction : Focus Store Obstruction
withObstruction =
    Ecs.focus
        { get = .obstruction
        , set = \value store -> { store | obstruction = value }
        }


withControl : Focus Store Control
withControl =
    Ecs.focus
        { get = .control
        , set = \value store -> { store | control = value }
        }


withPlayer : Focus Store Player
withPlayer =
    Ecs.focus
        { get = .player
        , set = \value store -> { store | player = value }
        }


withRester : Focus Store Rester
withRester =
    Ecs.focus
        { get = .rester
        , set = \value store -> { store | rester = value }
        }


withSwitcher : Focus Store Switcher
withSwitcher =
    Ecs.focus
        { get = .switcher
        , set = \value store -> { store | switcher = value }
        }


withSpawner : Focus Store Spawner
withSpawner =
    Ecs.focus
        { get = .spawner
        , set = \value store -> { store | spawner = value }
        }


withMine : Focus Store Mine
withMine =
    Ecs.focus
        { get = .mine
        , set = \value store -> { store | mine = value }
        }


withTrapper : Focus Store Trapper
withTrapper =
    Ecs.focus
        { get = .trapper
        , set = \value store -> { store | trapper = value }
        }


withSprite : Focus Store Sprite
withSprite =
    Ecs.focus
        { get = .sprite
        , set = \value store -> { store | sprite = value }
        }


withArcher : Focus Store Archer
withArcher =
    Ecs.focus
        { get = .archer
        , set = \value store -> { store | archer = value }
        }


withFightable : Focus Store Fightable
withFightable =
    Ecs.focus
        { get = .fightable
        , set = \value store -> { store | fightable = value }
        }

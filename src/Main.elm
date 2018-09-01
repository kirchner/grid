module Main exposing (main)

import AStar
import Browser exposing (Document)
import Browser.Events as Events
import Dict exposing (Dict)
import Ecs.System as Ecs exposing (Focus, Id, System)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Json.Decode as Decode


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- CONSTANTS


width =
    32


height =
    16



---- STORE


type alias Store =
    { nextId : Int
    , position : Dict Id Position
    , player : Dict Id Player
    , enemy : Dict Id Enemy
    , spawner : Dict Id Spawner
    , mine : Dict Id Mine
    , trapper : Dict Id Trapper
    }


type alias Position =
    { x : Int
    , y : Int
    }


withPosition : Focus Store Position
withPosition =
    Ecs.focus
        { get = .position
        , set = \value store -> { store | position = value }
        }


type Player
    = Player


withPlayer : Focus Store Player
withPlayer =
    Ecs.focus
        { get = .player
        , set = \value store -> { store | player = value }
        }


type alias Enemy =
    { left : Int
    , side : Side
    }


type Side
    = Red
    | Green


withEnemy : Focus Store Enemy
withEnemy =
    Ecs.focus
        { get = .enemy
        , set = \value store -> { store | enemy = value }
        }


type Spawner
    = Spawner
        { interval : Int
        , left : Int
        , spawn : Position -> System Store -> System Store
        }


withSpawner : Focus Store Spawner
withSpawner =
    Ecs.focus
        { get = .spawner
        , set = \value store -> { store | spawner = value }
        }


type Mine
    = Mine


withMine : Focus Store Mine
withMine =
    Ecs.focus
        { get = .mine
        , set = \value store -> { store | mine = value }
        }


type alias Trapper =
    { left : Int
    }


withTrapper : Focus Store Trapper
withTrapper =
    Ecs.focus
        { get = .trapper
        , set = \value store -> { store | trapper = value }
        }



---- MODEL


type alias Model =
    { leftUntilSpawn : Int
    , corner : Int
    , system : Ecs.System Store
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { leftUntilSpawn = 8
      , corner = 3
      , system =
            { nextId = 0
            , position = Dict.empty
            , player = Dict.empty
            , enemy = Dict.empty
            , spawner = Dict.empty
            , mine = Dict.empty
            , trapper = Dict.empty
            }
                |> Ecs.spawnEntity
                    (\newId ->
                        Ecs.setComponent withPlayer newId Player
                            >> Ecs.setComponent withPosition newId { x = 10, y = 6 }
                    )
                |> spawnSpawner (spawnEnemy 3 Red) 0 0 32 2
                |> spawnSpawner (spawnEnemy 1 Red) (width - 1) (height - 1) 32 6
                |> spawnSpawner (spawnTrapper 8) 0 (height - 1) 32 10
                |> spawnSpawner (spawnTrapper 8) (width - 1) 0 32 14
      }
    , Cmd.none
    )


spawnSpawner :
    (Position -> System Store -> System Store)
    -> Int
    -> Int
    -> Int
    -> Int
    -> System Store
    -> System Store
spawnSpawner spawn x y interval left =
    let
        newSpawner =
            Spawner
                { interval = interval
                , left = left
                , spawn = spawn
                }
    in
    Ecs.spawnEntity
        (\newId ->
            Ecs.setComponent withSpawner newId newSpawner
                >> Ecs.setComponent withPosition newId { x = x, y = y }
        )


spawnEnemy : Int -> Side -> Position -> System Store -> System Store
spawnEnemy left side position =
    Ecs.spawnEntity
        (\newId ->
            Ecs.setComponent withEnemy newId { left = left, side = side }
                >> Ecs.setComponent withPosition newId position
        )


spawnTrapper : Int -> Position -> System Store -> System Store
spawnTrapper left position =
    Ecs.spawnEntity
        (\newId ->
            Ecs.setComponent withTrapper newId { left = left }
                >> Ecs.setComponent withPosition newId position
        )


spawnMine : Position -> System Store -> System Store
spawnMine { x, y } =
    Ecs.spawnEntity
        (\newId ->
            Ecs.setComponent withMine newId Mine
                >> Ecs.setComponent withPosition newId { x = x, y = y }
        )



---- VIEW


view : Model -> Document Msg
view model =
    { title = "grid"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            (viewGrid model.system)
        ]
    }


viewGrid : System Store -> Element Msg
viewGrid system =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (List.range 0 (height - 1)
            |> List.map (viewRow system)
        )


viewRow : System Store -> Int -> Element Msg
viewRow system rowIndex =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (List.range 0 (width - 1)
            |> List.map (viewField system rowIndex)
        )


viewField : System Store -> Int -> Int -> Element Msg
viewField system rowIndex columnIndex =
    let
        redTiles =
            List.concat
                [ redEnemyTiles
                , mineTiles
                ]

        enemies =
            Ecs.with2 withEnemy withPosition system

        greenEnemyTiles =
            enemies
                |> List.filterMap
                    (\( _, ( { side }, position ) ) ->
                        case side of
                            Red ->
                                Just position

                            Green ->
                                Nothing
                    )

        redEnemyTiles =
            enemies
                |> List.filterMap
                    (\( _, ( { side }, position ) ) ->
                        case side of
                            Red ->
                                Just position

                            Green ->
                                Nothing
                    )

        lightRedTiles =
            redEnemyTiles
                |> List.concatMap
                    (\{ x, y } ->
                        [ { x = x - 1, y = y }
                        , { x = x - 1, y = y + 1 }
                        , { x = x, y = y + 1 }
                        , { x = x + 1, y = y + 1 }
                        , { x = x + 1, y = y }
                        , { x = x + 1, y = y - 1 }
                        , { x = x, y = y - 1 }
                        , { x = x - 1, y = y - 1 }
                        ]
                    )

        greenTiles =
            List.concat
                [ Ecs.with2 withEnemy withPosition system
                    |> List.filterMap
                        (\( _, ( { side }, position ) ) ->
                            case side of
                                Red ->
                                    Nothing

                                Green ->
                                    Just position
                        )
                , playerTiles
                , trapperTiles
                , greenEnemyTiles
                ]

        playerTiles =
            Ecs.with2 withPlayer withPosition system
                |> List.map (Tuple.second >> Tuple.second)

        trapperTiles =
            Ecs.with2 withTrapper withPosition system
                |> List.map (Tuple.second >> Tuple.second)

        mineTiles =
            Ecs.with2 withMine withPosition system
                |> List.map (Tuple.second >> Tuple.second)

        backgroundColor =
            let
                tilePosition =
                    { x = columnIndex, y = rowIndex }
            in
            if List.member tilePosition redTiles then
                Element.rgb 1 0 0
            else if List.member tilePosition greenTiles then
                Element.rgb 0 1 0
            else if List.member tilePosition lightRedTiles then
                Element.rgb 1 0.5 0.5
            else
                Element.rgb 1 1 1
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Border.width 1
        , Border.color (Element.rgb 0 0 0)
        , Background.color backgroundColor
        ]
        Element.none



---- UPDATE


type Msg
    = ArrowUpPressed
    | ArrowDownPressed
    | ArrowLeftPressed
    | ArrowRightPressed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        handleArrowPressed deltaX deltaY =
            ( { model | system = step deltaX deltaY model.system }
            , Cmd.none
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



--- STEP


step : Int -> Int -> System Store -> System Store
step deltaX deltaY system =
    system
        |> movePlayer deltaX deltaY
        |> Maybe.map
            (fight
                >> moveEnemies
                >> moveTrappers
                >> switchEnemiesTeam
                >> stepSpawners
            )
        |> Maybe.withDefault system



-- STEP PLAYER MOVE


movePlayer : Int -> Int -> System Store -> Maybe (System Store)
movePlayer deltaX deltaY system =
    let
        enemies =
            Ecs.with2 withPosition withEnemy system
                |> List.map Tuple.second
    in
    Ecs.with2 withPlayer withPosition system
        |> List.head
        |> Maybe.andThen
            (\( playerId, ( _, playerPosition ) ) ->
                let
                    newPlayerPosition =
                        { playerPosition
                            | x = playerPosition.x + deltaX
                            , y = playerPosition.y + deltaY
                        }
                in
                if List.any (controlledByEnemy newPlayerPosition.x newPlayerPosition.y) enemies then
                    Nothing
                else if newPlayerPosition /= normalizePosition newPlayerPosition then
                    Nothing
                else
                    Just <|
                        Ecs.setComponent withPosition
                            playerId
                            (normalizePosition newPlayerPosition)
                            system
            )


normalizePosition : Position -> Position
normalizePosition position =
    { position
        | x = clamp 0 (width - 1) position.x
        , y = clamp 0 (height - 1) position.y
    }



-- STEP HIT ENEMIES


fight : System Store -> System Store
fight system =
    system
        |> fightEnemies
        |> fightTrappers


fightEnemies : System Store -> System Store
fightEnemies system =
    List.foldl fightEnemy system (Ecs.with2 withEnemy withPosition system)


fightEnemy : ( Id, ( Enemy, Position ) ) -> System Store -> System Store
fightEnemy ( id, ( enemy, enemyPosition ) ) system =
    Maybe.map
        (\playerPosition ->
            case enemy.side of
                Red ->
                    system

                Green ->
                    if
                        (playerPosition.x == enemyPosition.x)
                            && (playerPosition.y == enemyPosition.y)
                    then
                        system
                            |> Ecs.removeComponent withPosition id
                            |> Ecs.removeComponent withEnemy id
                    else
                        system
        )
        (Ecs.with2 withPlayer withPosition system
            |> List.head
            |> Maybe.map (Tuple.second >> Tuple.second)
        )
        |> Maybe.withDefault system


fightTrappers : System Store -> System Store
fightTrappers system =
    List.foldl fightTrapper system (Ecs.with2 withTrapper withPosition system)


fightTrapper : ( Id, ( Trapper, Position ) ) -> System Store -> System Store
fightTrapper ( id, ( trapper, trapperPosition ) ) system =
    Maybe.map
        (\playerPosition ->
            if
                (playerPosition.x == trapperPosition.x)
                    && (playerPosition.y == trapperPosition.y)
            then
                system
                    |> Ecs.removeComponent withPosition id
                    |> Ecs.removeComponent withTrapper id
            else
                system
        )
        (Ecs.with2 withPlayer withPosition system
            |> List.head
            |> Maybe.map (Tuple.second >> Tuple.second)
        )
        |> Maybe.withDefault system



-- STEP ENEMIES POSITION


moveEnemies : System Store -> System Store
moveEnemies system =
    List.foldl moveEnemy system (Ecs.with2 withEnemy withPosition system)


moveEnemy : ( Id, ( Enemy, Position ) ) -> System Store -> System Store
moveEnemy ( id, ( enemy, enemyPosition ) ) system =
    let
        obstacles =
            getObstacles id system

        playerPosition =
            Ecs.with2 withPlayer withPosition system
                |> List.head
                |> Maybe.map (Tuple.second >> Tuple.second)
    in
    Maybe.map (moveEnemyHelp obstacles enemyPosition)
        playerPosition
        |> Maybe.map
            (\newPosition ->
                Ecs.setComponent withPosition id newPosition system
            )
        |> Maybe.withDefault system


moveEnemyHelp :
    List Position
    -> Position
    -> Position
    -> Position
moveEnemyHelp obstacles enemyPosition playerPosition =
    let
        deltaX =
            playerPosition.x - enemyPosition.x

        deltaY =
            playerPosition.y - enemyPosition.y

        newEnemyPosition =
            if abs deltaX >= abs deltaY then
                if deltaX > 0 then
                    { enemyPosition | x = clamp 0 (width - 1) (enemyPosition.x + 1) }
                else if deltaX < 0 then
                    { enemyPosition | x = clamp 0 (width - 1) (enemyPosition.x - 1) }
                else
                    enemyPosition
            else if deltaY > 0 then
                { enemyPosition | y = clamp 0 (height - 1) (enemyPosition.y + 1) }
            else if deltaY < 0 then
                { enemyPosition | y = clamp 0 (height - 1) (enemyPosition.y - 1) }
            else
                enemyPosition
    in
    if List.member newEnemyPosition obstacles then
        enemyPosition
    else
        newEnemyPosition


controlledByEnemy : Int -> Int -> ( Position, Enemy ) -> Bool
controlledByEnemy x y ( enemyPosition, enemy ) =
    case enemy.side of
        Red ->
            (enemyPosition.x >= x - 1)
                && (enemyPosition.x <= x + 1)
                && (enemyPosition.y >= y - 1)
                && (enemyPosition.y <= y + 1)

        Green ->
            False



-- STEP TRAPPER POSITION


{-| Obstacles for enemy movement.
-}
getObstacles : Id -> System Store -> List Position
getObstacles id system =
    let
        filter ( otherId, ( _, position ) ) =
            if otherId /= id then
                Just position
            else
                Nothing
    in
    List.concat
        [ Ecs.with2 withEnemy withPosition system
            |> List.filterMap filter
        , Ecs.with2 withTrapper withPosition system
            |> List.filterMap filter
        , Ecs.with2 withMine withPosition system
            |> List.filterMap filter
        , Ecs.with2 withPlayer withPosition system
            |> List.filterMap filter
        ]


moveTrappers : System Store -> System Store
moveTrappers system =
    List.foldl moveTrapper system (Ecs.with2 withTrapper withPosition system)


moveTrapper : ( Id, ( Trapper, Position ) ) -> System Store -> System Store
moveTrapper ( id, ( trapper, trapperPosition ) ) system =
    let
        obstacles =
            getObstacles id system

        playerPosition =
            Ecs.with2 withPlayer withPosition system
                |> List.head
                |> Maybe.map (Tuple.second >> Tuple.second)
    in
    Maybe.map (moveTrapperHelp obstacles trapperPosition)
        playerPosition
        |> Maybe.map
            (\newPosition ->
                system
                    |> Ecs.setComponent withPosition id newPosition
                    |> (if trapper.left > 0 then
                            Ecs.setComponent withTrapper
                                id
                                { trapper | left = trapper.left - 1 }
                        else
                            Ecs.setComponent withTrapper id { trapper | left = 8 }
                                >> spawnMine trapperPosition
                       )
            )
        |> Maybe.withDefault system


moveTrapperHelp :
    List Position
    -> Position
    -> Position
    -> Position
moveTrapperHelp obstacles trapperPosition playerPosition =
    let
        deltaX =
            playerPosition.x - trapperPosition.x

        deltaY =
            playerPosition.y - trapperPosition.y

        newTrapperPosition =
            if abs deltaX >= abs deltaY then
                if deltaX > 0 then
                    { trapperPosition | x = clamp 0 (width - 1) (trapperPosition.x + 1) }
                else if deltaX < 0 then
                    { trapperPosition | x = clamp 0 (width - 1) (trapperPosition.x - 1) }
                else
                    trapperPosition
            else if deltaY > 0 then
                { trapperPosition | y = clamp 0 (height - 1) (trapperPosition.y + 1) }
            else if deltaY < 0 then
                { trapperPosition | y = clamp 0 (height - 1) (trapperPosition.y - 1) }
            else
                trapperPosition
    in
    if List.member newTrapperPosition obstacles then
        trapperPosition
    else
        newTrapperPosition



-- STEP ENEMIES TEAM


switchEnemiesTeam : System Store -> System Store
switchEnemiesTeam system =
    List.foldl switchEnemyTeam system (Ecs.having2 withPosition withEnemy system)


switchEnemyTeam : Id -> System Store -> System Store
switchEnemyTeam id system =
    Ecs.getComponent withEnemy id system
        |> Maybe.map
            (\enemy ->
                let
                    newEnemy =
                        if enemy.left > 0 then
                            { enemy | left = enemy.left - 1 }
                        else
                            { enemy
                                | left =
                                    case enemy.side of
                                        Green ->
                                            8

                                        Red ->
                                            2
                                , side =
                                    case enemy.side of
                                        Green ->
                                            Red

                                        Red ->
                                            Green
                            }
                in
                Ecs.setComponent withEnemy id newEnemy system
            )
        |> Maybe.withDefault system



-- STEP SPAWNING


stepSpawners : System Store -> System Store
stepSpawners system =
    let
        spawners =
            Ecs.having withSpawner system
    in
    List.foldl stepSpawner system spawners


stepSpawner : Id -> System Store -> System Store
stepSpawner id system =
    Maybe.map2
        (\(Spawner spawner) spawnerPosition ->
            if spawner.left > 0 then
                Ecs.setComponent withSpawner
                    id
                    (Spawner { spawner | left = spawner.left - 1 })
                    system
            else
                system
                    |> Ecs.setComponent withSpawner
                        id
                        (Spawner { spawner | left = spawner.interval })
                    |> spawner.spawn spawnerPosition
        )
        (Ecs.getComponent withSpawner id system)
        (Ecs.getComponent withPosition id system)
        |> Maybe.withDefault system



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onKeyPress
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    case key of
                        "ArrowUp" ->
                            Decode.succeed ArrowUpPressed

                        "ArrowDown" ->
                            Decode.succeed ArrowDownPressed

                        "ArrowLeft" ->
                            Decode.succeed ArrowLeftPressed

                        "ArrowRight" ->
                            Decode.succeed ArrowRightPressed

                        _ ->
                            Decode.fail "not handling that key"
                )
        )

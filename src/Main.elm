module Main exposing (main)

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


{-| TODO: Make components not opaque
-}
type Enemy
    = Enemy EnemyData


type alias EnemyData =
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


type alias Spawner =
    { left : Int
    }


withSpawner : Focus Store Spawner
withSpawner =
    Ecs.focus
        { get = .spawner
        , set = \value store -> { store | spawner = value }
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
            { nextId = 4
            , position =
                Dict.fromList
                    [ ( 0, { x = 10, y = 6 } )
                    , ( 1, { x = 0, y = 0 } )
                    , ( 2, { x = 2, y = 5 } )
                    , ( 3, { x = 0, y = 0 } )
                    ]
            , player =
                Dict.fromList
                    [ ( 0, Player )
                    ]
            , enemy =
                Dict.fromList
                    [ ( 1, Enemy { left = 3, side = Red } )
                    , ( 2, Enemy { left = 1, side = Red } )
                    ]
            , spawner =
                Dict.fromList
                    [ ( 3, { left = 8 } )
                    ]
            }
      }
    , Cmd.none
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
        enemies =
            Ecs.with2 withPosition withEnemy system
                |> List.map Tuple.second

        backgroundColor =
            case housesEnemy of
                Nothing ->
                    if housesPlayer then
                        Element.rgb 0 1 0
                    else
                        case nextToEnemy of
                            Nothing ->
                                Element.rgb 1 1 1

                            Just Red ->
                                Element.rgb 1 0.5 0.5

                            Just Green ->
                                Element.rgb 0.5 1 0.5

                Just Red ->
                    Element.rgb 1 0 0

                Just Green ->
                    Element.rgb 0 1 0

        housesPlayer =
            Ecs.with2 withPlayer withPosition system
                |> List.head
                |> Maybe.map
                    (\( _, ( _, playerPosition ) ) ->
                        (rowIndex == playerPosition.y)
                            && (columnIndex == playerPosition.x)
                    )
                |> Maybe.withDefault False

        housesEnemy =
            enemies
                |> List.filterMap
                    (\( enemyPosition, Enemy enemy ) ->
                        if
                            (enemyPosition.x == columnIndex)
                                && (enemyPosition.y == rowIndex)
                        then
                            Just enemy.side
                        else
                            Nothing
                    )
                |> List.head

        nextToEnemy =
            enemies
                |> List.filterMap
                    (\( enemyPosition, (Enemy { side }) as enemy ) ->
                        if controlledByEnemy columnIndex rowIndex ( enemyPosition, enemy ) then
                            Just side
                        else
                            Nothing
                    )
                |> List.head
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
    let
        enemyIds =
            Ecs.having2 withPosition withEnemy system
    in
    system
        |> movePlayer deltaX deltaY
        |> Maybe.map
            (fightEnemies
                >> moveEnemies enemyIds
                >> switchEnemiesTeam enemyIds
                >> spawnEnemies
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


fightEnemies : System Store -> System Store
fightEnemies system =
    let
        enemyIds =
            Ecs.having withEnemy system
    in
    List.foldl fightEnemy system enemyIds


fightEnemy : Id -> System Store -> System Store
fightEnemy id system =
    Maybe.map2
        (\playerPosition ( enemyPosition, Enemy { side } ) ->
            case side of
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
        (Maybe.map2 Tuple.pair
            (Ecs.getComponent withPosition id system)
            (Ecs.getComponent withEnemy id system)
        )
        |> Maybe.withDefault system



-- STEP ENEMIES POSITION


moveEnemies : List Id -> System Store -> System Store
moveEnemies ids system =
    List.foldl moveEnemy system ids


moveEnemy : Id -> System Store -> System Store
moveEnemy id system =
    let
        otherEnemies =
            Ecs.with2 withPosition withEnemy system
                |> List.filter (Tuple.first >> (/=) id)

        enemyPosition =
            Ecs.getComponent withPosition id system

        enemy =
            Ecs.getComponent withEnemy id system

        playerPosition =
            Ecs.with2 withPlayer withPosition system
                |> List.head
                |> Maybe.map (Tuple.second >> Tuple.second)
    in
    Maybe.map3 (moveEnemyHelp otherEnemies)
        playerPosition
        enemyPosition
        enemy
        |> Maybe.map
            (\newPosition ->
                Ecs.setComponent withPosition id newPosition system
            )
        |> Maybe.withDefault system


moveEnemyHelp :
    List ( Id, ( Position, Enemy ) )
    -> Position
    -> Position
    -> Enemy
    -> Position
moveEnemyHelp otherEnemies playerPosition enemyPosition enemy =
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
    if
        List.any (controlledByEnemy newEnemyPosition.x newEnemyPosition.y)
            (List.map Tuple.second otherEnemies)
    then
        enemyPosition
    else if (newEnemyPosition.x == playerPosition.x) && (newEnemyPosition.y == playerPosition.y) then
        enemyPosition
    else
        newEnemyPosition


controlledByEnemy : Int -> Int -> ( Position, Enemy ) -> Bool
controlledByEnemy x y ( enemyPosition, Enemy enemy ) =
    case enemy.side of
        Red ->
            (enemyPosition.x >= x - 1)
                && (enemyPosition.x <= x + 1)
                && (enemyPosition.y >= y - 1)
                && (enemyPosition.y <= y + 1)

        Green ->
            False



-- STEP ENEMIES TEAM


switchEnemiesTeam : List Id -> System Store -> System Store
switchEnemiesTeam ids system =
    List.foldl switchEnemyTeam system ids


switchEnemyTeam : Id -> System Store -> System Store
switchEnemyTeam id system =
    Ecs.getComponent withEnemy id system
        |> Maybe.map
            (\(Enemy enemyData) ->
                let
                    newEnemy =
                        Enemy <|
                            if enemyData.left > 0 then
                                { enemyData | left = enemyData.left - 1 }
                            else
                                { enemyData
                                    | left =
                                        case enemyData.side of
                                            Green ->
                                                8

                                            Red ->
                                                2
                                    , side =
                                        case enemyData.side of
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


spawnEnemies : System Store -> System Store
spawnEnemies system =
    let
        spawners =
            Ecs.having withSpawner system
    in
    List.foldl spawnEnemy system spawners


spawnEnemy : Id -> System Store -> System Store
spawnEnemy id system =
    Maybe.map2
        (\spawner spawnerPosition ->
            if spawner.left > 0 then
                Ecs.setComponent withSpawner
                    id
                    { spawner | left = spawner.left - 1 }
                    system
            else
                system
                    |> Ecs.setComponent withSpawner
                        id
                        { spawner | left = 25 }
                    |> Ecs.spawnEntity
                        (\newId ->
                            Ecs.setComponent withEnemy newId (Enemy { left = 3, side = Red })
                                >> Ecs.setComponent withPosition newId spawnerPosition
                        )
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

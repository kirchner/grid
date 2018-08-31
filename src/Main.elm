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
            Ecs.having2 withPosition withEnemy system
                |> List.filterMap
                    (\id ->
                        Maybe.map2 Tuple.pair
                            (Ecs.getComponent withPosition id system)
                            (Ecs.getComponent withEnemy id system)
                    )

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
            Ecs.having withPlayer system
                |> List.head
                |> Maybe.andThen
                    (\id ->
                        Ecs.getComponent withPosition id system
                    )
                |> Maybe.map
                    (\playerPosition ->
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
        |> stepPlayerMove deltaX deltaY
        |> stepHitEnemies
        |> stepEnemiesPosition enemyIds
        |> stepEnemiesTeam enemyIds
        |> stepSpawners



-- STEP PLAYER MOVE


stepPlayerMove : Int -> Int -> System Store -> System Store
stepPlayerMove deltaX deltaY system =
    let
        enemies =
            Ecs.having2 withPosition withEnemy system
                |> List.filterMap
                    (\otherEnemyId ->
                        Maybe.map2 Tuple.pair
                            (Ecs.getComponent withPosition otherEnemyId system)
                            (Ecs.getComponent withEnemy otherEnemyId system)
                    )
    in
    Ecs.having withPlayer system
        |> List.head
        |> Maybe.andThen
            (\playerId ->
                Ecs.getComponent withPosition playerId system
                    |> Maybe.map
                        (\playerPosition ->
                            let
                                newX =
                                    clamp 0 (width - 1) (playerPosition.x + deltaX)

                                newY =
                                    clamp 0 (height - 1) (playerPosition.y + deltaY)

                                newPlayerPosition =
                                    if List.any (controlledByEnemy newX newY) enemies then
                                        playerPosition
                                    else
                                        { playerPosition | x = newX, y = newY }
                            in
                            Ecs.setComponent withPosition playerId newPlayerPosition system
                        )
            )
        |> Maybe.withDefault system



-- STEP HIT ENEMIES


stepHitEnemies : System Store -> System Store
stepHitEnemies system =
    let
        enemyIds =
            Ecs.having withEnemy system
    in
    List.foldl stepHitEnemy system enemyIds


stepHitEnemy : Id -> System Store -> System Store
stepHitEnemy id system =
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
        (Ecs.having withPlayer system
            |> List.head
            |> Maybe.andThen
                (\playerId ->
                    Ecs.getComponent withPosition playerId system
                )
        )
        (Maybe.map2 Tuple.pair
            (Ecs.getComponent withPosition id system)
            (Ecs.getComponent withEnemy id system)
        )
        |> Maybe.withDefault system



-- STEP ENEMIES POSITION


stepEnemiesPosition : List Id -> System Store -> System Store
stepEnemiesPosition ids system =
    List.foldl stepEnemyPosition system ids


stepEnemyPosition : Id -> System Store -> System Store
stepEnemyPosition id system =
    let
        otherEnemyIds =
            Ecs.having2 withPosition withEnemy system
                |> List.filter ((/=) id)

        otherEnemies =
            otherEnemyIds
                |> List.filterMap
                    (\otherEnemyId ->
                        Maybe.map2 Tuple.pair
                            (Ecs.getComponent withPosition otherEnemyId system)
                            (Ecs.getComponent withEnemy otherEnemyId system)
                    )

        enemyPosition =
            Ecs.getComponent withPosition id system

        enemy =
            Ecs.getComponent withEnemy id system

        playerPosition =
            Ecs.having withPlayer system
                |> List.head
                |> Maybe.andThen
                    (\playerId ->
                        Ecs.getComponent withPosition playerId system
                    )
    in
    Maybe.map3 (stepEnemy otherEnemies)
        playerPosition
        enemyPosition
        enemy
        |> Maybe.map
            (\newPosition ->
                Ecs.setComponent withPosition id newPosition system
            )
        |> Maybe.withDefault system


stepEnemy :
    List ( Position, Enemy )
    -> Position
    -> Position
    -> Enemy
    -> Position
stepEnemy otherEnemies playerPosition enemyPosition enemy =
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
    if List.any (controlledByEnemy newEnemyPosition.x newEnemyPosition.y) otherEnemies then
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


stepEnemiesTeam : List Id -> System Store -> System Store
stepEnemiesTeam ids system =
    List.foldl stepEnemyTeam system ids


stepEnemyTeam : Id -> System Store -> System Store
stepEnemyTeam id system =
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

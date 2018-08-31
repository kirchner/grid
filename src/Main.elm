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
            { nextId = 3
            , position =
                Dict.fromList
                    [ ( 0, { x = 10, y = 6 } )
                    , ( 1, { x = 0, y = 0 } )
                    , ( 2, { x = 2, y = 5 } )
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
    --    let
    --        handleArrowPressed deltaX deltaY =
    --            case movePlayer model.enemies deltaX deltaY model.player of
    --                Nothing ->
    --                    ( model
    --                    , Cmd.none
    --                    )
    --
    --                Just newPlayer ->
    --                    ( { model
    --                        | player =
    --                            newPlayer
    --                        , enemies =
    --                            model.enemies
    --                                -- |> hitEnemies newPlayer
    --                                |> stepEnemiesPosition newPlayer
    --
    --                        -- |> List.map stepEnemyTeam
    --                      }
    --                      -- TODO: |> stepSpawning
    --                    , Cmd.none
    --                    )
    --    in
    --    case msg of
    --        ArrowUpPressed ->
    --            handleArrowPressed 0 -1
    --
    --        ArrowDownPressed ->
    --            handleArrowPressed 0 1
    --
    --        ArrowLeftPressed ->
    --            handleArrowPressed -1 0
    --
    --        ArrowRightPressed ->
    --            handleArrowPressed 1 0
    ( model, Cmd.none )


step : System Store -> System Store
step system =
    let
        enemyIds =
            Ecs.having2 withPosition withEnemy system
    in
    system
        |> stepEnemiesPosition enemyIds


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
                    { enemyPosition | x = enemyPosition.x + 1 }
                else if deltaX < 0 then
                    { enemyPosition | x = enemyPosition.x - 1 }
                else
                    enemyPosition
            else if deltaY > 0 then
                { enemyPosition | y = enemyPosition.y + 1 }
            else if deltaY < 0 then
                { enemyPosition | y = enemyPosition.y - 1 }
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



--stepSpawning : Model -> Model
--stepSpawning model =
--    if model.leftUntilSpawn > 0 then
--        { model | leftUntilSpawn = model.leftUntilSpawn - 1 }
--    else
--        let
--            ( x, y ) =
--                case model.corner of
--                    0 ->
--                        ( 0, 0 )
--
--                    1 ->
--                        ( width - 1, 0 )
--
--                    2 ->
--                        ( width - 1, height - 1 )
--
--                    3 ->
--                        ( 0, height - 1 )
--
--                    _ ->
--                        ( 0, 0 )
--
--            newEnemy =
--                { left = 3
--                , side = Red
--                , x = x
--                , y = y
--                }
--        in
--        { model
--            | enemies = newEnemy :: model.enemies
--            , leftUntilSpawn = 8
--            , corner = modBy 4 (model.corner - 1)
--        }
--movePlayer : List Enemy -> Int -> Int -> Player -> Maybe Player
--movePlayer enemies deltaX deltaY player =
--    let
--        newX =
--            player.x + deltaX
--
--        newY =
--            player.y + deltaY
--    in
--    if List.any (controlledByEnemy newX newY) enemies then
--        Nothing
--    else
--        Just { player | x = newX, y = newY }
--stepEnemyTeam : Enemy -> Enemy
--stepEnemyTeam enemy =
--    if enemy.left > 0 then
--        { enemy | left = enemy.left - 1 }
--    else
--        { enemy
--            | left =
--                case enemy.side of
--                    Green ->
--                        8
--
--                    Red ->
--                        2
--            , side =
--                case enemy.side of
--                    Green ->
--                        Red
--
--                    Red ->
--                        Green
--        }
--
--
--hitEnemies : Player -> List Enemy -> List Enemy
--hitEnemies player enemies =
--    let
--        hitEnemy enemy =
--            case enemy.side of
--                Red ->
--                    Just enemy
--
--                Green ->
--                    if (player.x == enemy.x) && (player.y == enemy.y) then
--                        Nothing
--                    else
--                        Just enemy
--    in
--    List.filterMap hitEnemy enemies
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

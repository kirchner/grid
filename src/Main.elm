module Main exposing (main)

import AStar
import Browser exposing (Document)
import Browser.Events as Events
import Dict exposing (Dict)
import Ecs.System as Ecs exposing (Focus, Id, System)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



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


positionFromTuple : ( Int, Int ) -> Position
positionFromTuple ( x, y ) =
    { x = x, y = y }


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


type Model
    = Welcome
    | InLevel Int Int Level
    | GameWon Int
    | GameOver Int Level


init : {} -> ( Model, Cmd Msg )
init _ =
    ( Welcome
    , Cmd.none
    )



---- LEVELS


type alias Level =
    { width : Int
    , height : Int
    , system : System Store
    , info : String
    , isFinished : System Store -> Finished
    }


type Finished
    = Running
    | Lost Int
    | Won Int


emptySystem =
    { nextId = 0
    , position = Dict.empty
    , player = Dict.empty
    , enemy = Dict.empty
    , spawner = Dict.empty
    , mine = Dict.empty
    , trapper = Dict.empty
    }


level0 : Level
level0 =
    let
        width =
            6

        height =
            6
    in
    { width = width
    , height = height
    , system =
        emptySystem
            |> spawnPlayer 5 5
            |> spawnEnemy 3 Red { x = 0, y = 0 }
    , info = "Press the Arrow Keys!"
    , isFinished = isFinished width height 10
    }


level1 : Level
level1 =
    let
        width =
            9

        height =
            9
    in
    { width = width
    , height = height
    , system =
        emptySystem
            |> spawnPlayer 4 4
            |> spawnEnemy 1 Red { x = 0, y = 0 }
            |> spawnTrapper 8 { x = width - 1, y = height - 1 }
    , info = "Watch out!"
    , isFinished = isFinished width height 20
    }


isFinished : Int -> Int -> Int -> System Store -> Finished
isFinished width height score system =
    let
        allEnemiesGone =
            List.isEmpty (Ecs.having withEnemy system)
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
                                        List.any
                                            (controlledByEnemy
                                                neighbourX
                                                neighbourY
                                            )
                                            enemies
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

        enemies =
            Ecs.with2 withPosition withEnemy system
                |> List.map Tuple.second
    in
    if allEnemiesGone then
        Won score
    else if playerSurrounded then
        Lost 0
    else
        Running



---- SPAWNING


spawnPlayer :
    Int
    -> Int
    -> System Store
    -> System Store
spawnPlayer x y =
    Ecs.spawnEntity
        (\newId ->
            Ecs.setComponent withPlayer newId Player
                >> Ecs.setComponent withPosition newId { x = x, y = y }
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
            , Font.family
                [ Font.external
                    { name = "Lato"
                    , url = "https://fonts.googleapis.com/css?family=Lato"
                    }
                , Font.sansSerif
                ]
            , Font.size 32
            ]
            (case model of
                Welcome ->
                    viewWelcome

                InLevel score name system ->
                    viewLevel score name system

                GameWon score ->
                    viewGameWon score

                GameOver score level ->
                    viewGameOver score level
            )
        ]
    }



-- WELCOME


viewWelcome : Element Msg
viewWelcome =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 25
        ]
        [ Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (Element.text "Welcome!")
        , Input.button
            [ Element.centerX
            , Element.centerY
            , Element.padding 10
            , Border.width 2
            , Border.color (Element.rgb 0 0 0)
            , Background.color (Element.rgb 0 1 0)
            ]
            { onPress = Just StartClicked
            , label = Element.text "Start"
            }
        ]



-- IN LEVEL


type alias Tiles =
    { red : List Position
    , green : List Position
    , lightRed : List Position
    , path : List Position
    }


viewLevel : Int -> Int -> Level -> Element Msg
viewLevel score name { width, height, system, info } =
    Element.column
        [ Element.centerX
        , Element.centerY
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.el
                [ Element.alignLeft
                , Element.padding 5
                ]
                (Element.text ("Score: " ++ String.fromInt score))
            , Element.el
                [ Element.alignRight
                , Element.padding 5
                ]
                (Element.text ("Level #" ++ String.fromInt name))
            ]
        , viewSystem width height system
        , Element.el
            [ Element.padding 5 ]
            (Element.text info)
        ]


viewSystem : Int -> Int -> System Store -> Element Msg
viewSystem width height system =
    let
        tiles =
            { red = redTiles
            , green = greenTiles
            , lightRed = lightRedTiles
            , path = pathTiles
            }

        pathTiles =
            List.concat
                [ enemies
                    |> List.filterMap
                        (\( enemyId, ( _, enemyPosition ) ) ->
                            Ecs.with2 withPlayer withPosition system
                                |> List.head
                                |> Maybe.andThen
                                    (\( playerId, ( _, playerPosition ) ) ->
                                        let
                                            obstacles =
                                                getObstacles enemyId system
                                        in
                                        AStar.compute (aStarConfig width height obstacles)
                                            ( enemyPosition.x, enemyPosition.y )
                                            ( playerPosition.x, playerPosition.y )
                                    )
                                |> Maybe.map (List.map (\( x, y ) -> { x = x, y = y }))
                        )
                    |> List.concat
                , trappers
                    |> List.filterMap
                        (\( trapperId, ( _, trapperPosition ) ) ->
                            Ecs.with2 withPlayer withPosition system
                                |> List.head
                                |> Maybe.andThen
                                    (\( playerId, ( _, playerPosition ) ) ->
                                        let
                                            obstacles =
                                                getObstacles trapperId system
                                        in
                                        AStar.compute (aStarConfig width height obstacles)
                                            ( trapperPosition.x, trapperPosition.y )
                                            ( playerPosition.x, playerPosition.y )
                                    )
                                |> Maybe.map (List.map (\( x, y ) -> { x = x, y = y }))
                        )
                    |> List.concat
                ]

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
            trappers
                |> List.map (Tuple.second >> Tuple.second)

        trappers =
            Ecs.with2 withTrapper withPosition system

        mineTiles =
            Ecs.with2 withMine withPosition system
                |> List.map (Tuple.second >> Tuple.second)
    in
    Element.column
        [ Border.width 1
        , Border.color (Element.rgb 0 0 0)
        ]
        (List.range 0 (height - 1)
            |> List.map (viewRow width height tiles system)
        )


viewRow : Int -> Int -> Tiles -> System Store -> Int -> Element Msg
viewRow width height tiles system rowIndex =
    Element.row []
        (List.range 0 (width - 1)
            |> List.map (viewTile tiles system rowIndex)
        )


viewTile : Tiles -> System Store -> Int -> Int -> Element Msg
viewTile tiles system rowIndex columnIndex =
    let
        backgroundColor =
            let
                tilePosition =
                    { x = columnIndex, y = rowIndex }
            in
            if List.member tilePosition tiles.red then
                Element.rgb 1 0 0
            else if List.member tilePosition tiles.green then
                Element.rgb 0 1 0
            else if List.member tilePosition tiles.lightRed then
                Element.rgb 1 0.5 0.5
            else if List.member tilePosition tiles.path then
                Element.rgb 0.5 0.5 1
            else
                Element.rgb 1 1 1
    in
    Element.el
        [ Element.width (Element.px 80)
        , Element.height (Element.px 80)
        , Border.width 2
        , Border.color (Element.rgb 0 0 0)
        , Background.color backgroundColor
        ]
        Element.none



-- GAME WON


viewGameWon : Int -> Element Msg
viewGameWon score =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.spacing 25
        ]
        [ Element.el
            [ Element.centerX ]
            (Element.text "You have won!")
        , Element.el
            [ Element.centerX ]
            (Element.text ("Score: " ++ String.fromInt score))
        , Input.button
            [ Element.centerX
            , Element.centerY
            , Element.padding 10
            , Border.width 2
            , Border.color (Element.rgb 0 0 0)
            , Background.color (Element.rgb 0 1 0)
            ]
            { onPress = Just StartAgainClicked
            , label = Element.text "Start again"
            }
        ]



-- GAME OVER


viewGameOver : Int -> Level -> Element Msg
viewGameOver score level =
    Element.el
        [ Element.centerX
        , Element.centerY
        , Element.inFront <|
            Element.column
                [ Element.centerX
                , Element.centerY
                , Element.spacing 25
                , Element.padding 15
                , Border.width 2
                , Border.color (Element.rgb 0 0 0)
                , Background.color (Element.rgb 1 1 1)
                ]
                [ Element.el
                    [ Element.centerX ]
                    (Element.text "Gameover")
                , Element.el
                    [ Element.centerX ]
                    (Element.text ("Score: " ++ String.fromInt score))
                , Input.button
                    [ Element.centerX
                    , Element.centerY
                    , Element.padding 10
                    , Border.width 2
                    , Border.color (Element.rgb 0 0 0)
                    , Background.color (Element.rgb 0 1 0)
                    ]
                    { onPress = Just StartAgainClicked
                    , label = Element.text "Start again"
                    }
                ]
        ]
        (viewSystem level.width level.height level.system)



---- UPDATE


type Msg
    = StartClicked
      -- IN LEVEL
    | ArrowUpPressed
    | ArrowDownPressed
    | ArrowLeftPressed
    | ArrowRightPressed
      -- GAME WON/OVER
    | StartAgainClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Welcome ->
            case msg of
                StartClicked ->
                    ( InLevel 0 0 level0, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InLevel score name level ->
            let
                handleArrowPressed deltaX deltaY =
                    let
                        newSystem =
                            step level.width level.height deltaX deltaY level.system

                        newLevel =
                            { level | system = newSystem }
                    in
                    case level.isFinished newSystem of
                        Running ->
                            ( InLevel score name newLevel
                            , Cmd.none
                            )

                        Lost levelScore ->
                            ( GameOver (score + levelScore) newLevel
                            , Cmd.none
                            )

                        Won levelScore ->
                            case name of
                                0 ->
                                    ( InLevel (score + levelScore) 1 level1
                                    , Cmd.none
                                    )

                                1 ->
                                    ( GameWon (score + levelScore)
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )
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

                _ ->
                    ( model, Cmd.none )

        GameWon score ->
            case msg of
                StartAgainClicked ->
                    ( InLevel 0 0 level0
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GameOver score _ ->
            case msg of
                StartAgainClicked ->
                    ( InLevel 0 0 level0
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



--- STEP


step : Int -> Int -> Int -> Int -> System Store -> System Store
step width height deltaX deltaY system =
    system
        |> movePlayer width height deltaX deltaY
        |> Maybe.map
            (fight
                >> moveEnemies width height
                >> moveTrappers width height
                >> switchEnemiesTeam
                >> stepSpawners
            )
        |> Maybe.withDefault system



-- STEP PLAYER MOVE


movePlayer : Int -> Int -> Int -> Int -> System Store -> Maybe (System Store)
movePlayer width height deltaX deltaY system =
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
                else if
                    newPlayerPosition
                        /= normalizePosition width height newPlayerPosition
                then
                    Nothing
                else
                    Just <|
                        Ecs.setComponent withPosition
                            playerId
                            (normalizePosition width height newPlayerPosition)
                            system
            )


normalizePosition : Int -> Int -> Position -> Position
normalizePosition width height position =
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


moveEnemies : Int -> Int -> System Store -> System Store
moveEnemies width height system =
    List.foldl (moveEnemy width height) system (Ecs.with2 withEnemy withPosition system)


moveEnemy : Int -> Int -> ( Id, ( Enemy, Position ) ) -> System Store -> System Store
moveEnemy width height ( id, ( enemy, enemyPosition ) ) system =
    let
        obstacles =
            getObstacles id system

        playerPosition =
            Ecs.with2 withPlayer withPosition system
                |> List.head
                |> Maybe.map (Tuple.second >> Tuple.second)
    in
    Maybe.map (walkTo width height obstacles enemyPosition)
        playerPosition
        |> Maybe.map
            (\newPosition ->
                Ecs.setComponent withPosition id newPosition system
            )
        |> Maybe.withDefault system


walkTo :
    Int
    -> Int
    -> List Position
    -> Position
    -> Position
    -> Position
walkTo width height obstacles enemyPosition playerPosition =
    let
        maybeNewEnemyPosition =
            AStar.compute (aStarConfig width height obstacles)
                ( enemyPosition.x, enemyPosition.y )
                ( playerPosition.x, playerPosition.y )
                |> Maybe.andThen (List.drop 1 >> List.head)
                |> Maybe.map positionFromTuple
    in
    case maybeNewEnemyPosition of
        Nothing ->
            enemyPosition

        Just newEnemyPosition ->
            if List.member newEnemyPosition obstacles || newEnemyPosition == playerPosition then
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


aStarConfig : Int -> Int -> List Position -> AStar.Config
aStarConfig width height obstacles =
    { heuristicCostEstimate =
        \goal current ->
            toFloat <|
                ((Tuple.first goal - Tuple.first current) ^ 2)
                    + ((Tuple.second goal - Tuple.second current) ^ 2)
    , neighbours = neighbours width height obstacles
    }


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
        ]


moveTrappers : Int -> Int -> System Store -> System Store
moveTrappers width height system =
    List.foldl (moveTrapper width height) system (Ecs.with2 withTrapper withPosition system)


moveTrapper : Int -> Int -> ( Id, ( Trapper, Position ) ) -> System Store -> System Store
moveTrapper width height ( id, ( trapper, trapperPosition ) ) system =
    let
        obstacles =
            getObstacles id system

        playerPosition =
            Ecs.with2 withPlayer withPosition system
                |> List.head
                |> Maybe.map (Tuple.second >> Tuple.second)
    in
    Maybe.map (walkTo width height obstacles trapperPosition)
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

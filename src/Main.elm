module Main exposing (main)

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
import Browser exposing (Document)
import Browser.Events as Events
import Dict exposing (Dict)
import Ecs.System as Ecs exposing (Focus, Id, System)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as Attributes
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
    , movement : Dict Id Movement
    , obstruction : Dict Id Obstruction
    , control : Dict Id Control
    , player : Dict Id Player
    , switcher : Dict Id Switcher
    , spawner : Dict Id Spawner
    , mine : Dict Id Mine
    , trapper : Dict Id Trapper
    , sprite : Dict Id Sprite
    }


removeEntity : Id -> System Store -> System Store
removeEntity id system =
    system
        |> Ecs.removeComponent withPosition id
        |> Ecs.removeComponent withMovement id
        |> Ecs.removeComponent withObstruction id
        |> Ecs.removeComponent withControl id
        |> Ecs.removeComponent withPlayer id
        |> Ecs.removeComponent withSwitcher id
        |> Ecs.removeComponent withSpawner id
        |> Ecs.removeComponent withMine id
        |> Ecs.removeComponent withTrapper id
        |> Ecs.removeComponent withSprite id



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


type Movement
    = Movement


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


type Player
    = Player


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
    }


type Sprite
    = SwitcherSprite
    | TrapperSprite
    | ChessRookSprite
    | PlayerSprite


spriteIcon : Sprite -> String
spriteIcon sprite =
    case sprite of
        SwitcherSprite ->
            "chess-pawn"

        TrapperSprite ->
            "chess-bishop"

        ChessRookSprite ->
            "chess-rook"

        PlayerSprite ->
            "chess-queen"


spriteForegroundColor : Sprite -> Element.Color
spriteForegroundColor sprite =
    case sprite of
        SwitcherSprite ->
            Element.rgb 0 0 0

        TrapperSprite ->
            Element.rgb 0 0 0

        ChessRookSprite ->
            Element.rgb 0 0 0

        PlayerSprite ->
            Element.rgb 0 0 0



---- MODEL


type Model
    = Welcome
    | Playing PlayingData
    | GameWon Int
    | GameOver Int Level


type alias PlayingData =
    { score : Int
    , remainingLevels : List Level
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( Welcome, Cmd.none )



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


emptySystem =
    { nextId = 0
    , position = Dict.empty
    , player = Dict.empty
    , switcher = Dict.empty
    , spawner = Dict.empty
    , mine = Dict.empty
    , trapper = Dict.empty
    , movement = Dict.empty
    , obstruction = Dict.empty
    , control = Dict.empty
    , sprite = Dict.empty
    }


allLevels : List Level
allLevels =
    [ level0
    , level1
    ]


level0 : Level
level0 =
    let
        width =
            6

        height =
            6
    in
    { name = "Level #0"
    , width = width
    , height = height
    , system =
        emptySystem
            |> spawnPlayer { x = 5, y = 5 }
            |> spawnSwitcher { x = 0, y = 0 } { left = 3 }
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
    { name = "Level #1"
    , width = width
    , height = height
    , system =
        emptySystem
            |> spawnPlayer { x = 4, y = 4 }
            -- |> spawnSwitcher { x = 0, y = 0 } { left = 1 }
            |> spawnChessRook { x = 0, y = 0 }
            |> spawnTrapper { x = width - 1, y = height - 1 } { left = 8 }
    , info = "Watch out!"
    , isFinished = isFinished width height 20
    }


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



---- SPAWNING


spawnPlayer : Position -> System Store -> System Store
spawnPlayer position system =
    Ecs.spawnEntity
        (\newId ->
            Ecs.setComponent withPlayer newId Player
                >> Ecs.setComponent withPosition newId position
                >> Ecs.setComponent withSprite newId PlayerSprite
        )
        system


spawnSwitcher : Position -> Switcher -> System Store -> System Store
spawnSwitcher position switcher =
    Ecs.spawnEntity
        (\newId ->
            identity
                >> Ecs.setComponent withSwitcher newId switcher
                >> Ecs.setComponent withPosition newId position
                >> Ecs.setComponent withObstruction newId Obstruction
                >> Ecs.setComponent withMovement newId Movement
                >> Ecs.setComponent withControl newId SwitcherControl
                >> Ecs.setComponent withSprite newId SwitcherSprite
        )


spawnChessRook : Position -> System Store -> System Store
spawnChessRook position =
    Ecs.spawnEntity
        (\newId ->
            identity
                >> Ecs.setComponent withPosition newId position
                >> Ecs.setComponent withObstruction newId Obstruction
                >> Ecs.setComponent withMovement newId Movement
                >> Ecs.setComponent withControl newId ChessRookControl
                >> Ecs.setComponent withSprite newId ChessRookSprite
        )


spawnTrapper : Position -> Trapper -> System Store -> System Store
spawnTrapper position trapper =
    Ecs.spawnEntity
        (\newId ->
            identity
                >> Ecs.setComponent withTrapper newId trapper
                >> Ecs.setComponent withPosition newId position
                >> Ecs.setComponent withObstruction newId Obstruction
                >> Ecs.setComponent withMovement newId Movement
                >> Ecs.setComponent withSprite newId TrapperSprite
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

                Playing data ->
                    List.head data.remainingLevels
                        |> Maybe.map (viewLevel data.score)
                        |> Maybe.withDefault Element.none

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


viewLevel : Int -> Level -> Element Msg
viewLevel score { name, width, height, system, info } =
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
                , Font.italic
                ]
                (Element.text name)
            ]
        , viewSystem width height system
        , Element.el
            [ Element.padding 5 ]
            (Element.text info)
        ]


type alias TileInfo =
    { backgroundColors : Dict ( Int, Int ) Element.Color
    , foregroundColors : Dict ( Int, Int ) Element.Color
    , icons : Dict ( Int, Int ) String
    , infos : Dict ( Int, Int ) String
    }


computeTileInfo : Int -> Int -> System Store -> TileInfo
computeTileInfo width height system =
    { backgroundColors =
        [ Ecs.with2 withControl withPosition system
            |> List.concatMap
                (\( controlId, ( control, position ) ) ->
                    rasterizeControl width height control position
                )
            |> List.map (\position -> ( ( position.x, position.y ), Element.rgb 1 0.5 0.5 ))
        , Ecs.with2 withPlayer withPosition system
            |> List.map (Tuple.second >> Tuple.second)
            |> List.map (\position -> ( ( position.x, position.y ), Element.rgb 0 1 0 ))
        ]
            |> List.concat
            |> Dict.fromList
    , foregroundColors =
        Ecs.with2 withSprite withPosition system
            |> List.map
                (\( spriteId, ( sprite, spritePosition ) ) ->
                    ( ( spritePosition.x, spritePosition.y ), spriteForegroundColor sprite )
                )
            |> Dict.fromList
    , icons =
        Ecs.with2 withSprite withPosition system
            |> List.map
                (\( spriteId, ( sprite, spritePosition ) ) ->
                    ( ( spritePosition.x, spritePosition.y ), spriteIcon sprite )
                )
            |> Dict.fromList
    , infos =
        [ Ecs.with2 withSwitcher withPosition system
            |> List.map
                (\( _, ( { left }, switcherPosition ) ) ->
                    ( ( switcherPosition.x, switcherPosition.y )
                    , String.fromInt left
                    )
                )
        , Ecs.with2 withTrapper withPosition system
            |> List.map
                (\( _, ( { left }, trapperPosition ) ) ->
                    ( ( trapperPosition.x, trapperPosition.y )
                    , String.fromInt left
                    )
                )
        ]
            |> List.concat
            |> Dict.fromList
    }


viewSystem : Int -> Int -> System Store -> Element Msg
viewSystem width height system =
    Element.column
        [ Border.width 1
        , Border.color (Element.rgb 0 0 0)
        ]
        (List.range 0 (height - 1)
            |> List.map (viewRow width height (computeTileInfo width height system) system)
        )


viewRow : Int -> Int -> TileInfo -> System Store -> Int -> Element Msg
viewRow width height tileInfo system rowIndex =
    Element.row []
        (List.range 0 (width - 1)
            |> List.map (viewTile tileInfo system rowIndex)
        )


viewTile : TileInfo -> System Store -> Int -> Int -> Element Msg
viewTile tileInfo system rowIndex columnIndex =
    let
        backgroundColor =
            Dict.get ( columnIndex, rowIndex ) tileInfo.backgroundColors
                |> Maybe.withDefault (Element.rgb 1 1 1)

        foregroundColor =
            Dict.get ( columnIndex, rowIndex ) tileInfo.foregroundColors
                |> Maybe.withDefault (Element.rgb 0 0 0)

        iconName =
            Dict.get ( columnIndex, rowIndex ) tileInfo.icons
                |> Maybe.withDefault ""

        info =
            Dict.get ( columnIndex, rowIndex ) tileInfo.infos
    in
    Element.el
        [ Element.width (Element.px 80)
        , Element.height (Element.px 80)
        , Border.width 2
        , Border.color (Element.rgb 0 0 0)
        , Font.color foregroundColor
        , Background.color backgroundColor
        , Element.above <|
            case info of
                Nothing ->
                    Element.none

                Just infoText ->
                    Element.el
                        [ Element.width Element.fill
                        , Font.size 24
                        , Element.moveDown 28
                        , Element.moveLeft 5
                        ]
                        (Element.el
                            [ Element.alignRight ]
                            (Element.text infoText)
                        )
        ]
        (icon iconName)



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



-- HELPER


icon : String -> Element msg
icon name =
    Element.html <|
        Html.i
            [ Attributes.class "fas"
            , Attributes.class ("fa-" ++ name)
            , Attributes.style "text-align" "center"
            , Attributes.style "margin" "auto"
            ]
            []



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
                    ( Playing
                        { score = 0
                        , remainingLevels = allLevels
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Playing { score, remainingLevels } ->
            case remainingLevels of
                [] ->
                    ( model, Cmd.none )

                level :: rest ->
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
                                    ( Playing
                                        { score = score
                                        , remainingLevels = newLevel :: rest
                                        }
                                    , Cmd.none
                                    )

                                Lost levelScore ->
                                    ( GameOver (score + levelScore) newLevel
                                    , Cmd.none
                                    )

                                Won levelScore ->
                                    case rest of
                                        [] ->
                                            ( GameWon (score + levelScore)
                                            , Cmd.none
                                            )

                                        _ ->
                                            ( Playing
                                                { score = score + levelScore
                                                , remainingLevels = rest
                                                }
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

                        _ ->
                            ( model, Cmd.none )

        GameWon score ->
            case msg of
                StartAgainClicked ->
                    ( Playing
                        { score = 0
                        , remainingLevels = allLevels
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GameOver score _ ->
            case msg of
                StartAgainClicked ->
                    ( Playing
                        { score = 0
                        , remainingLevels = allLevels
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



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



--- STEP


debugStep : Bool
debugStep =
    False


step : Int -> Int -> Int -> Int -> System Store -> System Store
step width height deltaX deltaY system =
    let
        optionallyDebugStep newSystem =
            if debugStep then
                --Debug.log "newSystem" newSystem
                newSystem
            else
                newSystem
    in
    system
        |> movePlayer width height deltaX deltaY
        |> Maybe.map
            (fight
                >> moveEntities width height
                >> stepTrapper
                >> stepEnemies
                >> stepSpawners
            )
        |> Maybe.withDefault system
        |> optionallyDebugStep



-- MOVE PLAYER


movePlayer : Int -> Int -> Int -> Int -> System Store -> Maybe (System Store)
movePlayer width height deltaX deltaY system =
    let
        enemies =
            Ecs.with2 withPosition withSwitcher system
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
                        Ecs.setComponent withPosition
                            playerId
                            normalizedPlayerPosition
                            system
            )



-- FIGHT


fight : System Store -> System Store
fight system =
    system
        |> fightEnemies
        |> fightTrappers


fightEnemies : System Store -> System Store
fightEnemies system =
    List.foldl fightSwitcher system (Ecs.with2 withSwitcher withPosition system)


fightSwitcher : ( Id, ( Switcher, Position ) ) -> System Store -> System Store
fightSwitcher ( id, ( switcher, switcherPosition ) ) system =
    let
        hasControl =
            Ecs.getComponent withControl id system /= Nothing
    in
    Maybe.map
        (\playerPosition ->
            if hasControl then
                system
            else if
                (playerPosition.x == switcherPosition.x)
                    && (playerPosition.y == switcherPosition.y)
            then
                removeEntity id system
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



-- MOVE ENTITIES


moveEntities : Int -> Int -> System Store -> System Store
moveEntities width height system =
    List.foldl (moveEntity width height)
        system
        (Ecs.with2 withMovement withPosition system)


moveEntity : Int -> Int -> ( Id, ( Movement, Position ) ) -> System Store -> System Store
moveEntity width height ( id, ( _, position ) ) system =
    let
        obstacles =
            getObstructedPositions id system

        playerPosition =
            Ecs.with2 withPlayer withPosition system
                |> List.head
                |> Maybe.map (Tuple.second >> Tuple.second)
    in
    Maybe.map (walkTo width height obstacles position)
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
walkTo width height obstacles position playerPosition =
    let
        maybeNewPosition =
            AStar.compute (aStarConfig width height obstacles)
                ( position.x, position.y )
                ( playerPosition.x, playerPosition.y )
                |> Maybe.andThen (List.drop 1 >> List.head)
                |> Maybe.map positionFromTuple
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
            |> Ecs.setComponent withTrapper id { trapper | left = 8 }
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
                            8
                }
            |> (if hasControl then
                    Ecs.removeComponent withControl id
                else
                    Ecs.setComponent withControl id SwitcherControl
               )



-- STEP SPAWNERS


stepSpawners : System Store -> System Store
stepSpawners system =
    List.foldl stepSpawner system (Ecs.with2 withSpawner withPosition system)


stepSpawner : ( Id, ( Spawner, Position ) ) -> System Store -> System Store
stepSpawner ( id, ( Spawner spawner, spawnerPosition ) ) system =
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

module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
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



---- MODEL


type alias Model =
    { player : Player
    , enemies : List Enemy
    , leftUntilSpawn : Int
    , corner : Int
    }


type alias Player =
    { x : Int
    , y : Int
    }


type alias Enemy =
    { left : Int
    , side : Side
    , x : Int
    , y : Int
    }


type Side
    = Red
    | Green


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { player =
            { x = 10
            , y = 6
            }
      , enemies =
            [ { left = 3
              , side = Red
              , x = 0
              , y = 0
              }
            , { left = 1
              , side = Red
              , x = 2
              , y = 5
              }
            ]
      , leftUntilSpawn = 8
      , corner = 3
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
            (viewGrid model.player model.enemies)
        ]
    }


viewGrid player enemies =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (List.range 0 (height - 1)
            |> List.map (viewRow player enemies)
        )


viewRow player enemies rowIndex =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (List.range 0 (width - 1)
            |> List.map (viewField player enemies rowIndex)
        )


viewField player enemies rowIndex columnIndex =
    let
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
            (rowIndex == player.y)
                && (columnIndex == player.x)

        housesEnemy =
            enemies
                |> List.filterMap
                    (\enemy ->
                        if (enemy.x == columnIndex) && (enemy.y == rowIndex) then
                            Just enemy.side
                        else
                            Nothing
                    )
                |> List.head

        nextToEnemy =
            enemies
                |> List.filterMap
                    (\enemy ->
                        if controlledByEnemy columnIndex rowIndex enemy then
                            Just enemy.side
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
update msg ({ player } as model) =
    let
        handleArrowPressed deltaX deltaY =
            case movePlayer model.enemies deltaX deltaY model.player of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just newPlayer ->
                    ( { model
                        | player =
                            newPlayer
                        , enemies =
                            model.enemies
                                |> hitEnemies newPlayer
                                |> stepEnemiesPosition newPlayer
                                |> List.map stepEnemyTeam
                      }
                        |> stepSpawning
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


stepSpawning : Model -> Model
stepSpawning model =
    if model.leftUntilSpawn > 0 then
        { model | leftUntilSpawn = model.leftUntilSpawn - 1 }
    else
        let
            ( x, y ) =
                case model.corner of
                    0 ->
                        ( 0, 0 )

                    1 ->
                        ( width - 1, 0 )

                    2 ->
                        ( width - 1, height - 1 )

                    3 ->
                        ( 0, height - 1 )

                    _ ->
                        ( 0, 0 )

            newEnemy =
                { left = 3
                , side = Red
                , x = x
                , y = y
                }
        in
        { model
            | enemies = newEnemy :: model.enemies
            , leftUntilSpawn = 8
            , corner = modBy 4 (model.corner - 1)
        }


movePlayer : List Enemy -> Int -> Int -> Player -> Maybe Player
movePlayer enemies deltaX deltaY player =
    let
        newX =
            player.x + deltaX

        newY =
            player.y + deltaY
    in
    if List.any (controlledByEnemy newX newY) enemies then
        Nothing
    else
        Just { player | x = newX, y = newY }


stepEnemyTeam : Enemy -> Enemy
stepEnemyTeam enemy =
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


hitEnemies : Player -> List Enemy -> List Enemy
hitEnemies player enemies =
    let
        hitEnemy enemy =
            case enemy.side of
                Red ->
                    Just enemy

                Green ->
                    if (player.x == enemy.x) && (player.y == enemy.y) then
                        Nothing
                    else
                        Just enemy
    in
    List.filterMap hitEnemy enemies


stepEnemiesPosition : Player -> List Enemy -> List Enemy
stepEnemiesPosition player enemies =
    stepEnemiesPositionHelp player [] enemies


stepEnemiesPositionHelp : Player -> List Enemy -> List Enemy -> List Enemy
stepEnemiesPositionHelp player steppedEnemies enemies =
    case enemies of
        [] ->
            List.reverse steppedEnemies

        enemy :: rest ->
            let
                newEnemy =
                    stepEnemy player (steppedEnemies ++ rest) enemy
            in
            stepEnemiesPositionHelp player (newEnemy :: steppedEnemies) rest


stepEnemy : Player -> List Enemy -> Enemy -> Enemy
stepEnemy player otherEnemies enemy =
    let
        deltaX =
            player.x - enemy.x

        deltaY =
            player.y - enemy.y

        newEnemy =
            if abs deltaX >= abs deltaY then
                if deltaX > 0 then
                    { enemy | x = enemy.x + 1 }
                else if deltaX < 0 then
                    { enemy | x = enemy.x - 1 }
                else
                    enemy
            else if deltaY > 0 then
                { enemy | y = enemy.y + 1 }
            else if deltaY < 0 then
                { enemy | y = enemy.y - 1 }
            else
                enemy
    in
    if List.any (controlledByEnemy newEnemy.x newEnemy.y) otherEnemies then
        enemy
    else if (newEnemy.x == player.x) && (newEnemy.y == player.y) then
        enemy
    else
        newEnemy


availableForPlayer : Int -> Int -> List Enemy -> Player -> Bool
availableForPlayer x y enemies player =
    not (List.any (controlledByEnemy x y) enemies)
        && (player.x >= x - 1)
        && (player.x <= x + 1)
        && (player.y >= y - 1)
        && (player.y <= y + 1)


controlledByEnemy : Int -> Int -> Enemy -> Bool
controlledByEnemy x y enemy =
    case enemy.side of
        Red ->
            (enemy.x >= x - 1)
                && (enemy.x <= x + 1)
                && (enemy.y >= y - 1)
                && (enemy.y <= y + 1)

        Green ->
            False


isOn : Int -> Int -> Enemy -> Bool
isOn x y enemy =
    (x == enemy.x)
        && (y == enemy.y)



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

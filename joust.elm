module SvgAnimation exposing (..)

import Html as Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes exposing (style)
import Set exposing (Set)
import Keyboard exposing (..)
import Time exposing (Time)
import Window
import Task
import Html.Events exposing (onClick)
import Html.Attributes as Attr



{- Main -}
main : Program Never Game Msg
main = Html.program
       {init = (init,initCmds),
        update = update,
        view   = view,
        subscriptions = subscriptions }

type alias Game = {
        dimensions                  : Window.Size,
        position                    : Coords,
        ai1                         : Ai,
        ai2                         : Ai,
        direction                   : Direction,
        previousDirection           : Direction,
        isDead                      : Bool,
        blockSize                   : Float,
        momentumSpeedCounter        : Int,
        directionMomentumCounter    : Int,
        upMomentumCounter           : Int,
        keysDown                    : Set KeyCode,
        keySum                      : Int,
        score                       : Int
                   }

type alias Coords = { x : Int, y : Int }

type alias Ai = {
        pos : Coords,
        isDead : Bool,
        pic : Int,
        direction : Direction
                  }

type alias Platform = {
        leftPos   : Int,
        rightPos  : Int,
        upPos     : Int,
        downPos   : Int,
        playerPos : Int
                        }

type Direction
    = Left
    | Right
    | NoDirection
--KeyMsg Keyboard.KeyCode
type Msg
    = StartGame
    | KeyDown KeyCode
    | KeyUp KeyCode
    | SizeUpdated Window.Size
    | Tick Time
    | Nothing

init = ({ dimensions = Window.Size 1920 1080,
            position = {x = 350, y = 120},
            ai1 = {pos = {x = 0, y = 50}, isDead = False, pic = 0, direction = Right},
            ai2 = {pos = {x = 800, y = 250}, isDead = False, pic = 0, direction = Left},
            direction = NoDirection,
            previousDirection = NoDirection,
            isDead = True,
            blockSize = 0,
            momentumSpeedCounter = 0,
            directionMomentumCounter = 0,
            upMomentumCounter = 0,
            keysDown = Set.empty,
            keySum = 0,
            score = 0})

update : Msg -> Game -> (Game,Cmd.Cmd Msg)
update msg model = case msg of
        StartGame ->
            ({model | isDead = False, position = {x = 350, y = 120}}, Cmd.none)
        KeyDown keyCode ->
            (keyCode, model, Cmd.none)
            |> insertKeys
            |> movePos

        KeyUp keyCode ->
            ({ model | keysDown = Set.remove keyCode model.keysDown }, Cmd.none)

        SizeUpdated dimensions ->
            ({ model | dimensions = dimensions }, Cmd.none )

        Nothing -> (model, Cmd.none)

        Tick time ->
            updateGame model

insertKeys : (Int, Game, Cmd Msg) -> (Int, Game, Cmd Msg)
insertKeys (keyCode, model, cmd) =
    (keyCode, { model | keysDown = Set.insert keyCode model.keysDown }, Cmd.none)

movePos : (Int, Game, Cmd Msg) -> (Game, Cmd Msg)
movePos (keyCode, model, cmd) =
    case (List.sum (Set.toList model.keysDown)) of
      32 -> ( { model | previousDirection = model.direction, upMomentumCounter = 12 }, Cmd.none)
      --83 -> ({ model | previousDirection = model.direction, direction = Down }, Cmd.none)
      65 -> ( { model | previousDirection = model.direction, direction = Left, momentumSpeedCounter = -4 }, Cmd.none)
      68 -> ( { model | previousDirection = model.direction, direction = Right, momentumSpeedCounter = 4 }, Cmd.none)
      97 -> ( { model | previousDirection = model.direction, direction = Left, momentumSpeedCounter = -4, upMomentumCounter = 12 }, Cmd.none)
      100 -> ( { model | previousDirection = model.direction, direction = Right, momentumSpeedCounter = 4, upMomentumCounter = 12 }, Cmd.none)
      --65 -> ({ model | position = { x = model.position.x - 20, y = model.position.y}, previousDirection = model.direction, direction = Left }, Cmd.none)
      --68 -> ({ model | position = { x = model.position.x + 20, y = model.position.y}, previousDirection = model.direction, direction = Right }, Cmd.none)
      _ -> ( model, Cmd.none)

updateGame : Game -> ( Game, Cmd Msg )
updateGame model =
        ( model )
            |> blockSize
            |> ai1Pos
            |> ai2Pos
            |> ai1collision
            |> ai2collision
            |> momentum
            |> gravity
            |> upMomentum
            |> updateDirection
            |> basePlatform
            |> leftPlatform
            |> middlePlatform
            |> rightPlatform
            |> outOfScreen
            |> pic
            |> pic2

blockSize : Game -> ( Game, Cmd Msg )
blockSize model =
    ( { model | blockSize = (toFloat (winWidth model.dimensions))/800.0}, Cmd.none)

{-
ai1Pos : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
ai1Pos (model, cmd) =
      let
          ab = True
      in
          if model.ai1.pos.x > 800 then
              ({ model | ai1 = { pos = { x = 0, y = model.ai1.pos.y}}}, Cmd.none)
          else
              ({ model | ai1 = { pos = { x = model.ai1.pos.x + 10, y = model.ai1.pos.y}}}, Cmd.none)
-}

ai1Pos : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
ai1Pos (model, cmd) =
    if model.ai1.pos.x > 800 then
        ({ model | ai1 = { pos = { x = -100, y = model.ai1.pos.y}, isDead = model.ai1.isDead, pic = model.ai1.pic, direction = model.ai1.direction}}, Cmd.none)
    else
        ({ model | ai1 = { pos = { x = model.ai1.pos.x + 5, y = model.ai1.pos.y}, isDead = model.ai1.isDead, pic = model.ai1.pic, direction = model.ai1.direction}}, Cmd.none)

ai2Pos : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
ai2Pos (model, cmd) =
    if model.ai2.pos.x < -70 then
        ({ model | ai2 = { pos = { x = 900, y = model.ai2.pos.y}, isDead = model.ai2.isDead, pic = model.ai2.pic, direction = model.ai2.direction}}, Cmd.none)
    else
        ({ model | ai2 = { pos = { x = model.ai2.pos.x - 5, y = model.ai2.pos.y}, isDead = model.ai2.isDead, pic = model.ai2.pic, direction = model.ai2.direction}}, Cmd.none)

ai1collision : ( Game , Cmd Msg ) -> ( Game, Cmd Msg )
ai1collision ( model, cmd ) =
    let
        aix = model.ai1.pos.x
        aiy = model.ai1.pos.y
        px = model.position.x
        py = model.position.y

    in
        if (abs(aix - px) <= 40) then
            if ((py - aiy) < -30) && ((py - aiy) > -50) then
                ({ model | score = model.score + 1, ai1 = { pos = { x = -400, y = model.ai1.pos.y}, isDead = model.ai1.isDead, pic = model.ai1.pic, direction = model.ai1.direction}}, Cmd.none)
            else if ((py - aiy) < 50) && ((py - aiy) > -30) then
                ({ model | isDead = True}, Cmd.none)
            else (model , Cmd.none)
        else (model , Cmd.none)

ai2collision : ( Game , Cmd Msg ) -> ( Game, Cmd Msg )
ai2collision ( model, cmd ) =
    let
        aix = model.ai2.pos.x
        aiy = model.ai2.pos.y
        px = model.position.x
        py = model.position.y

    in
        if (abs(aix - px) <= 40) then
            if ((py - aiy) < -30) && ((py - aiy) > -50) then
                ({ model | score = model.score + 1, ai2 = { pos = { x = 1200, y = model.ai2.pos.y}, isDead = model.ai2.isDead, pic = model.ai2.pic, direction = model.ai2.direction}}, Cmd.none)
            else if ((py - aiy) < 50) && ((py - aiy) > -30) then
                ({ model | isDead = True}, Cmd.none)
            else (model , Cmd.none)
        else (model , Cmd.none)

updateDirection : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
updateDirection ( model , cmd )= ({ model | previousDirection = model.direction, direction = model.direction }, Cmd.none)


momentum : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
momentum ( model , cmd ) =
    let

        reset =         if model.previousDirection == model.direction then model.momentumSpeedCounter
                        else 0

        moreSpeed =     --if reset /= 0 then
                        if ((model.momentumSpeedCounter > -8) && (model.momentumSpeedCounter < 8)) then
                            if model.direction == Left then -1
                            else 1
                        else 0
                        --else 0

        newCounter = reset + moreSpeed
        speed = speedConversion model


        --speed = speedConversion model

    in

        if model.direction == Left then
            ({ model | position = { x = model.position.x - speed, y = model.position.y}, momentumSpeedCounter = newCounter}, Cmd.none)
        else if model.direction == Right then
            ({ model | position = { x = model.position.x + speed, y = model.position.y}, momentumSpeedCounter = newCounter}, Cmd.none)
        else ({ model | position = { x = model.position.x, y = model.position.y } }, Cmd.none)

        {-
        if model.direction == Left then
            ({ model | position = { x = model.position.x - speed, y = model.position.y}, momentumSpeedCounter = 0 }, Cmd.none)
        else if model.direction == Right then
            ({ model | position = { x = model.position.x + speed, y = model.position.y}, momentumSpeedCounter = 0 }, Cmd.none)
        else ({ model | position = { x = model.position.x, y = model.position.y } }, Cmd.none)
        -}


upMomentum : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
upMomentum ( model , cmd ) =
    let
        upCounter = if model.upMomentumCounter > 0 then
                    model.upMomentumCounter - 1
                    else model.upMomentumCounter
        --speed = upSpeedConversion upCounter
    in
        ({ model | position = { x = model.position.x, y = model.position.y - upCounter}, upMomentumCounter = upCounter }, Cmd.none)
{--
upSpeedConversion : Int -> Int
upSpeedConversion speed =
    if speed == 0 then 0
    else if speed == 1 then 1
    else if speed == 2 then 2
    else if speed == 3 then 3
    else if speed == 4 then 4
    else if speed == 5 then 5
    else if speed == 6 then 6
    else if speed == 7 then 7
    else if speed == 8 then 8
    else if speed == 9 then 9
    else 0
-}

gravity : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
gravity ( model, cmd ) =
    ({ model | position = { x = model.position.x, y = model.position.y + 4} }, Cmd.none)

basePlatform : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
basePlatform ( model, cmd ) =
    let
        leftPos = (round ((39.0 * 800)/236)) - 25
        rightPos = (round (800.0 - ((48.0 * 800))/236)) - 25
        upPos = (400 - 50 - (round ((21.0 * 400)/118)))
        downPos = (400 - 50 - (round ((15.0 * 400)/118)))
    in
        if (model.position.y > upPos) && (model.position.y < downPos)
            && (model.position.x > leftPos) && (model.position.x < rightPos)
            then ({ model | position = { x = model.position.x, y = upPos} }, Cmd.none)
        else ({ model | position = { x = model.position.x, y = model.position.y} }, Cmd.none)

leftPlatform : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
leftPlatform ( model , cmd ) =
    let
        leftPos = 0 - 25
        rightPos = (round ((49 * 400)/118))-25
        upPos = (round ((31 * 400)/118))-50
        downPos = (round ((33 * 400)/118))-50
    in
        if (model.position.y > upPos) && (model.position.y < downPos)
            && (model.position.x > leftPos) && (model.position.x < rightPos)
            then ({ model | position = { x = model.position.x, y = upPos} }, Cmd.none)
        else ({ model | position = { x = model.position.x, y = model.position.y} }, Cmd.none)

middlePlatform : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
middlePlatform ( model , cmd ) =
    let
        leftPos = (round ((81 * 400)/118))-25
        rightPos = (round ((132 * 400)/118))-25
        upPos = (round ((54 * 400)/118))-50
        downPos = (round ((56 * 400)/118))-50
    in
        if (model.position.y > upPos) && (model.position.y < downPos)
            && (model.position.x > leftPos) && (model.position.x < rightPos)
            then ({ model | position = { x = model.position.x, y = upPos} }, Cmd.none)
        else ({ model | position = { x = model.position.x, y = model.position.y} }, Cmd.none)

rightPlatform : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
rightPlatform ( model , cmd ) =
    let
        leftPos = (round (((236-35) * 400)/118))-25
        rightPos = (round ((236 * 400)/118)) - 25
        upPos = (round ((31 * 400)/118))-50
        downPos = (round ((33 * 400)/118))-50
    in
        if (model.position.y > upPos) && (model.position.y < downPos)
            && (model.position.x > leftPos) && (model.position.x < rightPos)
            then ({ model | position = { x = model.position.x, y = upPos} }, Cmd.none)
        else ({ model | position = { x = model.position.x, y = model.position.y} }, Cmd.none)

outOfScreen : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
outOfScreen ( model , cmd) =
        if model.position.x > 800 then
            ({ model | position = { x = 0, y = model.position.y} }, Cmd.none)
        else if model.position.x < 0 then
            ({ model | position = { x = 800, y = model.position.y} }, Cmd.none)
        --else if model.position.y > 400 - 50 - (round ((21.0 * 400)/118)) then
        --    ({ model | position = { x = model.position.x, y = 400 - 50 - (round ((21.0 * 400)/118))} }, Cmd.none)
        else if model.position.y < 0 then
            ({ model | position = { x = model.position.x, y = 0} }, Cmd.none)
        else if model.position.y > 320 then
            ({ model | isDead = True }, Cmd.none)
        else ({ model | position = { x = model.position.x, y = model.position.y } }, Cmd.none)

pic : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
pic (model, cmd) =
    if model.ai1.pic > 5 then
        ({ model | ai1 = {pos = model.ai1.pos, isDead = model.ai1.isDead, pic = 0, direction = model.ai1.direction}}, Cmd.none)
    else ({ model | ai1 = {pos = model.ai1.pos, isDead = model.ai1.isDead, pic = model.ai1.pic + 1, direction = model.ai1.direction}}, Cmd.none)

pic2 : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
pic2 (model, cmd) =
    if model.ai2.pic > 5 then
        ({ model | ai2 = {pos = model.ai2.pos, isDead = model.ai2.isDead, pic = 0, direction = model.ai2.direction}}, Cmd.none)
    else ({ model | ai2 = {pos = model.ai2.pos, isDead = model.ai2.isDead, pic = model.ai2.pic + 1, direction = model.ai2.direction}}, Cmd.none)

backgroundColor : Attribute Msg
backgroundColor =
    fill "Black"

scale : Window.Size -> ( String, String )
scale size =
    let
        toPixelStr =
            \i -> round i |> toString

        ( fWidth, fHeight ) =
            ( toFloat size.width, toFloat size.height )

        ( scaledX, scaledY ) =
            if fWidth > fHeight then
                ( fHeight / fWidth, 1.0 )
            else
                ( 1.0, fWidth / fHeight )
    in
        ( toPixelStr (fWidth * scaledX), toPixelStr (fHeight * scaledY) )

winWidth : Window.Size -> Int
winWidth size = size.width

speedConversion : Game -> Int
speedConversion model = case model.momentumSpeedCounter of
    (-7) -> 3
    (-6) -> 6
    (-5) -> 9
    (-4) -> 9
    (-3) -> 9
    (-2) -> 6
    (-1) -> 3
    0 -> 0
    1 -> 3
    2 -> 6
    3 -> 9
    4 -> 9
    5 -> 9
    6 -> 6
    7 -> 3
    _ -> 0

view : Game -> Html.Html Msg
view model = let
        posX = toString (toFloat model.position.x * model.blockSize)
        posY = toString (toFloat model.position.y * model.blockSize)
        ai1X = toString (toFloat model.ai1.pos.x * model.blockSize)
        ai1Y = toString (toFloat model.ai1.pos.y * model.blockSize)
        ai2X = toString (toFloat model.ai2.pos.x * model.blockSize)
        ai2Y = toString (toFloat model.ai2.pos.y * model.blockSize)
        r1 = "./src/images/r1.png"
        r2 = "./src/images/r2.png"
        r3 = "./src/images/r3.png"
        r4 = "./src/images/r4.png"
        l1 = "./src/images/l1.png"
        l2 = "./src/images/l2.png"
        l3 = "./src/images/l3.png"
        l4 = "./src/images/l4.png"
        ai1Image =  if model.ai1.direction == Right then
                        case model.ai1.pic of
                            0 -> r1
                            1 -> r1
                            2 -> r2
                            3 -> r2
                            4 -> r3
                            5 -> r3
                            _ -> r4
                    else
                        case model.ai1.pic of
                            0 -> l1
                            1 -> l1
                            2 -> l2
                            3 -> l2
                            4 -> l3
                            5 -> l3
                            _ -> l4

        ai2Image =  if model.ai2.direction == Right then
                        case model.ai1.pic of
                            0 -> r1
                            1 -> r1
                            2 -> r2
                            3 -> r2
                            4 -> r3
                            5 -> r3
                            _ -> r4
                    else
                        case model.ai1.pic of
                            0 -> l1
                            1 -> l1
                            2 -> l2
                            3 -> l2
                            4 -> l3
                            5 -> l3
                            _ -> l4
        ( scaledWidth, scaledHeight ) = scale model.dimensions
        bs = model.blockSize
        pimage = if model.direction == Right then
                    if model.upMomentumCounter > 9 then "./src/images/pf1.0.png"
                    else "./src/images/pf2.0.png"
                else
                    if model.upMomentumCounter > 9 then "./src/images/pf1.1.png"
                    else "./src/images/pf2.1.png"

    --parentStyle =
    --      Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ) ]

    in
        if model.isDead == False then
            svg [width "1500",height "700"]
              --([ renderBackground model ]
              ([image [x "0", y "0", width (toString(800*bs)), height (toString(400*bs)), Svg.Attributes.xlinkHref "./src/images/background.png"][]]
              ++ [image [x ai1X, y ai1Y, width (toString(70*bs)), height (toString(40*bs)), Svg.Attributes.xlinkHref ai1Image][]]
              ++ [image [x ai2X, y ai2Y, width (toString(70*bs)), height (toString(40*bs)), Svg.Attributes.xlinkHref ai2Image][]]
              ++ [image [x posX, y posY, width (toString(50*bs)), height (toString(50*bs)), Svg.Attributes.xlinkHref pimage][]]
              )
        else
            Html.div []
            [ Html.button [ onClick StartGame ] [ text "Click here to play" ]
            , text "   Instructions: use keys A (left), D (Right), Space (Up) to land on top of enemies.   "
            , text "Your score is: "
            , text (toString(model.score))
            ]

renderBackground : Game -> Svg Msg
renderBackground model =
    rect [ x "0", y "0", width "100%", height "100%", backgroundColor ] []


subscriptions : Game -> Sub Msg
subscriptions model =
    let
        window = windowDimensionsChanged
        --keys = Keyboard.downs KeyMsg
        keysD = Keyboard.downs KeyDown
        keysU = Keyboard.ups KeyUp
        ticks = tick

    in
        Sub.batch [window, keysD, keysU, ticks]

initCmds : Cmd Msg
initCmds =
    Task.perform SizeUpdated Window.size

windowDimensionsChanged : Sub Msg
windowDimensionsChanged =
    Window.resizes SizeUpdated

tick : Sub Msg
tick =
    Time.every (33 * Time.millisecond) Tick

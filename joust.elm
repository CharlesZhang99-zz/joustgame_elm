module SvgAnimation exposing (..)

import Html as Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes exposing (style)
import Keyboard as Key
import Time exposing (Time)
import Window
import Task


{- Main -}
main : Program Never Game Msg
main = Html.program
       {init = (init,initCmds),
        update = update,
        view   = view,
        subscriptions = subscriptions }

type alias Game = {
        dimensions : Window.Size,
        position : Coords,
        ai1 : Ai1,
        direction : Direction,
        previousDirection: Direction,
        isDead : Bool,
        blockSize : Float,
        momentumSpeedCounter : Int,
        directionMomentumCounter : Int,
        upMomentumCounter : Int
                   }

type alias Block = {
        x : Int,
        y : Int
                    }
type alias Coords = { x : Int, y : Int }

type alias Ai1 = {
        pos : Coords
                  }
type Direction
    = Left
    | Right
    | NoDirection

type Msg
    = KeyMsg Key.KeyCode
    | SizeUpdated Window.Size
    | Tick Time
    | Nothing

init = ({ dimensions = Window.Size 0 0,
            position = {x = 100, y = 100},
            ai1 = {pos = {x = 0, y = 0}},
            direction = NoDirection,
            previousDirection = NoDirection,
            isDead = False,
            blockSize = 0,
            momentumSpeedCounter = 0,
            directionMomentumCounter = 0,
            upMomentumCounter = 0})

update : Msg -> Game -> (Game,Cmd.Cmd Msg)
update msg model = case msg of
        KeyMsg keyCode ->
            movePos keyCode model

        SizeUpdated dimensions ->
            ({ model | dimensions = dimensions }, Cmd.none )

        Nothing -> (model, Cmd.none)

        Tick time ->
            updateGame model


movePos : Int -> Game -> (Game, Cmd.Cmd Msg)
movePos keyCode model =
    case keyCode of
      87 -> ({ model | previousDirection = model.direction, upMomentumCounter = 12 }, Cmd.none)
      --83 -> ({ model | previousDirection = model.direction, direction = Down }, Cmd.none)
      65 -> ({ model | previousDirection = model.direction, direction = Left, momentumSpeedCounter = -4 }, Cmd.none)
      68 -> ({ model | previousDirection = model.direction, direction = Right, momentumSpeedCounter = 4 }, Cmd.none)
      --65 -> ({ model | position = { x = model.position.x - 20, y = model.position.y}, previousDirection = model.direction, direction = Left }, Cmd.none)
      --68 -> ({ model | position = { x = model.position.x + 20, y = model.position.y}, previousDirection = model.direction, direction = Right }, Cmd.none)
      _ -> (model, Cmd.none)

updateGame : Game -> ( Game, Cmd Msg )
updateGame model =
        ( model )
            |> blockSize
            |> ai1Pos
            |> collision
            |> momentum
            |> gravity
            |> upMomentum
            |> updateDirection
            |> outOfScreen

blockSize : Game -> ( Game, Cmd Msg )
blockSize model =
    ( { model | blockSize = (toFloat (winWidth model.dimensions))/800.0}, Cmd.none)


ai1Pos : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
ai1Pos (model, cmd) =
      let
          ab = True
      in
          if model.ai1.pos.x > 800 then
              ({ model | ai1 = { pos = { x = 0, y = model.ai1.pos.y}}}, Cmd.none)
          else
              ({ model | ai1 = { pos = { x = model.ai1.pos.x + 10, y = model.ai1.pos.y}}}, Cmd.none)


collision : ( Game , Cmd Msg ) -> ( Game, Cmd Msg )
collision ( model, cmd ) =
      --({ model | isDead = False }, Cmd.none)
--      let

      if ((model.position.y - model.ai1.pos.y) < 50) then
              ({ model | isDead = False }, cmd)
      else
              ({ model | isDead = False }, cmd)
--      in

--platform : ( Game, Cmd Msg ) ->

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
    in
        {--if model.direction == Up then
            ({ model | position = { x = model.position.x, y = model.position.y} }, Cmd.none)
        else if model.direction == Down then
            ({ model | position = { x = model.position.x, y = model.position.y} }, Cmd.none)
            --}
        if model.direction == Left then
            ({ model | position = { x = model.position.x - speed, y = model.position.y}, momentumSpeedCounter = newCounter}, Cmd.none)
        else if model.direction == Right then
            ({ model | position = { x = model.position.x + speed, y = model.position.y}, momentumSpeedCounter = newCounter}, Cmd.none)
        else ({ model | position = { x = model.position.x, y = model.position.y } }, Cmd.none)


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
{--
gravity : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
gravity ( model, cmd ) =
    if model.gravity == 0 then
        ({ model | position = { x = model.position.x, y = model.position.y + 10} }, Cmd.none)
    else if model.gravity == 1 then
        ({ model | position = { x = model.position.x, y = model.position.y + 20} }, Cmd.none)
    -}
gravity : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
gravity ( model, cmd ) =
    ({ model | position = { x = model.position.x, y = model.position.y + 3} }, Cmd.none)

outOfScreen : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
outOfScreen ( model , cmd) =
        if model.position.x > 800 then
            ({ model | position = { x = 0, y = model.position.y} }, Cmd.none)
        else if model.position.x < 0 then
            ({ model | position = { x = 800, y = model.position.y} }, Cmd.none)
        else if model.position.y > 400 then
            ({ model | position = { x = model.position.x, y = 0} }, Cmd.none)
        else if model.position.y < 0 then
            ({ model | position = { x = model.position.x, y = 0} }, Cmd.none)
        else ({ model | position = { x = model.position.x, y = model.position.y } }, Cmd.none)

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
speedConversion model =
    if model.momentumSpeedCounter == -7 then 3
    else if model.momentumSpeedCounter == -6 then 6
    else if model.momentumSpeedCounter == -5 then 9
    else if model.momentumSpeedCounter == -4 then 9
    else if model.momentumSpeedCounter == -3 then 9
    else if model.momentumSpeedCounter == -2 then 6
    else if model.momentumSpeedCounter == -1 then 3
    else if model.momentumSpeedCounter == 0 then 0
    else if model.momentumSpeedCounter == 1 then 3
    else if model.momentumSpeedCounter == 2 then 6
    else if model.momentumSpeedCounter == 3 then 9
    else if model.momentumSpeedCounter == 4 then 9
    else if model.momentumSpeedCounter == 5 then 9
    else if model.momentumSpeedCounter == 6 then 6
    else if model.momentumSpeedCounter == 7 then 3
    else 0


view : Game -> Html.Html Msg
view model = let
        posX = toString (toFloat model.position.x * model.blockSize)
        posY = toString (toFloat model.position.y * model.blockSize)
        ai1X = toString (toFloat model.ai1.pos.x * model.blockSize)
        ai1Y = toString (toFloat model.ai1.pos.y * model.blockSize)
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
            svg [width "100%",height "100%"]
              --([ renderBackground model ]
              ([image [x "0", y "0", width (toString(800*bs)), height (toString(400*bs)), Svg.Attributes.xlinkHref "./src/images/background.png"][]]
              ++ [image [x posX, y posY, width (toString(50*bs)), height (toString(50*bs)), Svg.Attributes.xlinkHref pimage][]]
              --++ [rect [x ai1X,y ai1Y, width "50", height "50", fill "blue"] []]
              )
        else
            svg [width "0%",height "0%"]
              (
              [rect [x "posX",y "posY", width "50", height "50", fill "red"] []])

{--
view : Game -> Html.Html Msg
view model = let
      posX = toFloat model.position.x * model.blockSize
      posY = toFloat model.position.y * model.blockSize
      ai1X = toFloat model.ai1.pos.x * model.blockSize
      ai1Y = toFloat model.ai1.pos.y * model.blockSize
      bs = model.blockSize
      ( scaledWidth, scaledHeight ) =
          scale model.dimensions

      --parentStyle =
    --      Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ) ]
    in
        if model.isDead == False then
            svg [width "100%",height "100%"]
              ([ renderBackground model ]
              ++ [rect [x (toString ((posX + 4)*bs)),y (toString ((posY - 15)*bs)), width (toString(13.0*bs)), height (toString(18.0*bs)), fill "#BEBC3C"] []]
              ++ [rect [x (toString (posX*bs)),y (toString (posY*bs)), width (toString(30.0*bs)), height (toString(30.0*bs)), fill "#70CCDB"] []]
              ++ [rect [x (toString ((posX - 12)*bs)),y (toString ((posY + 12)*bs)), width (toString(15.0*bs)), height (toString(15.0*bs)), fill "#70CCDB"] []]
              ++ [rect [x (toString ((posX + 15)*bs)),y (toString ((posY - 4)*bs)), width (toString(15.0*bs)), height (toString(15.0*bs)), fill "#70CCDB"] []]
              ++ [rect [x (toString ((posX + 2)*bs)),y (toString ((posY + 10)*bs)), width (toString(13.0*bs)), height (toString(13.0*bs)), fill "#924E1B"] []]
              ++ [rect [x (toString ((posX + 12)*bs)),y (toString ((posY + 16)*bs)), width (toString(15.0*bs)), height (toString(15.0*bs)), fill "#924E1B"] []]
              --++ [rect [x (toString ((posX + 25)*bs)),y (toString ((posY + 38)*bs)), width "25", height "25", fill "#924E1B"] []]
              ++ [rect [x (toString ((posX + 21)*bs)),y (toString ((posY + 25)*bs)), width (toString(13.0*bs)), height (toString(13.0*bs)), fill "#924E1B"] []]


              --++ [rect [x ai1X,y ai1Y, width "50", height "50", fill "blue"] []]
              )
        else
            svg [width "0%",height "0%"]
              (
              [rect [x "posX",y "posY", width "50", height "50", fill "red"] []])
-}
renderBackground : Game -> Svg Msg
renderBackground model =
    rect [ x "0", y "0", width "100%", height "100%", backgroundColor ] []

subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.batch [windowDimensionsChanged, Key.downs KeyMsg, tick]

initCmds : Cmd Msg
initCmds =
    Task.perform SizeUpdated Window.size

windowDimensionsChanged : Sub Msg
windowDimensionsChanged =
    Window.resizes SizeUpdated

tick : Sub Msg
tick =
    Time.every (33 * Time.millisecond) Tick

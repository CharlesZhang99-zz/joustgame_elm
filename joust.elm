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
        momentumCounter : Int
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
    = Up
    | Down
    | Left
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
            momentumCounter = 0})

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
      87 -> ({ model | previousDirection = model.direction, direction = Up }, Cmd.none)
      83 -> ({ model | previousDirection = model.direction, direction = Down }, Cmd.none)
      65 -> ({ model | previousDirection = model.direction, direction = Left }, Cmd.none)
      68 -> ({ model | previousDirection = model.direction, direction = Right }, Cmd.none)
      _ -> (model, Cmd.none)

updateGame : Game -> ( Game, Cmd Msg )
updateGame model =
        ( model )
            |> blockSize
            |> ai1Pos
            |> collision
            |> momentum
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

updateDirection : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
updateDirection ( model , cmd )= ({ model | previousDirection = model.direction, direction = model.direction }, Cmd.none)


momentum : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
momentum ( model , cmd ) =
    let
        reset =         if model.previousDirection == model.direction then model.momentumCounter
                        else 0
        moreSpeed =     --if reset /= 0 then
                        if ((model.momentumCounter > -3) && (model.momentumCounter < 3)) then 1
                        else 0
                        --else 0
        newCounter = reset + moreSpeed
        speed = speedChanges model
    in
        if model.direction == Up then
            ({ model | position = { x = model.position.x, y = model.position.y - 20} }, Cmd.none)
        else if model.direction == Down then
            ({ model | position = { x = model.position.x, y = model.position.y + 20} }, Cmd.none)
        else if model.direction == Left then
            ({ model | position = { x = model.position.x - speed, y = model.position.y}, momentumCounter = newCounter}, Cmd.none)
        else if model.direction == Right then
            ({ model | position = { x = model.position.x + speed, y = model.position.y}, momentumCounter = newCounter}, Cmd.none)
        else ({ model | position = { x = model.position.x, y = model.position.y } }, Cmd.none)

speedChanges : Game -> Int
speedChanges model =
    if model.momentumCounter == -3 then 30
    else if model.momentumCounter == -2 then 20
    else if model.momentumCounter == -1 then 10
    else if model.momentumCounter == 0 then 0
    else if model.momentumCounter == 1 then 10
    else if model.momentumCounter == 2 then 20
    else if model.momentumCounter == 3 then 30
    else 0

outOfScreen : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
outOfScreen ( model , cmd) =
        if model.position.x > 800 then
            ({ model | position = { x = 0, y = model.position.y} }, Cmd.none)
        else if model.position.y < 0 then
            ({ model | position = { x = model.position.x, y = 400} }, Cmd.none)
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

{-momentumConversion : Game -> Int
momentumConversion model =
    a = model.momentumCounter
    case a of
        -3 -> ("30")
        -2 -> ("15")
        -1 -> ("5")
        0 -> ("0")
        1 -> ("5")
        2 -> ("15")
        3 -> ("30")
    -}


view : Game -> Html.Html Msg
view model = let
      posX = toString (toFloat model.position.x * model.blockSize)
      posY = toString (toFloat model.position.y * model.blockSize)
      ai1X = toString (toFloat model.ai1.pos.x * model.blockSize)
      ai1Y = toString (toFloat model.ai1.pos.y * model.blockSize)
      ( scaledWidth, scaledHeight ) =
          scale model.dimensions

      parentStyle =
          Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ) ]
    in
        if model.isDead == False then
            svg [width "100%",height "100%"]
              ([ renderBackground model ]
              ++ [rect [x posX,y posY, width "50", height "50", fill "red"] []]
              ++ [rect [x ai1X,y ai1Y, width "50", height "50", fill "blue"] []])
        else
            svg [width "0%",height "0%"]
              (
              [rect [x "posX",y "posY", width "50", height "50", fill "red"] []])

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
    Time.every (100 * Time.millisecond) Tick

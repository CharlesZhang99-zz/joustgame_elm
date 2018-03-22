module SvgAnimation exposing (..)

import Html as Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard as Key
import Time exposing (Time)
import Window

{- Main -}
main : Program Never Game Msg
main = Html.program
       {init = (init,Cmd.none),
        update = update,
        view   = view,
        subscriptions = subscriptions }

type alias Game = {
        dimensions : Window.Size,
        position : Coords,
        ai1 : Ai1,
        direction : Direction,
        isDead : Bool
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


--type Msg = KeyMsg Key.KeyCode
type Msg
    = KeyMsg Key.KeyCode
    | SizeUpdate Window.Size
    | Tick Time
    | Nothing

init = ({ dimensions = Window.Size 0 0, position = {x = 300, y = 300}, ai1 = {pos = {x = 100, y = 100}}, direction = NoDirection, isDead = False})

update : Msg -> Game -> (Game,Cmd.Cmd Msg)
update msg model = case msg of
      (KeyMsg keyCode) ->
          movePos keyCode model

      Nothing -> (model, Cmd.none)

      SizeUpdate dimensions ->
          ( { model | dimensions = dimensions }, Cmd.none )

      Tick time ->
          updateGame model

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

movePos : Int -> Game -> (Game, Cmd.Cmd Msg)
movePos keyCode model =
    case keyCode of
      87 -> ({ model | position = { x = model.position.x, y = model.position.y - 20 } }, Cmd.none)
      83 -> ({ model | position = { x = model.position.x, y = model.position.y + 20 } }, Cmd.none)
      65 -> ({ model | position = { x = model.position.x - 20, y = model.position.y }, direction = Left }, Cmd.none)
      68 -> ({ model | position = { x = model.position.x + 20, y = model.position.y }, direction = Right }, Cmd.none)
      _ -> (model, Cmd.none)

updateGame : Game -> ( Game, Cmd Msg )
updateGame model =
      ( model )
          |> ai1Pos
          |> collision
          |> momentum

ai1Pos : Game -> ( Game, Cmd Msg )
ai1Pos model =
      let
          ab = True
      in
          if model.ai1.pos.x > 1000 then
              ({ model | ai1 = { pos = { x = 0, y = model.ai1.pos.y}}}, Cmd.none)
          else
              ({ model | ai1 = { pos = { x = model.ai1.pos.x + 10, y = model.ai1.pos.y}}}, Cmd.none)


collision : ( Game , Cmd Msg ) -> ( Game, Cmd Msg )
collision ( model, cmd ) =
      --({ model | isDead = False }, Cmd.none)
--      let

      if ((model.position.y - model.ai1.pos.y) < 50) then
              ({ model | isDead = True }, cmd)
      else
              ({ model | isDead = False }, cmd)
--      in

momentum : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
momentum ( model , cmd ) =

        if model.direction == Left then
            ({ model | position = { x = model.position.x - 20, y = model.position.y} }, Cmd.none)
        else if model.direction == Right then
            ({ model | position = { x = model.position.x + 20, y = model.position.y} }, Cmd.none)
        else ({ model | position = { x = model.position.x, y = model.position.y } }, Cmd.none)


view : Game -> Html.Html Msg
view model = let
      posX = toString model.position.x
      posY = toString model.position.y
      ai1X = toString model.ai1.pos.x
      ai1Y = toString model.ai1.pos.y
    in
        if model.isDead == False then
            svg [width "100%",height "100%"]
              (
              [rect [x posX,y posY, width "50", height "50", fill "red"] []]
              ++ [rect [x ai1X,y ai1Y, width "50", height "50", fill "blue"] []])
        else
            svg [width "0%",height "0%"]
              (
              [rect [x posX,y posY, width "50", height "50", fill "red"] []])

subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.batch [windowDimensionsChanged, Key.downs KeyMsg, tick]

windowDimensionsChanged : Sub Msg
windowDimensionsChanged =
    Window.resizes SizeUpdate

tick : Sub Msg
tick =
    Time.every (100 * Time.millisecond) Tick

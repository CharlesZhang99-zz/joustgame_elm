module SvgAnimation exposing (..)

import Html as Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard as Key
import Time exposing (Time)

{- Main -}
main : Program Never Game Msg
main = Html.program
       {init = (init,Cmd.none),
        update = update,
        view   = view,
        subscriptions = subscriptions }

type alias Game = {
        position : Coords,
        ai1 : Ai1,
        direction : Direction,
        isDead : Bool
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
    | Tick Time
    | Nothing

init = ({ position = {x = 300, y = 300}, ai1 = {pos = {x = 100, y = 100}}, direction = NoDirection, isDead = False})

update : Msg -> Game -> (Game,Cmd.Cmd Msg)
update msg model = case msg of
      (KeyMsg keyCode) ->
          movePos keyCode model

      Nothing -> (model, Cmd.none)

      Tick time ->
          updateGame model

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

ai1Pos : Game -> ( Game, Cmd Msg )
ai1Pos model =
      let
          ab = True
      in
          if model.ai1.pos.x > 1000 then
              ({ model | ai1 = { pos = { x = 0, y = model.ai1.pos.y}}}, Cmd.none)
          else
              ({ model | ai1 = { pos = { x = model.ai1.pos.x + 10, y = model.ai1.pos.y}}}, Cmd.none)


collision : Game -> ( Game, Cmd Msg )
collision model =
      --({ model | isDead = False }, Cmd.none)
--      let

      if ((model.position.y - model.ai1.pos.y) < 50) then
              ({ model | isDead = True }, Cmd.none)
      else
              ({ model | isDead = False }, Cmd.none)
--      in



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
    Sub.batch [Key.downs KeyMsg, tick]

tick : Sub Msg
tick =
    Time.every (100 * Time.millisecond) Tick

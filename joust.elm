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
        position : Movers,
        ai1 : Movers

                   }
type alias Movers = { x : Int, y : Int }

--type Msg = KeyMsg Key.KeyCode
type Msg
    = KeyMsg Key.KeyCode
    | Tick Time
    | Nothing

init = ({ position = {x = 300, y = 300}, ai1 = {x = 100, y = 100}})

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
      65 -> ({ model | position = { x = model.position.x - 20, y = model.position.y } }, Cmd.none)
      68 -> ({ model | position = { x = model.position.x + 20, y = model.position.y } }, Cmd.none)
      _ -> (model, Cmd.none)

updateGame : Game -> ( Game, Cmd Msg )
updateGame model =

      ai1Pos model
ai1Pos : Game -> ( Game, Cmd Msg )
ai1Pos model =
      if model.ai1.x < 400 then
        ({ model | ai1 = { x = model.ai1.x + 10, y = model.ai1.y}, position = { x = model.ai1.x, y = model.ai1.y + 10}}, Cmd.none)
      else
        ({ model | ai1 = { x = model.ai1.x - 10, y = model.ai1.y}}, Cmd.none)

view : Game -> Html.Html Msg
view model = let
      posX = toString model.position.x
      posY = toString model.position.y
      ai1X = toString model.ai1.x
      ai1Y = toString model.ai1.y
    in svg [width "100%",height "100%"]
    (
       [rect [x posX,y posY, width "50", height "50", fill "red"] []]
       ++ [rect [x ai1X,y ai1Y, width "50", height "50", fill "blue"] []])

subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.batch [Key.downs KeyMsg, tick]

tick : Sub Msg
tick =
    Time.every (100 * Time.millisecond) Tick

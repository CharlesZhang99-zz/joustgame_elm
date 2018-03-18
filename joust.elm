module SvgAnimation exposing (..)

import Html as Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard as Key


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
    | Nothing

init = ({ position = {x = 300, y = 300}, ai1 = {x = 100, y = 100}})

update : Msg -> Game -> (Game,Cmd.Cmd Msg)
update msg model = case msg of
      (KeyMsg keyCode) ->
        movePos keyCode model

      Nothing -> (model, Cmd.none)

movePos : Int -> Game -> (Game, Cmd.Cmd Msg)
movePos keyCode model =
    case keyCode of
      87 -> ({ model | position = { x = model.position.x, y = model.position.y - 20 } }, Cmd.none)
      83 -> ({ model | position = { x = model.position.x, y = model.position.y + 20 } }, Cmd.none)
      65 -> ({ model | position = { x = model.position.x - 20, y = model.position.y } }, Cmd.none)
      68 -> ({ model | position = { x = model.position.x + 20, y = model.position.y } }, Cmd.none)
      _ -> (model, Cmd.none)



--updatePos : Game -> (Game, Cmd.Cmd)




view : Game -> Html.Html Msg
view model = let
      posX = toString model.position.x
      posY = toString model.position.y
    in svg [width "100%",height "100%"]
    (
       [rect [x posX,y posY, width "50", height "50", fill "red"] []]
       ++ [rect [x posX,y posY, width "50", height "50", fill "red"] []])

subscriptions : Game -> Sub Msg
subscriptions model = Key.downs KeyMsg

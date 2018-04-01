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
        ai1                         : Ai1,
        direction                   : Direction,
        previousDirection           : Direction,
        isDead                      : Bool,
        blockSize                   : Float,
        momentumSpeedCounter        : Int,
        directionMomentumCounter    : Int,
        upMomentumCounter           : Int,
        keysDown                    : Set KeyCode
                   }

type alias Coords = { x : Int, y : Int }

type alias Ai1 = {
        pos : Coords
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
    = KeyDown KeyCode
    | KeyUp KeyCode
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
            upMomentumCounter = 0,
            keysDown = Set.empty})

update : Msg -> Game -> (Game,Cmd.Cmd Msg)
update msg model = case msg of
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
            |> collision
            |> momentum
            |> gravity
            |> upMomentum
            |> updateDirection
            |> basePlatform
            |> leftPlatform
            |> middlePlatform
            |> rightPlatform
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
        {--
basePlatform : ( Game , Cmd Msg ) -> ( Game , Cmd Msg )
basePlatform ( model , cmd) =
    --&& ((model.position.x >  (round ((39.0 * 400)/118))) && model.position.x < (round (((400.0-48) * 400)/118)))
    --(model.position.x > (round ((39.0 * 800)/236))) &&
    if (model.position.y > (400 - 50 - (round ((21.0 * 400)/118)))) && (model.position.y < (400 - 50 - (round ((15.0 * 400)/118))))
        && (model.position.x + 25 > (round ((39.0 * 800)/236))) && (model.position.x + 25 < (round (800.0 - ((48.0 * 800))/236)))
        --&& (model.position.y < round ((450*400)/118))
        then ({ model | position = { x = model.position.x, y = 400 - 50 - (round ((21.0 * 400)/118))} }, Cmd.none)
    else ({ model | position = { x = model.position.x, y = model.position.y} }, Cmd.none)

-}
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
    {--
    if (model.position.x + 25 > 0) && ((model.position.x + 25) < (round ((49.0 * 800)/236)))
        then ({ model | position = { x = model.position.x, y = (round (((24.0 - 8) * 400)/118))} }, Cmd.none)
    else ({ model | position = { x = model.position.x, y = model.position.y} }, Cmd.none)
-}

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
    {--
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
    -}

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
{--
subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.batch [windowDimensionsChanged, Key.downs KeyMsg, tick]
-}

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
{-
subscriptions : Model -> Sub Msg
subscriptions {ui} =
  let
      window = Window.resizes (\{width,height} -> ResizeWindow (width,height))
      keys = [ Keyboard.downs (KeyChange True)
             , Keyboard.ups (KeyChange False)
             ]
      animation = [ AnimationFrame.diffs Tick ]
      seconds = Time.every Time.second TimeSecond
  in
     (
     case ui.screen of
       StartScreen ->
         [ window, seconds ]

       PlayScreen ->
         [ window ] ++ keys ++ animation

       GameoverScreen ->
         [ window ] ++ keys

     ) |> Sub.batch

update : Msg -> Model -> (Model, Cmd Msg)
update action ({ui,scene} as model) =
  case action of
    ResizeWindow dimensions ->
      ({ model | ui = { ui | windowSize = dimensions } }, Cmd.none)

    Tick delta ->
      let
          player1 = scene.player1 |> steerAndGravity delta ui
          player2 = scene.player2 |> steerAndGravity delta ui
          round = scene.round
          (player1_, player2_) = handleCollisions player1 player2
          player1__ = player1_ |> movePlayer delta
          player2__ = player2_ |> movePlayer delta
          hasAnyPlayerFallen = hasFallen player1 || hasFallen player2
          isRoundOver = hasAnyPlayerFallen && round.touchdownTime > 1400
          (player1___, player2___) = applyScores player1__ player2__ isRoundOver
          isGameOver = player1___.score>=winScore || player2___.score>=winScore
          (round_, screen_) =
            if isGameOver then
               (round, GameoverScreen)
            else if isRoundOver then
               (newRound, PlayScreen)
            else if hasAnyPlayerFallen then
              ({ round | touchdownTime = round.touchdownTime + delta }, PlayScreen)
            else
              (round, PlayScreen)
          scene_ = { scene | player1 = player1___
                           , player2 = player2___
                           , round = round_ }
          ui_ = { ui | screen = screen_ }
      in
          ({ model | scene = scene_, ui = ui_ }, Cmd.none)

    KeyChange pressed keycode ->
      (handleKeyChange pressed keycode model, Cmd.none)

    StartGame ->
      (freshGame ui, Cmd.none)

    TimeSecond _ ->
      ({ model | secondsPassed = model.secondsPassed+1 }, Cmd.none)

    NoOp ->
      (model, Cmd.none)

-}
initCmds : Cmd Msg
initCmds =
    Task.perform SizeUpdated Window.size

windowDimensionsChanged : Sub Msg
windowDimensionsChanged =
    Window.resizes SizeUpdated

tick : Sub Msg
tick =
    Time.every (33 * Time.millisecond) Tick

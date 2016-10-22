import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Collage exposing (Form, groupTransform, polygon, collage, filled, rect, circle, move, rotate)
import Transform exposing (Transform)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import Window
import String
import Mouse
import Debug exposing (log)
import Pythagoras exposing (buildTree)
import Math exposing (Point, calculateDistance)

main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

type Draggable = Anchor | Edge Int

type alias Model =
  { width : Int, height : Int
  , mouseX : Int, mouseY : Int
  , ptree : Pythagoras.Model
  , draggables : List (Point, Draggable)
  , currentDraggable : Maybe Draggable
  , isDragging : Bool
  }

updateDraggables : Pythagoras.Model -> List (Point, Draggable)
updateDraggables ptree =
  [(ptree.point, Anchor)]
  --++ List.map (\p -> (p, Edge 0)) ptree.points

init : (Model, Cmd Msg)
init =
  let
    factor = 20
    ptree = Pythagoras.init
    model =
    { width = 500, height = 500
    , mouseX = 0, mouseY = 0
    , ptree = ptree
    , draggables = updateDraggables ptree
    , currentDraggable = Nothing
    , isDragging = False
    }
  in (model, Cmd.none)

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes (\{height, width} -> Resize width height)
    , Mouse.moves (\{x, y} -> MouseMove x y)
    , Mouse.downs (\{x, y} -> MouseDown x y)
    , Mouse.ups (\{x, y} -> MouseUp x y)
    ]

type Msg
  = Update
  | Resize Int Int
  | MouseMove Int Int
  | MouseDown Int Int
  | MouseUp Int Int

isClose : Point -> Point -> Bool
isClose p1 p2 =
  if calculateDistance p1 p2 < 10
    then True
    else False

findHovered : Point -> List (Point, Draggable) -> Maybe Draggable
findHovered mouse draggables =
  case draggables of
    [] ->
      Nothing
    (point, draggable) :: rest ->
      if isClose mouse point
        then Just draggable
        else findHovered mouse rest

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Update -> (model, Cmd.none)
    Resize width height ->
      ({model | width = width, height = height}, Cmd.none)
    MouseMove x y ->
      let
        x' = screenCoordsToCollage x model.width
        y' = screenCoordsToCollage y model.height

        p = (x', -y')

        draggable = case model.isDragging of
          False -> findHovered p model.draggables
          True -> model.currentDraggable

        model' = if model.isDragging
          then
            case model.currentDraggable of
              Nothing -> model
              Just draggable -> case draggable of
                Anchor ->
                  let
                    ptree = Pythagoras.updatePoint p model.ptree
                    draggables = updateDraggables ptree
                  in
                    {model | draggables = draggables, ptree = ptree}
                Edge idx -> model
          else
            model
      in
        ({model' | mouseX = x, mouseY = y, currentDraggable = draggable}, Cmd.none)
    MouseDown x y ->
      ({model | isDragging = True}, Cmd.none)
    MouseUp x y ->
      ({model | isDragging = False}, Cmd.none)

drawRectangle : Color -> Int -> Int -> Form
drawRectangle color width height =
  filled color (rect (toFloat (width)) (toFloat (height)))

drawBackground : Model -> Int -> Form
drawBackground {width, height} padding =
  drawRectangle (rgb 200 200 200) (width-padding) (height-padding)

screenCoordsToCollage : Int -> Int -> Float
screenCoordsToCollage screenCoord screenSize =
  (toFloat screenCoord) - ((toFloat screenSize) / 2)

view : Model -> Html Msg
view model =
  let
    {width, height, mouseX, mouseY, ptree} = model

    pt = buildTree 7 ptree

    forms =
      [drawBackground model 0]
      ++ pt
  in
    collage width height forms |> toHtml

import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Collage exposing (Form, groupTransform, polygon, collage, filled, rect,
  circle, move, rotate)
import Text exposing (fromString)
import Transform exposing (Transform)
import Color exposing (Color, rgb, rgba, complement)
import Element exposing (toHtml)
import Window
import String
import Mouse
import Debug exposing (log)
import Pythagoras exposing (buildTree)
import Math exposing (Point, calculateDistance, calculateClosestPoint)
import Task

main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

type DraggableType = Anchor | Edge Int

type alias Draggable = {point : Point, draggable : DraggableType}

type alias Insertable = {point : Point, n : Int}

type alias Model =
  { width : Int, height : Int
  , mouseX : Float, mouseY : Float
  , backgroundColor : Color
  , startColor : Color
  , ptree : Pythagoras.Model
  , draggables : List Draggable
  , currentDraggable : Maybe Draggable
  , insertable : Maybe Insertable
  , isDragging : Bool
  }

updateDraggables : Pythagoras.Model -> List Draggable
updateDraggables ptree =
  [{point = ptree.point, draggable = Anchor}]
  ++ List.indexedMap (\n p -> {point = p, draggable = Edge n}) ptree.points

init : (Model, Cmd Msg)
init =
  let
    ptree = Pythagoras.init
    model =
    { width = 500, height = 500
    , mouseX = 0, mouseY = 0
    , backgroundColor = rgb 30 30 30
    , startColor = rgb 255 0 0
    , ptree = ptree
    , draggables = updateDraggables ptree
    , currentDraggable = Nothing
    , insertable = Nothing
    , isDragging = False
    }
  in (model, initialSizeCmd)

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes (\{height, width} -> Resize width height)
    , Mouse.moves (\{x, y} -> MouseMove x y)
    , Mouse.downs (\{x, y} -> MouseDown x y)
    , Mouse.ups (\{x, y} -> MouseUp x y)
    ]

initialSizeCmd : Cmd Msg
initialSizeCmd =
  let
    failure = \_ -> Resize 500 500
    success = \size -> Resize size.width size.height
  in Task.perform failure success Window.size

type Msg
  = Update
  | Resize Int Int
  | MouseMove Int Int
  | MouseDown Int Int
  | MouseUp Int Int

maxDistance = 20
dotSize = 6

isClose : Point -> Point -> Bool
isClose p1 p2 = calculateDistance p1 p2 < maxDistance

findHovered : Point -> List Draggable -> Maybe Draggable
findHovered mouse draggables =
  case draggables of
    [] ->
      Nothing
    draggable :: rest ->
      if isClose mouse draggable.point
        then Just draggable
        else findHovered mouse rest

findInsertable : Model -> Maybe Insertable
findInsertable model =
  let
    l1 = model.ptree.points
    l2 = (List.drop 1 model.ptree.points) ++ (List.take 1 model.ptree.points)
    calculatePointAndDistance mouse p1 p2 =
      let
        closestPoint = calculateClosestPoint (p1, p2) mouse
        distance = calculateDistance closestPoint mouse
      in {point = closestPoint, distance = distance, n = 0}
    insertable = List.map2 (calculatePointAndDistance (model.mouseX, model.mouseY)) l1 l2
    |> List.indexedMap (\n item -> {item | n = n})
    |> List.filter (\{distance} -> distance < maxDistance)
    |> List.sortBy .distance
    |> List.head
  in
    case insertable of
      Nothing -> Nothing
      Just {point, n} -> Just {point = point, n = n}


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

        insertable = findInsertable model

        model' = if model.isDragging
          then
            case model.currentDraggable of
              Nothing -> model
              Just {draggable} ->
                let
                  ptree = case draggable of
                    Anchor -> Pythagoras.updatePoint p model.ptree
                    Edge n -> Pythagoras.updatePoints n p model.ptree
                  draggables = updateDraggables ptree
                in
                  {model | draggables = draggables, ptree = ptree}
          else
            model
      in
        ({model' | mouseX = x', mouseY = -y',
          currentDraggable = draggable, insertable = insertable}, Cmd.none)
    MouseDown x y ->
      let
        model = case model.currentDraggable of
          Nothing ->
            let
              ptree = case model.insertable of
                Nothing -> model.ptree
                Just {point, n} -> Pythagoras.insertPoint n point model.ptree
              draggables = updateDraggables ptree
              draggable = findHovered (model.mouseX, model.mouseY) draggables
            in
              {model | isDragging = True, ptree = ptree, draggables = draggables,
                currentDraggable = draggable}
          _ -> model
      in ({model | isDragging = True}, Cmd.none)
    MouseUp x y ->
      let draggables = updateDraggables model.ptree
      in ({model | isDragging = False, draggables = draggables,
        currentDraggable = Nothing}, Cmd.none)

drawRectangle : Color -> Int -> Int -> Form
drawRectangle color width height =
  filled color (rect (toFloat (width)) (toFloat (height)))

drawBackground : Model -> Form
drawBackground {width, height, backgroundColor} =
  drawRectangle (rgb 30 30 30) (width) (height)

screenCoordsToCollage : Int -> Int -> Float
screenCoordsToCollage screenCoord screenSize =
  (toFloat screenCoord) - ((toFloat screenSize) / 2)

drawPoint : Color -> Point -> Form
drawPoint color point =
  move point (filled color (circle dotSize))

drawDraggable : Model -> List Form
drawDraggable model =
  if model.isDragging
    then []
    else
      case model.currentDraggable of
        Nothing -> []
        Just {point, draggable} ->
          [drawPoint (rgba 255 255 255 0.6) point]

drawInsertable : Model -> List Form
drawInsertable model =
  case model.currentDraggable of
    Nothing ->
      case model.insertable of
        Nothing -> []
        Just {point, n} -> [drawPoint (rgba 255 255 255 0.6) point]
    _ -> []

drawHint : Model -> List Form
drawHint model =
  let
    form =
      "Hint: you can change the base shape by moving and adding edges"
      |> fromString
      |> Text.color (rgba 255 255 255 0.5)
      |> Text.height 13
      |> Collage.text
      |> move (0, toFloat model.height / 2 - 15)
  in [form]

view : Model -> Html Msg
view model =
  [drawBackground model]
  ++ buildTree 9 model.ptree model.startColor
  ++ drawDraggable model
  ++ drawInsertable model
  ++ drawHint model
  |> collage model.width model.height
  |> toHtml

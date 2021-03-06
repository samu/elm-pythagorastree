module Pythagoras
    exposing
        ( Model
        , buildTree
        , init
        , updatePoint
        , updatePoints
        , insertPoint
        , removePoint
        , getOnlyEdgePoints
        )

import Transform exposing (Transform)
import Collage exposing (Form, Shape, polygon, groupTransform, filled)
import Color exposing (Color, rgb)
import Math exposing (Point, calculateAngle, calculateDistance)
import Array exposing (Array, fromList, get)
import Color.Mixing exposing (lighten, darken)


type alias Model =
    { points : List Point
    , point : Point
    , e0 : Int
    , e1 : Int
    , e3 : Int
    , e2 : Int
    }


rectangle : Float -> List ( Float, Float )
rectangle factor =
    [ ( -0.8, 1 ), ( 0.9, 0.85 ), ( 1.1, -1.5 ), ( -1, -1 ) ]
        |> List.map (\( x, y ) -> ( x * factor, y * factor ))


init : Model
init =
    let
        factor =
            100

        translationY =
            -100

        rect =
            rectangle factor
                |> List.map (\( x, y ) -> ( x, y + translationY ))
    in
        { points = rect
        , point = ( factor * 0, factor * 1.9 + translationY )
        , e0 = 0
        , e1 = 1
        , e3 = 3
        , e2 = 2
        }


updatePoint : Point -> Model -> Model
updatePoint point model =
    { model | point = point }


updatePoints : Int -> Point -> Model -> Model
updatePoints n point model =
    let
        f =
            \n_ point_ ->
                if n == n_ then
                    point
                else
                    point_
    in
        { model | points = List.indexedMap f model.points }


updateEdgeIndices : Int -> (Int -> Int -> Int) -> Model -> ( Int, Int, Int )
updateEdgeIndices n op model =
    let
        bump index =
            if index >= n then
                1
            else
                0

        e1 =
            op model.e1 (bump model.e1)

        e2 =
            op model.e2 (bump model.e2)

        e3 =
            op model.e3 (bump model.e3)
    in
        ( e1, e2, e3 )


insertPoint : Int -> Point -> Model -> Model
insertPoint n point model =
    let
        n_ =
            n + 1

        points =
            List.take n_ model.points ++ [ point ] ++ List.drop n_ model.points

        ( e1, e2, e3 ) =
            updateEdgeIndices n_ (+) model
    in
        { model | points = points, e1 = e1, e2 = e2, e3 = e3 }


removePoint : Int -> Model -> Model
removePoint n model =
    let
        points =
            List.take n model.points ++ List.drop (n + 1) model.points

        ( e1, e2, e3 ) =
            updateEdgeIndices n (-) model
    in
        { model | points = points, e1 = e1, e2 = e2, e3 = e3 }


getPoint : List Point -> Int -> Point
getPoint points n =
    case fromList points |> get n of
        Just a ->
            a

        Nothing ->
            ( 0, 0 )


getOnlyEdgePoints : Model -> List Point
getOnlyEdgePoints model =
    let
        p0 =
            getPoint model.points model.e0

        p1 =
            getPoint model.points model.e1

        p2 =
            getPoint model.points model.e2

        p3 =
            getPoint model.points model.e3
    in
        [ p0, p1, p2, p3 ]


buildMatrix : Point -> Point -> Float -> Float -> Transform
buildMatrix p1 p2 angle ratio =
    let
        ( p1x, p1y ) =
            p1

        ( p2x, p2y ) =
            p2

        m1 =
            Transform.translation -p2x -p2y

        m2 =
            Transform.rotation angle

        m3 =
            Transform.scale ratio

        m4 =
            Transform.translation p2x p2y

        m5 =
            Transform.translation (p1x - p2x) (p1y - p2y)
    in
        List.foldl Transform.multiply Transform.identity [ m1, m2, m3, m4, m5 ]


buildMatrices : Model -> ( Transform, Transform )
buildMatrices model =
    let
        p0 =
            getPoint model.points model.e0

        p1 =
            getPoint model.points model.e1

        p2 =
            getPoint model.points model.e2

        p3 =
            getPoint model.points model.e3

        bottomLength =
            calculateDistance p2 p3

        leftLength =
            calculateDistance p0 model.point

        rightLength =
            calculateDistance p1 model.point

        leftRatio =
            leftLength / bottomLength

        rightRatio =
            rightLength / bottomLength

        r =
            90 * (pi / 180)

        bottomAngle =
            r - calculateAngle p3 p2 p1

        topAngle =
            r - calculateAngle p2 p1 p0

        leftAngle =
            calculateAngle model.point p0 p1 + topAngle + bottomAngle

        rightAngle =
            -(calculateAngle model.point p1 p0) + topAngle + bottomAngle

        rightMatrix =
            buildMatrix p1 p2 rightAngle rightRatio

        leftMatrix =
            buildMatrix p0 p3 leftAngle leftRatio
    in
        ( leftMatrix, rightMatrix )


type alias Config =
    { n : Int
    , polygon : Shape
    , color : Color
    , transformationMatrices : ( Transform, Transform )
    , previousMatrix : Transform
    }


buildTree_ : Config -> List Form
buildTree_ { n, polygon, color, transformationMatrices, previousMatrix } =
    let
        newColor =
            lighten 0.05 color

        form =
            filled newColor polygon

        ( left, right ) =
            transformationMatrices

        newMatrixLeft =
            Transform.multiply previousMatrix left

        newMatrixRight =
            Transform.multiply previousMatrix right

        formRight =
            groupTransform newMatrixRight [ form ]

        formLeft =
            groupTransform newMatrixLeft [ form ]
    in
        if n > 1 then
            let
                rightConfig =
                    { n = n - 1
                    , polygon = polygon
                    , color = newColor
                    , transformationMatrices = ( left, right )
                    , previousMatrix = newMatrixRight
                    }

                leftConfig =
                    { rightConfig | previousMatrix = newMatrixLeft }
            in
                []
                    ++ [ formRight ]
                    ++ buildTree_ rightConfig
                    ++ [ formLeft ]
                    ++ buildTree_ leftConfig
        else
            [ formLeft, formRight ]


buildTree : Int -> Model -> Color -> List Form
buildTree n model startColor =
    let
        form =
            filled startColor (polygon model.points)

        config =
            { n = n
            , polygon = polygon model.points
            , color = startColor
            , transformationMatrices = (buildMatrices model)
            , previousMatrix = Transform.identity
            }
    in
        [ form ] ++ buildTree_ config
